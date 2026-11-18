#!/usr/bin/env python3
"""
gitshuffle.py - keep local git repos backed up on the NAS and free local space.

Layout
  local : /home/kranium/git/...            (any depth, any layout)
  nas   : /armorydata/git/<host>/<owner>/<repo>

How it works
  sync     For every local repo, find its NAS twin (from the origin URL) and have
           the NAS repo FETCH from the local one. Local branches, tags and stashes
           land on the NAS under  refs/remotes/<machine>/*  and  refs/<machine>-stash,
           so nothing on the NAS (its own branches / checked-out tree) is touched.
           If the NAS twin does not exist yet, a bare repo is created for it.
  status   Table of every local repo: size, last used, synced?, ready to archive?
  archive  Move repos to the NAS: back up every ref, verify, delete the laptop
           copy. Name repos, or use --idle DAYS. Dry run unless you pass --yes.
  recall   Bring an archived repo back to where it was, with all its branches.

Only uses the Python standard library + the git CLI.
"""

import argparse
import os
import re
import shutil
import socket
import subprocess
import sys
import time
from pathlib import Path

# ---------------------------------------------------------------- config ----
LOCAL_ROOT = Path("/home/kranium/git")
NAS_ROOT = Path("/armorydata/git")
MACHINE = socket.gethostname().split(".")[0]   # tag for refs pushed to the NAS
NO_REMOTE_HOST = "local"                       # NAS folder for repos with no origin
MAX_DEPTH = 12                                 # how deep to look for repos (nested groups)
# ----------------------------------------------------------------------------


def git(repo, *args, check=True):
    r = subprocess.run(["git", "-C", str(repo), *args],
                       capture_output=True, text=True)
    if check and r.returncode != 0:
        raise RuntimeError(f"git {' '.join(args)} in {repo}:\n{r.stderr.strip()}")
    return r.stdout.strip()


def find_local_repos(root=None, max_depth=MAX_DEPTH):
    repos = []
    root = (root or LOCAL_ROOT).resolve()
    for dirpath, dirnames, _ in os.walk(root):
        p = Path(dirpath)
        depth = len(p.relative_to(root).parts)
        if (p / ".git").exists():
            repos.append(p)
            dirnames[:] = []           # don't descend into a repo
            continue
        if depth >= max_depth:
            dirnames[:] = []
        dirnames[:] = [d for d in dirnames if not d.startswith(".")]
    return sorted(repos)


URL_RE = re.compile(
    r"^(?:[a-z+]+://)?(?:[^@/]+@)?(?P<host>[^:/]+)(?::\d+)?[:/](?P<path>.+?)(?:\.git)?/?$")


def nas_path_for(repo):
    """Map a local repo to /armorydata/git/<host>/<owner>/<repo>."""
    url = git(repo, "config", "--get", "remote.origin.url", check=False)
    if url:
        m = URL_RE.match(url)
        if m:
            return NAS_ROOT / m["host"].lower() / m["path"]
    # no usable origin: keep the same relative layout under NAS_ROOT/local/
    return NAS_ROOT / NO_REMOTE_HOST / repo.relative_to(LOCAL_ROOT)


def local_refs(repo):
    """{refname: sha} for branches, tags and the stash."""
    out = git(repo, "for-each-ref", "--format=%(refname) %(objectname)",
              "refs/heads", "refs/tags", "refs/stash")
    return dict(line.split(" ", 1) for line in out.splitlines() if line)


def ensure_nas_repo(nas):
    if (nas / ".git").exists() or (nas / "HEAD").exists():
        return False
    nas.mkdir(parents=True, exist_ok=True)
    git(nas, "init", "--bare", "-q")
    return True


def stash_shas(repo):
    """All stash entries, newest first."""
    return git(repo, "reflog", "--format=%H", "refs/stash", check=False).splitlines()


def sync_repo(repo, nas):
    """Copy every local branch, tag and stash into the NAS repo, namespaced per machine.

    NAS layout (none of it touches the NAS repo's own branches/tags/working tree):
      refs/remotes/<machine>/<branch>
      refs/<machine>/tags/<tag>
      refs/<machine>/stash/<n>      (0 = newest)
    """
    created = ensure_nas_repo(nas)
    git(nas, "fetch", "--quiet", "--no-tags", "--no-write-fetch-head", str(repo),
        f"+refs/heads/*:refs/remotes/{MACHINE}/*",
        f"+refs/tags/*:refs/{MACHINE}/tags/*")
    # drop old stash copies, then store the current ones
    for ref in git(nas, "for-each-ref", "--format=%(refname)", f"refs/{MACHINE}/stash/").splitlines():
        git(nas, "update-ref", "-d", ref)
    shas = stash_shas(repo)
    if shas:
        # make each stash entry reachable on the laptop side so it can be fetched
        for i, sha in enumerate(shas):
            git(repo, "update-ref", f"refs/gitshuffle-stash/{i}", sha)
        try:
            git(nas, "fetch", "--quiet", "--no-tags", "--no-write-fetch-head", str(repo),
                f"+refs/gitshuffle-stash/*:refs/{MACHINE}/stash/*")
        finally:
            for i in range(len(shas)):
                git(repo, "update-ref", "-d", f"refs/gitshuffle-stash/{i}", check=False)
    return created


def missing_on_nas(repo, nas):
    """List local refs whose commits are not present in the NAS repo."""
    if not nas.exists():
        return list(local_refs(repo))
    missing = []
    for ref, sha in local_refs(repo).items():
        r = subprocess.run(["git", "-C", str(nas), "cat-file", "-e", f"{sha}^{{commit}}"],
                           capture_output=True)
        if r.returncode != 0:
            missing.append(ref)
    # every stash entry (not just the newest) must be on the NAS too
    for sha in stash_shas(repo):
        r = subprocess.run(["git", "-C", str(nas), "cat-file", "-e", sha], capture_output=True)
        if r.returncode != 0:
            missing.append(f"stash@{sha[:8]}")
    return missing


def blockers(repo):
    """Reasons a repo must NOT be deleted locally even if refs are backed up."""
    why = []
    dirty = git(repo, "status", "--porcelain", "--untracked-files=all", check=False)
    if dirty:
        n = len(dirty.splitlines())
        why.append(f"{n} uncommitted/untracked file(s)")
    if len(git(repo, "worktree", "list", check=False).splitlines()) > 1:
        why.append("has extra worktrees")
    if (repo / ".gitmodules").exists():
        why.append("has submodules (check by hand)")
    gd = repo / ".git"
    if gd.is_dir() and any((gd / x).exists() for x in
                           ("MERGE_HEAD", "rebase-merge", "rebase-apply", "CHERRY_PICK_HEAD")):
        why.append("merge/rebase in progress")
    return why


def last_used(repo):
    """Most recent of: last commit, index change, HEAD change."""
    times = []
    for f in (".git/index", ".git/HEAD", ".git/logs/HEAD"):
        p = repo / f
        if p.exists():
            times.append(p.stat().st_mtime)
    return max(times) if times else 0


def dir_size(p):
    total = 0
    for dp, _, files in os.walk(p):
        for f in files:
            try:
                total += os.lstat(os.path.join(dp, f)).st_size
            except OSError:
                pass
    return total


def human(n):
    for unit in ("B", "K", "M", "G", "T"):
        if n < 1024:
            return f"{n:.0f}{unit}"
        n /= 1024
    return f"{n:.0f}P"


def check_nas_mounted():
    if not NAS_ROOT.is_dir():
        sys.exit(f"NAS not reachable at {NAS_ROOT}. Is it mounted?")


# ------------------------------------------------------------- commands ----
def cmd_sync(args):
    check_nas_mounted()
    for repo in find_local_repos():
        nas = nas_path_for(repo)
        rel = repo.relative_to(LOCAL_ROOT)
        try:
            created = sync_repo(repo, nas)
            miss = missing_on_nas(repo, nas)
            tag = "NEW " if created else ""
            state = "ok" if not miss else f"MISSING {len(miss)} ref(s)"
            print(f"  {tag}{rel}  ->  {nas}  [{state}]")
        except RuntimeError as e:
            print(f"  FAIL {rel}: {e}", file=sys.stderr)


def cmd_status(args):
    check_nas_mounted()
    now = time.time()
    rows, total, freeable = [], 0, 0
    for repo in find_local_repos():
        nas = nas_path_for(repo)
        size = dir_size(repo)
        total += size
        days = int((now - last_used(repo)) / 86400)
        miss = missing_on_nas(repo, nas)
        block = blockers(repo)
        if miss:
            verdict = f"not synced ({len(miss)})"
        elif block:
            verdict = "keep: " + "; ".join(block)
        elif days < args.days:
            verdict = f"keep: used {days}d ago"
        else:
            verdict = "ready to archive"
            freeable += size
        rows.append((str(repo.relative_to(LOCAL_ROOT)), human(size), f"{days}d", verdict))
    w = max((len(r[0]) for r in rows), default=4)
    print(f"{'repo':<{w}}  {'size':>6}  {'idle':>5}  status")
    for r in rows:
        print(f"{r[0]:<{w}}  {r[1]:>6}  {r[2]:>5}  {r[3]}")
    print(f"\nlocal total {human(total)}, freeable now {human(freeable)} "
          f"(idle >= {args.days}d). Then: gitshuffle archive --idle {args.days}")


def resolve_targets(names):
    """Match names like 'bolt', 'puppetlabs/bolt' or a path to local repos."""
    repos = find_local_repos()
    out = []
    for n in names:
        p = Path(n).expanduser()
        if p.is_absolute() or p.exists():
            p = p.resolve()
            hits = [r for r in repos if r == p]
        else:
            hits = [r for r in repos
                    if str(r.relative_to(LOCAL_ROOT)) == n or r.name == n
                    or str(r.relative_to(LOCAL_ROOT)).endswith("/" + n)]
        if len(hits) != 1:
            print(f"  ? {n}: {len(hits)} matches "
                  + ", ".join(str(h.relative_to(LOCAL_ROOT)) for h in hits))
            continue
        out.append(hits[0])
    return out


def cmd_archive(args):
    """Move repos to the NAS: back up every ref, verify, then delete the laptop copy."""
    check_nas_mounted()
    if args.repos:
        targets = resolve_targets(args.repos)
    elif args.idle is not None:
        cutoff = time.time() - args.idle * 86400
        targets = [r for r in find_local_repos() if last_used(r) < cutoff]
    else:
        sys.exit("name repos to archive, or use --idle DAYS")

    freed = 0
    for repo in targets:
        rel = repo.relative_to(LOCAL_ROOT)
        nas = nas_path_for(repo)
        try:
            sync_repo(repo, nas)
        except RuntimeError as e:
            print(f"  skip {rel}: sync failed: {e}")
            continue
        miss, block = missing_on_nas(repo, nas), blockers(repo)
        if miss or block:
            print(f"  skip {rel}: " + "; ".join(block + [f"missing {m}" for m in miss]))
            continue
        size = dir_size(repo)
        if args.yes:
            # remember where it lived so 'recall' can put it back
            git(nas, "config", f"gitshuffle.{MACHINE}.path", str(repo))
            shutil.rmtree(repo)
            print(f"  archived {rel}  ({human(size)})  ->  {nas}")
        else:
            print(f"  would archive {rel}  ({human(size)})  ->  {nas}")
        freed += size
    if args.yes:
        print(f"\nfreed {human(freed)}")
    else:
        print(f"\nwould free {human(freed)}   (dry run, add --yes to do it)")


def is_repo(p):
    return (p / ".git").exists() or ((p / "HEAD").is_file() and (p / "objects").is_dir())


def find_nas_repo(name):
    """'coverity-analysis', 'synopsys/coverity/coverity-analysis' or a full path."""
    p = Path(name).expanduser()
    if p.is_absolute():
        return p
    name = name.strip("/")
    direct = [NAS_ROOT / name] + [h / name for h in NAS_ROOT.iterdir() if h.is_dir()]
    cands = [c for c in direct if c.is_dir() and is_repo(c)]
    if not cands:                      # walk the NAS, matching the end of the path
        for dp, dirs, _ in os.walk(NAS_ROOT):
            d = Path(dp)
            if is_repo(d):
                rel = str(d.relative_to(NAS_ROOT))
                if rel == name or rel.endswith("/" + name):
                    cands.append(d)
                dirs[:] = []
            elif len(d.relative_to(NAS_ROOT).parts) >= MAX_DEPTH:
                dirs[:] = []
            else:
                dirs[:] = [x for x in dirs if not x.startswith(".")]
    cands = sorted(set(cands))
    if len(cands) != 1:
        sys.exit(f"{len(cands)} NAS matches for '{name}'"
                 + (": " + ", ".join(map(str, cands)) if cands else "")
                 + ("  (use more of the path)" if len(cands) > 1 else ""))
    return cands[0]


def cmd_recall(args):
    """Bring an archived repo back to the laptop with all its branches."""
    check_nas_mounted()
    nas = find_nas_repo(args.repo)
    remembered = git(nas, "config", "--get", f"gitshuffle.{MACHINE}.path", check=False)
    if args.dest:
        dest = Path(args.dest).expanduser()
    elif remembered:
        dest = Path(remembered)
    else:
        dest = LOCAL_ROOT / nas.relative_to(NAS_ROOT)
    if dest.exists():
        sys.exit(f"{dest} already exists")
    dest.parent.mkdir(parents=True, exist_ok=True)
    git(dest.parent, "clone", "-q", "--no-checkout", str(nas), str(dest))

    # origin = real upstream (if the NAS copy knows it), nas = the NAS copy
    upstream = git(nas, "config", "--get", "remote.origin.url", check=False)
    git(dest, "remote", "rename", "origin", "nas")
    if upstream:
        git(dest, "remote", "add", "origin", upstream)

    # recreate this laptop's branches, tags and stash
    have = git(nas, "for-each-ref", "--format=%(refname)", f"refs/remotes/{MACHINE}/")
    if have:
        git(dest, "fetch", "-q", "--update-head-ok", "nas",
            f"+refs/remotes/{MACHINE}/*:refs/heads/*")
        git(dest, "fetch", "-q", "--no-tags", "nas", f"refs/{MACHINE}/tags/*:refs/tags/*",
            check=False)
        stashes = git(nas, "for-each-ref", "--format=%(refname)", f"refs/{MACHINE}/stash/")
        stashes = sorted(stashes.splitlines(), key=lambda r: int(r.rsplit("/", 1)[1]))
        for ref in reversed(stashes):                     # oldest first, so order matches
            sha = git(nas, "rev-parse", ref)
            git(dest, "fetch", "-q", "--no-tags", "nas", ref)
            git(dest, "stash", "store", "-m", git(dest, "log", "-1", "--format=%s", sha), sha)
        # check out main/master (or the first branch)
        for b in ("main", "master"):
            if git(dest, "rev-parse", "--verify", "-q", f"refs/heads/{b}", check=False):
                git(dest, "symbolic-ref", "HEAD", f"refs/heads/{b}")
                break
        else:
            first = git(dest, "for-each-ref", "--count=1", "--format=%(refname)", "refs/heads")
            if first:
                git(dest, "symbolic-ref", "HEAD", first)
    git(dest, "checkout", "-q", "-f")
    if upstream:
        git(dest, "fetch", "-q", "origin", check=False)   # ok if offline
    branches = git(dest, "branch", "--format=%(refname:short)").splitlines()
    print(f"recalled {nas}  ->  {dest}")
    print(f"branches: {', '.join(branches)}")


def main():
    global LOCAL_ROOT, NAS_ROOT
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--local", default=str(LOCAL_ROOT))
    ap.add_argument("--nas", default=str(NAS_ROOT))
    sub = ap.add_subparsers(dest="cmd", required=True)

    sub.add_parser("sync", help="back up all local branches to the NAS")

    s = sub.add_parser("status", help="show what is synced and what can be archived")
    s.add_argument("--days", type=int, default=30, help="idle days to count as unused (30)")

    a = sub.add_parser("archive", help="move repos from the laptop to the NAS")
    a.add_argument("repos", nargs="*", help="repo names, e.g. bolt or puppetlabs/bolt")
    a.add_argument("--idle", type=int, metavar="DAYS",
                   help="archive every repo not used for DAYS days")
    a.add_argument("--yes", action="store_true", help="actually do it (default: dry run)")

    r = sub.add_parser("recall", help="bring an archived repo back from the NAS")
    r.add_argument("repo", help="e.g. bolt or puppetlabs/bolt")
    r.add_argument("--dest", help="where to put it (default: where it was before)")

    args = ap.parse_args()
    LOCAL_ROOT, NAS_ROOT = Path(args.local).expanduser(), Path(args.nas).expanduser()
    {"sync": cmd_sync, "status": cmd_status,
     "archive": cmd_archive, "recall": cmd_recall}[args.cmd](args)


if __name__ == "__main__":
    main()
