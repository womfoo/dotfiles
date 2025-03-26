#!/usr/bin/env python3

import os
import re
import subprocess
import sys

base_root = '/home/kranium/git'

if __name__ == '__main__':
    args = sys.argv[1:]
    if not args:
        sys.exit('usage: ' + sys.argv[0] + ' <repository>')
    else:
        repo_url = args[0]
        # drop prefix and ~ in ~username
        m = re.search(r'(git@|https://)(?P<provider>[\w\-\.]+)(\/|:)(?P<user>[\~\w\-\.]+)\/(?P<repo>[\w\-\.]+)', repo_url)
        user_root = base_root + '/' + m.group('provider') + '/' + m.group('user')
        repo_root = user_root + '/' + m.group('repo')
        os.makedirs(user_root, exist_ok=True)
        print (m.group('provider') + '/' + m.group('user') + '/' + m.group('repo'))
        if os.path.isdir(repo_root):
            subprocess.call(['git', 'fetch'], cwd=repo_root)
        else:
            subprocess.call(['git', 'clone', repo_url], cwd=user_root)
