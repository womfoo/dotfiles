NIX_SQLITE_PATH=/nix/var/nix/db/
NIX_SQLITE_BACKUP_PATH="/armorydata/$HOSTNAME-nix-var-nix-db/"

rsync \
  -avh \
  $NIX_SQLITE_PATH \
  $NIX_SQLITE_BACKUP_PATH

rsync \
  -avh \
  --exclude='.bundle' \
  --exclude='.cache' \
  --exclude='.compose-cache' \
  --exclude='.conda' \
  --exclude='.config/google-chrome/Default/Service Worker/CacheStorage' \
  --exclude='.config/google-chrome/Default/WebStorage' \
  --exclude='.daedalus' \
  --exclude='.docker' \
  --exclude='.dropbox-dist' \
  --exclude='.emacs.d/.cache' \
  --exclude='.emacs.d/eln-cache' \
  --exclude='.emacs.d/elpa' \
  --exclude='.emacs.d/quelpa' \
  --exclude='.mozilla/firefox/kranium/storage/default' \
  --exclude='.gem' \
  --exclude='.gnupg' \
  --exclude='.gradle' \
  --exclude='.local' \
  --exclude='.npm' \
  --exclude='.nv' \
  --exclude='.platformio' \
  --exclude='.sbt' \
  --exclude='.stack' \
  --exclude='.thunderbird' \
  --exclude='.vagrant.d/boxes' \
  --exclude='.vscode' \
  --exclude='.xmonad' \
  --exclude='encfs' \
  --exclude='git' \
  --exclude='go' \
  --exclude='node_modules' \
  --exclude='vagrantboxen' \
  --exclude='work' \
  /home/kranium/ \
  /armorydata/kranium/
