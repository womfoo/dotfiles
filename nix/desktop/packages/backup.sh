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
  --exclude='.conda' \
  --exclude='.compose-cache' \
  --exclude='.daedalus' \
  --exclude='.docker' \
  --exclude='.dropbox-dist' \
  --exclude='.emacs.d/eln-cache' \
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
