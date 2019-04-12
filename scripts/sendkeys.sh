#!/usr/bin/env bash

KEY_DIR=/home/kranium/encfs/darcs.private/gikos-it/keys
YAML_FILE=~/sendkeys.yaml

case $1 in
  password)
      ENTRY=$(yq -r '.passwords | keys[]' $YAML_FILE | dmenu)
      SECRET=$(eyaml decrypt --pkcs7-private-key=$KEY_DIR/private_key.pkcs7.pem --pkcs7-public-key=$KEY_DIR/public_key.pkcs7.pem -e $YAML_FILE | yq -r .passwords.$ENTRY | sed -e 's/^DEC::PKCS7\[//' -e 's/]!$//')
  ;;
  totp)
      ENTRY=$(yq -r '.totps | keys[]' $YAML_FILE | dmenu)
      TOKEN=$(eyaml decrypt --pkcs7-private-key=$KEY_DIR/private_key.pkcs7.pem --pkcs7-public-key=$KEY_DIR/public_key.pkcs7.pem -e $YAML_FILE | yq -r .totps.$ENTRY | sed -e 's/^DEC::PKCS7\[//' -e 's/]!$//')
      SECRET=$(oathtool --totp -b $TOKEN)
  ;;
  alias_)
      SECRET=$(yq -r .aliases[] $YAML_FILE | dmenu)
  ;;
  *)
  echo "invalid usage"
  exit 1
  ;;
esac

xdotool type "$SECRET"
