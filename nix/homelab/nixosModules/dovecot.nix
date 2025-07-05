{
  # lib,
  # pkgs,
  # config,
  ...
}:
{
  services.dovecot2.enable = true;
  services.dovecot2.enableImap = true;
  # FIXME: implement modern equiv?
  # services.dovecot2.modules = [ pkgs.dovecot_fts_xapian ];
  # services.dovecot2.mailPlugins.globally.enable = [ "fts" "fts_xapian" ];
  services.dovecot2.extraConfig = ''
    auth_username_format = %n
    service indexer-worker {
      vsz_limit = 512 M
    }
    service imap {
      vsz_limit = 512 M
    }
  '';
}
