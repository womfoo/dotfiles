{ config, ... }: {

  services.paperless-ng = {
    enable = true;
    address = "0.0.0.0";
    extraConfig = {
      PAPERLESS_TIME_ZONE = config.time.timeZone;
      PAPERLESS_DISABLE_LOGIN = "false";
      PAPERLESS_LIST_PER_PAGE = 1000;
      PAPERLESS_ALLOWED_HOSTS = "paperless.kranium.net,127.0.0.1";
      #PAPERLESS_INLINE_DOC=  "false";
    };
  };

}
