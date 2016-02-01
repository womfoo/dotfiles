{config, ... }: {
certs = {
  country = "PH";
  province = "NCR";
  city = "Makati";
  organization = "Gikos Networks";
  email = "admin@gikos.net";
  cname = "gikos.net";
  nodes = {
    openvpn.cname = "openvpn.gikos.net";
    openvpn.type = "server";
    hangin.cname = "hangin";
    blackbook.cname = "blackbook";
    silverspark.cname = "silverspark";
    greylock.cname = "greylock";
  };
};
ovpn.client = ''
  client
  dev tun
  proto udp
  remote openvpn.gikos.net 1194
  resolv-retry infinite
  nobind
  persist-key
  persist-tun
  remote-cert-tls server
  verb 3
  <ca>
  INSERT_CA_CERT
  </ca>
  <cert>
  INSERT_CLIENT_CERT
  </cert>
  <key>
  INSERT_CLIENT_KEY
  </key>
'';
}
