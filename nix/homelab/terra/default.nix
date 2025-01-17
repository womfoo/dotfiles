{
  inputs,
  cell,
}:
{
  cloudflare = {
    terraform.required_providers.cloudflare.source = "cloudflare/cloudflare";
    provider = {
      cloudflare.api_token = "\${ var.cloudflare_token }";
    };
    variable = {
      cloudflare_token = {
        type = "string";
      };
    };
    resource = {
      cloudflare_record.au02 = {
        zone_id = inputs.lihim.x86_64-linux.lihim.constants.cloudflare.zone_id;
        name = "au01";
        type = "CNAME";
        ttl = 3600;
        value = "au01.gikos.net";
      };
    };
  };
}
