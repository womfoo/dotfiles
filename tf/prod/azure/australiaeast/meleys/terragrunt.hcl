include "root" {
  path = find_in_parent_folders("root.hcl")
}

terraform {
  source = "../../../../modules/azure-nixos-vm"
}

dependency "base" {
  config_path = "../base"
}

inputs = {
  name                    = "meleys"
  resource_group_location = dependency.base.outputs.resource_group_location
  resource_group_name     = dependency.base.outputs.resource_group_name
  subnet_name             = "web"
  vnet_name               = dependency.base.outputs.vnet_name
  lb_backend_address_pool = dependency.base.outputs.lb_backend_address_pool
}
