module "naming" {
  source  = "Azure/naming/azurerm"
  version = "= 0.4.2"
  suffix  = [var.name]
}

resource "azurerm_resource_group" "this" {
  location = var.location
  name     = module.naming.resource_group.name_unique
}

module "vnet" {
  source        = "Azure/avm-res-network-virtualnetwork/azurerm"
  version       = "= 0.11.0"
  address_space = ["10.0.0.0/16"]
  location      = azurerm_resource_group.this.location
  name          = azurerm_resource_group.this.name
  parent_id     = azurerm_resource_group.this.id
  subnets = {
    "web" = {
      name             = "web"
      address_prefixes = ["10.0.0.0/24"]
    }
    "db" = {
      name             = "db"
      address_prefixes = ["10.0.1.0/24"]
    }
  }
}

module "privatednszone" {
  source      = "Azure/avm-res-network-privatednszone/azurerm"
  version     = "= 0.4.2"
  domain_name = "azurepriv.kranium.au"
  parent_id   = azurerm_resource_group.this.id
}

module "loadbalancer" {
  version = "= 0.4.1"
  source  = "Azure/avm-res-network-loadbalancer/azurerm"

  # Frontend IP Configuration
  frontend_ip_configurations = {
    frontend_configuration_1 = {
      name = "myFrontend"
      # Creates Public IP Address
      create_public_ip_address        = true
      public_ip_address_resource_name = module.naming.public_ip.name_unique
      # zones = ["1", "2", "3"] # Zone-redundant
      # zones = ["None"] # Non-zonal
    }
  }
  backend_address_pools = {
    pool_ssh1 = {
      name = "pool_ssh1"
    }
    pool_ssh2 = {
      name = "pool_ssh2"
    }
  }
  lb_probes = {
    probe_ssh = {
      name     = "lb-ssh-probe"
      protocol = "Tcp"
      port     = 22
      # interval_in_seconds = 5
    }
  }
  lb_rules = {
    rule_1 = {
      name                              = "rule_1"
      frontend_ip_configuration_name    = "myFrontend"
      backend_address_pool_object_names = ["pool_ssh1"]
      protocol                          = "Tcp"
      frontend_port                     = 2201
      backend_port                      = 22
      probe_object_name                 = "probe_ssh"
    }
    rule_2 = {
      name                              = "rule_2"
      frontend_ip_configuration_name    = "myFrontend"
      backend_address_pool_object_names = ["pool_ssh2"]
      protocol                          = "Tcp"
      frontend_port                     = 2202
      backend_port                      = 22
      probe_object_name                 = "probe_ssh"
    }
  }
  # Do not set
  # https://github.com/hashicorp/terraform-provider-azurerm/issues/25962
  # backend_address_pool_configuration = module.vnet.resource.id
  location            = azurerm_resource_group.this.location
  name                = "lbe-${module.naming.unique-seed}"
  resource_group_name = azurerm_resource_group.this.name
  # enable_telemetry    = var.enable_telemetry
}

