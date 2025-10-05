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
