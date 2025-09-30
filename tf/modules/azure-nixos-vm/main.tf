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
  name          = module.naming.virtual_network.name_unique
  parent_id     = "/subscriptions/4e80b227-2b79-448f-a373-00f10ac1aca1/resourceGroups/rg-terragrunt-ae"
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

module "nsg" {
  source              = "Azure/avm-res-network-networksecuritygroup/azurerm"
  version             = "= 0.5.0"
  location            = azurerm_resource_group.this.location
  name                = module.naming.network_security_group.name_unique
  resource_group_name = azurerm_resource_group.this.name
  security_rules = {
    "allowssh" = {
      name                       = "${module.naming.network_security_rule.name_unique}1"
      access                     = "Allow"
      destination_address_prefix = "*"
      destination_port_range     = "22"
      direction                  = "Inbound"
      priority                   = 100
      protocol                   = "Tcp"
      source_address_prefix      = "*"
      source_port_range          = "*"
    }
    "allowhttp" = {
      name                       = "${module.naming.network_security_rule.name_unique}2"
      access                     = "Allow"
      destination_address_prefix = "*"
      destination_port_ranges    = ["80", "443"]
      direction                  = "Inbound"
      priority                   = 200
      protocol                   = "Tcp"
      source_address_prefix      = "*"
      source_port_range          = "*"
    }
  }
}

data "azurerm_image" "image" {
  name                = "nixos-image"
  resource_group_name = "eee"
}

module "virtualmachine" {
  source   = "Azure/avm-res-compute-virtualmachine/azurerm"
  version  = "= 0.19.3"
  location = azurerm_resource_group.this.location
  name     = module.naming.virtual_machine.name_unique
  network_interfaces = {
    network_interface_1 = {
      name = module.naming.network_interface.name_unique
      ip_configurations = {
        ip_configuration_1 = {
          name                          = "${module.naming.network_interface.name_unique}-ipconfig1"
          private_ip_subnet_resource_id = module.vnet.subnets["web"].resource_id
          create_public_ip_address      = true
          public_ip_address_name        = module.naming.public_ip.name_unique
        }
      }
      network_security_groups = {
        nsg_1 = {
          network_security_group_resource_id = module.nsg.resource.id
        }
      }
    }
  }
  resource_group_name = azurerm_resource_group.this.name
  zone                = 1
  account_credentials = {
    password_authentication_disabled = false
  }
  enable_telemetry           = false
  encryption_at_host_enabled = true
  os_disk = {
    caching              = "ReadWrite"
    storage_account_type = "Premium_LRS"
  }
  os_type                  = "Linux"
  # sku_size                 = "Standard_B1s" # 1cpu 1gb
  sku_size                 = "Standard_B2ats_v2"
  source_image_resource_id = data.azurerm_image.image.id
}
