module "naming" {
  source  = "Azure/naming/azurerm"
  version = "= 0.4.2"
  suffix  = [var.name]
}

module "nsg" {
  source              = "Azure/avm-res-network-networksecuritygroup/azurerm"
  version             = "= 0.5.0"
  location            = var.resource_group_location
  name                = module.naming.network_security_group.name_unique
  resource_group_name = var.resource_group_name
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

data "azurerm_subnet" "subnet" {
  name = var.subnet_name
  virtual_network_name = var.vnet_name
  resource_group_name = var.resource_group_name
}

data "azurerm_image" "image" {
  name                = "nixos-image"
  resource_group_name = "eee"
}

module "virtualmachine" {
  source   = "Azure/avm-res-compute-virtualmachine/azurerm"
  version  = "= 0.19.3"
  location = var.resource_group_location
  name     = module.naming.virtual_machine.name_unique
  network_interfaces = {
    network_interface_1 = {
      name = module.naming.network_interface.name_unique
      ip_configurations = {
        ip_configuration_1 = {
          name                          = "${module.naming.network_interface.name_unique}-ipconfig1"
          private_ip_subnet_resource_id = data.azurerm_subnet.subnet.id
          # create_public_ip_address      = true
          # public_ip_address_name        = module.naming.public_ip.name_unique
        }
      }
      network_security_groups = {
        nsg_1 = {
          network_security_group_resource_id = module.nsg.resource.id
        }
      }
    }
  }
  resource_group_name = var.resource_group_name
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
  sku_size                 = var.sku_size
  source_image_resource_id = data.azurerm_image.image.id
}
