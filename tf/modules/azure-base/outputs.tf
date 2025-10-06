output "privatednszone" {
  value = module.privatednszone.resource
}

output "resource_group_location" {
  value = azurerm_resource_group.this.location
}

output "resource_group_name" {
  value = azurerm_resource_group.this.name
}

output "vnet_name" {
  value = module.vnet.resource.name
}

output "lb_backend_address_pool" {
  value = module.loadbalancer.azurerm_lb_backend_address_pool
}
