remote_state {
  backend = "azurerm"

  config = {
    resource_group_name  = "${get_env("TG_AZURE_RESOURCE_GROUP")}"
    storage_account_name = "${get_env("TG_AZURE_STORAGE_ACCOUNT")}"
    container_name       = "${get_env("TG_AZURE_CONTAINER_NAME")}"
    key                  = "${path_relative_to_include()}/terraform.tfstate"
  }
}

generate "backend" {
  path      = "backend.tf"
  if_exists = "overwrite_terragrunt"
  contents  = <<EOF
terraform {
  backend "azurerm" {
    resource_group_name  = "${get_env("TG_AZURE_RESOURCE_GROUP")}"
    storage_account_name = "${get_env("TG_AZURE_STORAGE_ACCOUNT")}"
    container_name       = "${get_env("TG_AZURE_CONTAINER_NAME")}"
    key                  = "${path_relative_to_include()}/terraform.tfstate"
  }
  required_providers {
    azapi = {
      source = "Azure/azapi"
    }
  }
}
EOF
}

generate "provider" {
  path      = "provider.tf"
  if_exists = "overwrite_terragrunt"
  contents  = <<EOF
provider "azurerm" {
  features {}
}
provider "azapi" {
}
EOF
}
