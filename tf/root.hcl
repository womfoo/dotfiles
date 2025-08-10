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
}
EOF
}
