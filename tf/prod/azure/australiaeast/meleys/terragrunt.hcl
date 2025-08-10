include "root" {
  path = find_in_parent_folders("root.hcl")
}

terraform {
  source = "../../../../modules/azure-nixos-vm"
}

inputs = {
  location = "australiaeast"
  name     = "meleys"
}

generate "provider" {
  path      = "provider.tf"
  if_exists = "overwrite_terragrunt"
  contents  = <<EOF
provider "azurerm" {
  features {}
}
provider "random" {}
EOF
}

