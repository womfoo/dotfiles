include "root" {
  path = find_in_parent_folders("root.hcl")
}

terraform {
  source = "../../../../modules/azure-base"
}

inputs = {
  location = "australiaeast"
  name     = "au0"
}
