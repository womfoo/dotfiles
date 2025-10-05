variable "name" {
  description = "vm name"
  type        = string
}

variable "resource_group_location" {}

variable "resource_group_name" {}

variable "sku_size" {
  description = "vm name"
  default     = "Standard_B2ats_v2"
  # default     = "Standard_B1s" # 1cpu 1gb
  type = string
}

variable "subnet_name" {}

variable "vnet_name" {}
