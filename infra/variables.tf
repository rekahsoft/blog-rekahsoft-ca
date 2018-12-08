variable "workspace_iam_roles" {
  default = {
    default    = "arn:aws:iam::068963069293:role/pipeline-role"
    staging    = "arn:aws:iam::068963069293:role/pipeline-role"
    production = "arn:aws:iam::068963069293:role/pipeline-role"
  }
}

variable "region" {
  default = "ca-central-1"
}

variable "project" {
  default = "blog-rekahsoft-ca"
}

variable "dns_apex" {}

variable "subdomain" {
  default = ""
}

variable "enable_naked_domain" {
  default = false
}
