module "remote-echidna" {
  source = "github.com/aviggiano/remote-echidna"

  project               = var.project
  project_git_url       = var.project_git_url
  run_tests_cmd         = var.run_tests_cmd
  ec2_instance_key_name = var.ec2_instance_key_name
  project_git_checkout  = var.project_git_checkout
}


variable "project" {
  type        = string
  description = "Your project name"
}

variable "ec2_instance_key_name" {
  type        = string
  description = "EC2 instance key name. Needs to be manually created first on the AWS console"
}

variable "project_git_url" {
  type        = string
  description = "Project Git URL"
}

variable "project_git_checkout" {
  type        = string
  description = "Project Git checkout (branch or commit hash)"
}

variable "run_tests_cmd" {
  type        = string
  description = "Command to run echidna tests"
}
