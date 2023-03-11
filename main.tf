module "remote-echidna" {
  source = "github.com/aviggiano/remote-echidna"

  project               = var.project
  project_git_url       = var.project_git_url
  run_tests_cmd         = var.run_tests_cmd
  ec2_instance_key_name = var.ec2_instance_key_name
  project_git_checkout  = var.project_git_checkout
}
