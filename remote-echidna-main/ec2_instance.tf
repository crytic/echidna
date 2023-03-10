# https://registry.terraform.io/providers/hashicorp/aws/latest/docs/resources/instance
data "aws_ami" "ubuntu" {
  most_recent = true

  filter {
    name   = "name"
    values = ["ubuntu/images/hvm-ssd/ubuntu-jammy-22.04-amd64-server-*"]
  }

  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }

  owners = ["099720109477"] # Canonical
}


resource "aws_instance" "ec2_instance" {
  ami                         = data.aws_ami.ubuntu.id
  instance_type               = var.instance_type
  vpc_security_group_ids      = [aws_security_group.security_group.id]
  user_data_replace_on_change = true

  tags = {
    Name = "${var.namespace}-${var.project}-instance"
  }

  key_name = var.ec2_instance_key_name
  user_data = templatefile("user_data.tftpl", {
    namespace             = var.namespace,
    project               = var.project,
    s3_bucket             = aws_s3_bucket.s3_bucket.id,
    solidity_version      = var.solidity_version,
    project_git_url       = var.project_git_url,
    project_git_checkout  = var.project_git_checkout,
    run_tests_cmd         = var.run_tests_cmd,
    aws_access_key_id     = aws_iam_access_key.iam_user_access_key.id,
    aws_secret_access_key = aws_iam_access_key.iam_user_access_key.secret,
  })
}
