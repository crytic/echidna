# https://registry.terraform.io/providers/hashicorp/aws/latest/docs/resources/s3_bucket
resource "aws_s3_bucket" "s3_bucket" {
  bucket = "${var.namespace}-${var.project}-bucket"

  tags = {
    Name = "${var.namespace}-${var.project}-bucket"
  }
}

resource "aws_s3_bucket_acl" "s3_bucket_acl" {
  bucket = aws_s3_bucket.s3_bucket.id
  acl    = "private"
}
