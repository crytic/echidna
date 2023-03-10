output "ec2_instance_public_ip" {
  value = aws_instance.ec2_instance.public_ip
}

output "s3_bucket_name" {
  value = aws_s3_bucket.s3_bucket.id
}
