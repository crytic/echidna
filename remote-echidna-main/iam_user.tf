resource "aws_iam_user" "iam_user" {
  name = "${var.namespace}-${var.project}-iam-user"
}

resource "aws_iam_user_policy" "iam_user_policy" {
  name = "${var.namespace}-${var.project}-policy"
  user = aws_iam_user.iam_user.name
  policy = templatefile("iam_user_policy.tftpl", {
    s3_bucket_arn = aws_s3_bucket.s3_bucket.arn
  })
}

resource "aws_iam_access_key" "iam_user_access_key" {
  user = aws_iam_user.iam_user.name
}

