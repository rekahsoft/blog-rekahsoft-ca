terraform {
  required_version = ">= 0.12"

  backend "s3" {
    region         = "ca-central-1"
    bucket         = "rekahsoft-terraform"
    key            = "blog-rekahsoft-ca/blog-rekahsoft-ca.tfstate"
    dynamodb_table = "rekahsoft-terraform"
  }
}

provider "aws" {
  region  = var.region
  version = "= 2.70.0"

  assume_role {
    role_arn = var.workspace_iam_roles[terraform.workspace]
  }
}

provider "aws" {
  alias   = "us_east_1"
  region  = "us-east-1"
  version = "= 2.70.0"

  assume_role {
    role_arn = var.workspace_iam_roles[terraform.workspace]
  }
}

provider "null" {
  version = "~> 2.1"
}

provider "random" {
  version = "= 2.1.2"
}

#
# Local values to be re-used throughout

locals {
  common_tags = {
    "Project"     = var.project
    "Environment" = terraform.workspace
  }

  cdn_origin_id = "${terraform.workspace}-origin-cdn"
  www           = var.enable_naked_domain ? "" : "www."
  subdomain     = var.subdomain == "" ? "" : "${var.subdomain}."
  naked_domain  = "${local.subdomain}${var.dns_apex}"
  domain        = "${local.www}${local.naked_domain}"
  project_env   = "${var.project}-${terraform.workspace}"

  bucket_arn     = aws_s3_bucket.static.arn
  user_arn       = aws_iam_user.app_deploy.arn
  cloudfront_arn = aws_cloudfront_origin_access_identity.origin_access_identity.iam_arn
}

#
# Data Sources

data "aws_iam_policy_document" "s3_origin_policy" {
  statement {
    principals {
      type = "AWS"
      identifiers = [local.cloudfront_arn]
    }
    actions = ["s3:GetObject"]
    resources = ["${local.bucket_arn}/*"]
  }

  statement {
    principals {
      type = "AWS"
      identifiers = [local.user_arn]
    }
    actions = ["s3:ListBucket"]
    resources = ["${local.bucket_arn}"]
  }

  statement {
    principals {
      type = "AWS"
      identifiers = [local.user_arn]
    }
    actions = ["s3:*"]
    resources = ["${local.bucket_arn}/*"]
  }
}

data "aws_route53_zone" "external" {
  name = "${var.dns_apex}."
}

#
# Resources

resource "aws_acm_certificate" "cert" {
  domain_name               = local.domain
  subject_alternative_names = compact([var.enable_naked_domain ? "" : local.naked_domain])
  validation_method         = "DNS"
  tags                      = local.common_tags

  provider = aws.us_east_1
}

resource "aws_route53_record" "cert_validation" {
  count = 1 + (var.enable_naked_domain ? 0 : 1)

  zone_id = data.aws_route53_zone.external.id
  name    = aws_acm_certificate.cert.domain_validation_options[count.index]["resource_record_name"]
  type    = aws_acm_certificate.cert.domain_validation_options[count.index]["resource_record_type"]
  ttl     = 60
  records = [aws_acm_certificate.cert.domain_validation_options[count.index]["resource_record_value"]]
}

resource "aws_acm_certificate_validation" "cert" {
  certificate_arn         = aws_acm_certificate.cert.arn
  validation_record_fqdns = aws_route53_record.cert_validation.*.fqdn

  provider = aws.us_east_1
}

resource "aws_s3_bucket" "static" {
  bucket_prefix = local.project_env
  acl           = "private"

  website {
    index_document = "index.html"
    error_document = "error.html"
  }

  tags = local.common_tags
}

resource "aws_s3_bucket" "static_redirect" {
  count = var.enable_naked_domain ? 0 : 1

  bucket_prefix = local.project_env
  acl           = "private"

  website {
    redirect_all_requests_to = "https://${local.domain}"
  }

  tags = local.common_tags
}

resource "aws_s3_bucket" "static_logs" {
  bucket_prefix = local.project_env
  acl           = "private"
}

resource "random_string" "app_deploy_username" {
  length           = 16
  special          = true
  override_special = "_+=,.@-"
}

resource "aws_iam_user" "app_deploy" {
  name = random_string.app_deploy_username.result
}

resource "aws_iam_access_key" "app_deploy" {
  user = aws_iam_user.app_deploy.name
  #  pgp_key = "keybase:some_person_that_exists"
}

resource "aws_route53_record" "static" {
  zone_id = data.aws_route53_zone.external.zone_id
  name    = "${local.domain}."
  type    = "A"

  alias {
    name                   = aws_cloudfront_distribution.cdn.domain_name
    zone_id                = aws_cloudfront_distribution.cdn.hosted_zone_id
    evaluate_target_health = true
  }
}

resource "aws_route53_record" "static_ipv6" {
  zone_id = data.aws_route53_zone.external.zone_id
  name    = "${local.domain}."
  type    = "AAAA"

  alias {
    name                   = aws_cloudfront_distribution.cdn.domain_name
    zone_id                = aws_cloudfront_distribution.cdn.hosted_zone_id
    evaluate_target_health = true
  }
}

resource "aws_route53_record" "static_redirect" {
  count = var.enable_naked_domain ? 0 : 1

  zone_id = data.aws_route53_zone.external.zone_id
  name    = "${local.naked_domain}."
  type    = "A"

  alias {
    name                   = aws_cloudfront_distribution.cdn_redirect[0].domain_name
    zone_id                = aws_cloudfront_distribution.cdn_redirect[0].hosted_zone_id
    evaluate_target_health = true
  }
}

resource "aws_route53_record" "static_redirect_ipv6" {
  count = var.enable_naked_domain ? 0 : 1

  zone_id = data.aws_route53_zone.external.zone_id
  name    = "${local.naked_domain}."
  type    = "AAAA"

  alias {
    name                   = aws_cloudfront_distribution.cdn_redirect[0].domain_name
    zone_id                = aws_cloudfront_distribution.cdn_redirect[0].hosted_zone_id
    evaluate_target_health = true
  }
}

resource "aws_s3_bucket_policy" "static_policy" {
  bucket = aws_s3_bucket.static.id
  policy = data.aws_iam_policy_document.s3_origin_policy.json
}

resource "aws_cloudfront_origin_access_identity" "origin_access_identity" {
  comment = "Origin access policy for ${local.project_env}"
}

resource "aws_cloudfront_distribution" "cdn" {
  # Static file origin
  origin {
    domain_name = aws_s3_bucket.static.bucket_regional_domain_name
    origin_id   = local.cdn_origin_id

    s3_origin_config {
      origin_access_identity = aws_cloudfront_origin_access_identity.origin_access_identity.cloudfront_access_identity_path
    }
  }

  enabled             = true
  is_ipv6_enabled     = true
  comment             = "CDN for ${var.project} (environment ${terraform.workspace})"
  default_root_object = "index.html"

  # Return index.html for any route that is not found
  custom_error_response {
    error_caching_min_ttl = 0
    error_code            = 403
    response_code         = 200
    response_page_path    = "/index.html"
  }

  logging_config {
    include_cookies = false
    bucket          = aws_s3_bucket.static_logs.bucket_domain_name
  }

  aliases = [local.domain]

  default_cache_behavior {
    allowed_methods  = ["GET", "HEAD", "OPTIONS"]
    cached_methods   = ["GET", "HEAD", "OPTIONS"]
    target_origin_id = local.cdn_origin_id

    forwarded_values {
      query_string = false

      cookies {
        forward = "none"
      }
    }

    min_ttl                = 0
    default_ttl            = 3600
    max_ttl                = 86400
    viewer_protocol_policy = "redirect-to-https"
  }

  # Cache behavior with precedence 0
  ordered_cache_behavior {
    path_pattern     = "index.html"
    allowed_methods  = ["GET", "HEAD", "OPTIONS"]
    cached_methods   = ["GET", "HEAD", "OPTIONS"]
    target_origin_id = local.cdn_origin_id

    forwarded_values {
      query_string = false
      headers      = ["Origin"]
      cookies {
        forward = "none"
      }
    }

    min_ttl                = 0
    default_ttl            = 0
    max_ttl                = 0
    compress               = true
    viewer_protocol_policy = "redirect-to-https"
  }

  price_class = "PriceClass_100"

  restrictions {
    geo_restriction {
      restriction_type = "none"
    }
  }

  tags = local.common_tags

  viewer_certificate {
    acm_certificate_arn      = aws_acm_certificate_validation.cert.certificate_arn
    ssl_support_method       = "sni-only"
    minimum_protocol_version = "TLSv1.1_2016"
  }
}

resource "aws_cloudfront_distribution" "cdn_redirect" {
  count = var.enable_naked_domain ? 0 : 1

  # Static file origin
  origin {
    domain_name = "${aws_s3_bucket.static_redirect[0].id}.${aws_s3_bucket.static_redirect[0].website_domain}"
    origin_id   = local.cdn_origin_id

    custom_origin_config {
      http_port  = 80
      https_port = 443

      origin_ssl_protocols   = ["TLSv1.1", "TLSv1.2"]
      origin_protocol_policy = "http-only"
    }
  }

  enabled         = true
  is_ipv6_enabled = true
  comment         = "CDN redirect for ${var.project} (environment ${terraform.workspace})"

  logging_config {
    include_cookies = false
    bucket          = aws_s3_bucket.static_logs.bucket_domain_name
  }

  aliases = [local.naked_domain]

  default_cache_behavior {
    allowed_methods  = ["GET", "HEAD", "OPTIONS"]
    cached_methods   = ["GET", "HEAD", "OPTIONS"]
    target_origin_id = local.cdn_origin_id

    forwarded_values {
      query_string = false

      cookies {
        forward = "none"
      }
    }

    min_ttl                = 0
    default_ttl            = 3600
    max_ttl                = 86400
    viewer_protocol_policy = "redirect-to-https"
  }

  price_class = "PriceClass_100"

  restrictions {
    geo_restriction {
      restriction_type = "none"
    }
  }

  tags = local.common_tags

  viewer_certificate {
    acm_certificate_arn      = aws_acm_certificate_validation.cert.certificate_arn
    ssl_support_method       = "sni-only"
    minimum_protocol_version = "TLSv1.1_2016"
  }
}

resource "null_resource" "deploy_app" {
  triggers = {
    always = uuid()
  }

  provisioner "local-exec" {
    interpreter = ["bash", "-c"]
    command     = <<SCRIPT
set -eo pipefail;

: Create temporary aws config and credentials files
export AWS_CONFIG_FILE=$(mktemp);
export AWS_SHARED_CREDENTIALS_FILE=$(mktemp);

: Add default AWS account profile;
aws configure --profile ${aws_iam_user.app_deploy.name} set aws_access_key_id ${aws_iam_access_key.app_deploy.id};
aws configure --profile ${aws_iam_user.app_deploy.name} set aws_secret_access_key ${aws_iam_access_key.app_deploy.secret};
aws configure --profile ${aws_iam_user.app_deploy.name} set region ${var.region};

: Create a random string to be used as a temporary directory name;
TMPDIR=/tmp/$(printf '%s' {a..z} {A..Z} {0..9} | fold -w1 | shuf | paste -s -d '' | head -c16);

: Copy site files so that they get a new date/time stamp, allowing 's3 sync' to operate correctly;
cp -r ${var.site_static_files_dir} $${TMPDIR};

: Allow copied site files to be removable after deployment;
chmod u+rw -R $${TMPDIR};

: Sync latest app build to s3 bucket;
aws --profile ${aws_iam_user.app_deploy.name} s3 sync --delete $${TMPDIR} s3://${aws_s3_bucket.static.id}/;

: Cleanup temporary aws config, its credentials files as well as the copied site files;
rm -r $${AWS_CONFIG_FILE} $${AWS_SHARED_CREDENTIALS_FILE} $${TMPDIR};
SCRIPT

  }
}
