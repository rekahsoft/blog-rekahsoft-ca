#!/bin/bash

display_help() {
    cat <<EOF
Usage: init_env.sh [create|update] <stack-name> <cf-bucket> <cnames>
       init_env.sh info <stack-name>
       init_env.sh [help|--help|-h]
EOF
}

display_info() {
    # Get parameters needed for gitlab-ci.yaml
    S3_BUCKET=$(aws cloudformation describe-stacks --stack-name "$STACK_NAME" --query "Stacks[*].Outputs[?OutputKey=='S3Bucket'].OutputValue" --output text)
    USER_ACCESS_KEY=$(aws cloudformation describe-stacks --stack-name "$STACK_NAME" --query "Stacks[*].Outputs[?OutputKey=='UserAccessKey'].OutputValue" --output text)
    USER_SECRET_KEY=$(aws cloudformation describe-stacks --stack-name "$STACK_NAME" --query "Stacks[*].Outputs[?OutputKey=='UserSecretKey'].OutputValue" --output text)

    echo "S3 Bucket: ${S3_BUCKET}"
    echo "Access Key: ${USER_ACCESS_KEY}"
    echo "Secret Key: ${USER_SECRET_KEY}"
}

# Variables set by the user using cli arguments
OP="$1"
STACK_NAME="$2"
BUCKET="$3"
CNAMES="$4"

case "$OP" in
    update|create)
        # Push cloudformation template to provided bucket
        echo aws s3 cp blog-rekahsoft.yaml "s3://${BUCKET}"

        # Create cloudformation stack
        echo aws cloudformation "${OP}-stack" --stack-name "$STACK_NAME" --template-url "https://${BUCKET}.s3.amazonaws.com/blog-rekahsoft.yaml" --parameters ParameterKey=AlternateURLs,ParameterValue=\"${CNAMES}\" --capabilities CAPABILITY_IAM

        echo aws cloudformation wait stack-update-complete --stack-name "$STACK_NAME"

        display_info
        ;;
    info)
        display_info
        ;;
    help|--help|-h)
        display_help
        ;;
    *)
        echo "Invalid operation! See $0 --help"
        exit 1
        ;;
esac

# Exit gracefully
exit 0
