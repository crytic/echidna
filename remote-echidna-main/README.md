# remote-echidna

Run echidna on AWS with Terraform

## Description

This project creates the following infrastructure on AWS:

- [Security Group](./terraform/security_group.tf) with SSH access from anywhere, to allow for an easier debugging
- [S3 bucket](./terraform/s3_bucket.tf) with private access to store and load echidna's output between runs
- [IAM Policy](./terraform/iam_user.tf) with access to the S3 bucket
- [IAM User](./terraform/iam_user.tf) with the created IAM Policy
- [EC2 Instance](./terraform/ec2_instance.tf) that runs echidna on the desired git project and uses the IAM User credentials to upload results to S3

## Usage on GitHub Actions

1. Add this project as a dependency of the repository you want to test. For example, if you are using hardhat:

- In case of using Hardhat, use: `npm install https://github.com/aviggiano/remote-echidna.git` or `yarn add https://github.com/aviggiano/remote-echidna.git`
- In case of using Foundry, use: `forge install aviggiano/remote-echidna`

2. Add the [hashicorp/setup-terraform](https://github.com/hashicorp/setup-terraform) GitHub action to your CI and configure the specific input parameters below

### Inputs

| Parameter               | Description                                                                  | Example                                                                                | Required |
| ----------------------- | ---------------------------------------------------------------------------- | -------------------------------------------------------------------------------------- | -------- |
| `ec2_instance_key_name` | EC2 instance key name. Needs to be manually created first on the AWS console | `key.pem`                                                                              | Yes      |
| `project`               | Your project name                                                            | `smart-contracts`                                                                      | Yes      |
| `project_git_url`       | Project Git URL                                                              | `https://github.com/aviggiano/smart-contracts.git`                                     | Yes      |
| `project_git_checkout`  | Project Git checkout (branch or commit hash)                                 | `main`                                                                                 | Yes      |
| `run_tests_cmd`         | Command to run echidna tests                                                 | `yarn && echidna-test test/Contract.sol --contract Contract --config test/config.yaml` | Yes      |

### Example usage

```
steps:
- uses: hashicorp/setup-terraform@v2
  with:
    cli_config_credentials_token: ${{ secrets.TF_API_TOKEN }}
- name: Terraform Init
  run: terraform init
- name: Terraform Apply
  env:
    project: 'smart-contracts'
    project_git_url: 'https://github.com/${{github.repository}}.git'
    project_git_checkout: ${{ github.head_ref || github.ref_name }}
    ec2_instance_key_name: 'key.pem'
    run_tests_cmd: 'yarn && echidna-test test/Contract.sol --contract Contract --config test/config.yaml'
  run: |
    terraform init
    terraform apply -var="ec2_instance_key_name=${{ env.ec2_instance_key_name }}" -var="project=${{ env.project }}" -var="project_git_url=${{ env.project_git_url }}" -var="project_git_checkout=${{ env.project_git_checkout }}" -var="run_tests_cmd=${{ env.run_tests_cmd }}" -no-color -input=false -auto-apply
```

## Development

#### 1. Create a `tfvars` file

Include the parameters required by [vars.tf](./terraform/vars.tf)

```
# vars.tfvars

project               = "echidna-project"
project_git_url       = "https://github.com/aviggiano/echidna-project.git"
project_git_checkout  = "main"
ec2_instance_key_name = "key.pem"
run_tests_cmd         = "yarn && echidna-test test/Contract.sol --contract Contract --config test/config.yaml"
```

### 2. Run terraform

```
terraform apply -var-file vars.tfvars
```

## Next steps

- [ ] Improve state management to avoid conflicting runs from multiple people
- [ ] Perform cleanup of terraform state after the job finishes
- [ ] Integrate with Terraform Cloud
- [ ] Create AMI with all required software instead of [installing everything](./terraform/user_data.tftpl) at each time (would speed up about 1min)
