#!/bin/sh

set -eux

git clone --recursive https://github.com/dapphub/dapptools/
cd dapptools
#nix-env -f . -iA hevm
cd src/libethjet
sed -i "1s/.*/with import <nixpkgs> {};/" default.nix
nix-build
cd ../../src/hevm
# ugly hack
printf 'cmVzb2x2ZXI6IGx0cy04LjE1CnBhY2thZ2VzOgotIC4KZXh0cmEtZGVwczoKLSBkYXRhLWR3b3JkLTAuMy4xCi0gZ2hjaS1wcmV0dHktMC4wLjIKLSBpcHByaW50LTAuNgotIHNyLWV4dHJhLTEuNDYuMy4yCi0gYnJpY2stMC4zNy4xCi0gbWVnYXBhcnNlYy02LjUuMAotIHJlc3RsZXNzLWdpdC0wLjUuMAotIHJvc2V6aXBwZXItMC4yCi0gcy1jYXJnb3QtMC4xLjQuMAotIHRyZWUtdmlldy0wLjUKLSBIU0gtMi4xLjMKLSBjb25maWctaW5pLTAuMi4yLjAKLSBkYXRhLWNsaXN0LTAuMS4yLjEKLSBwYXJzZXItY29tYmluYXRvcnMtMS4wLjAKLSB2dHktNS4yMQotIHdvcmQtd3JhcC0wLjQuMQoKZmxhZ3M6IHt9CmV4dHJhLXBhY2thZ2UtZGJzOiBbXQppbWFnZToKICBjb250YWluZXI6CiAgICBuYW1lOiBtYnJvY2svaGV2bQogICAgYmFzZTogaGFza2VsbDo4LjAuMQo=' | base64 -d > stack.yaml
stack install
cd ../../..
