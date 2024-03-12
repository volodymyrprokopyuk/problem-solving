#!/usr/bin/env fish

function installGRPC
  yay -S protobuf
  go install google.golang.org/protobuf/cmd/protoc-gen-go@latest
  go install google.golang.org/grpc/cmd/protoc-gen-go-grpc@latest
end

function compileContract -a contract
  protoc --proto_path (path dirname $contract) --go_out . --go-grpc_out . \
    $contract
end

# installGRPC
# compileContract ecommerce/productinfo.proto
