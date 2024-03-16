#!/usr/bin/env fish

function selfSignedCert -a key cert cn
  # generates a key pair and a  self-signed certificate of a public key
  openssl req -x509 -newkey rsa:4096 -sha256 -nodes \
    -subj "/CN=$cn" -addext "subjectAltName = DNS:$cn" -days 365 \
    -keyout $key.pem -out $cert.pem
end

function signCert -a cakey cacert key cert cn
  # generates a key pair
  openssl genrsa -out $key.pem 4096
  # creates a Certificate Signing Request CSR for a CN
  and openssl req -new -key $key.pem -out $key.csr \
    -subj "/CN=$cn" -addext "subjectAltName = DNS:$cn"
  # creates a certificate by signing a CSR for a time duration
  and openssl x509 -req -in $key.csr -out $cert.pem \
    -CAkey $cakey.pem -CA $cacert.pem -CAcreateserial -days 365 \
    -copy_extensions copyall
end

function showCert -a cert
  openssl x509 --in $cert.pem -text --noout |
    rg 'Issuer:|Subject:|Public-Key:|CA:'
  openssl x509 --in $cert.pem -text --noout |
    rg -A1 'Subject Alternative Name:'
end

function serverTLS
  rm -f srv{key,cert}.pem
  selfSignedCert srvkey srvcert localhost
  and showCert srvcert
end

function mutualTLS
  # self-signed root CA certificate
  rm -f ca{key,cert}.pem
  selfSignedCert cakey cacert localCA
  and showCert cacert
  # server certificate signed by a root CA
  rm -f srv{key,cert}.{csr,pem}
  signCert cakey cacert srvkey srvcert localhost
  and showCert srvcert
  # client certificate signed by a root CA
  rm -f cln{key,cert}.{csr,pem}
  signCert cakey cacert clnkey clncert localclient
  and showCert clncert
end

# serverTLS
# mutualTLS

# for c in *cert.pem
#   echo \* $c
#   showCert (path change-extension '' $c)
# end
