if [ ! -f pvt.key ]; then
    openssl genrsa -out pvt.key 512
fi
openssl rsa -in pvt.key -pubout > pub.pem
cat <<EOF > emqx.lic
220111
0
10
Foo
contact@foo.com
20220111
100000
10
EOF

openssl dgst -sha256 -sign pvt.key -out emqx.lic.sig emqx.lic
LIC="$(base64 -w 0 emqx.lic)"
SIG="$(base64 -w 0 emqx.lic.sig)"
echo "${LIC}.${SIG}" | tee emqx.lic.signed
