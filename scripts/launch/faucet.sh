### Run a disciplina faucet

faucet_params="
--path ./tmp/
--config ./configuration.yaml
--config-key demo
--faucet-listen 127.0.0.1:8095
--witness-backend 127.0.0.1:4020
--translated-amount 20
--faucet-keyfile ./run/faucet.key
--log-dir logs
"
