# Disciplina Faucet

Faucet backend for Disciplina.

Distributes money among users. Each user can get money only once.

### Launch

Run witness node with `./script/launcher/node.hs w`.
Run faucet with `./script/launcher/node.hs f`.
Faucet will take one of witness key from config and use it as source.

### Using

See [swagger spec](https://github.com/DisciplinaOU/disciplina/blob/master/specs/disciplina/faucet/api/faucet.yaml) for the list of served endpoints.
