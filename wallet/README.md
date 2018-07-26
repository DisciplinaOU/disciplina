# Disciplina Wallet

Ariadne-based wallet for Disciplina.

Short usage example:
```
knit> gen-key-pair
L "UDpQX35yW72QVKcL4ufC7YtQMDcPghbX+nKcccrRpHNLZFggwgj8Zo0cbRTepMzHudhyuatPGP5isWJNuNu7vS1ONBs="  -- encrypted secret key
  "D/nSS++ZrpsxqjV/Pj38iH75FJP73YBqyTyKUbqqfck="                                                  -- public key
  LL4qKnmSQJJB3hdRKdL1J61HJYFzSwChWjcoZqzHPFdSdzyLZmdSSKiD                                        -- address

knit> gen-key-pair "passphrase"
L "UBLf8FurpezJgVIFx6SHJMNQtz9eW8GTQs8y7eR5SzKDYVggFbb449bpHZIimt/H+bhjMyIRLj4aTKdpjFsFJPGS4Rg="  -- encrypted secret key
  "v6NE7nYvDzJDYGZ5hZnW4oBDmsmWL+l1L2jpFy3s3+M="                                                  -- public key
  LL4qKtozvcr44DA3rXDDy7kX94aseq5XfnZ2yYbmUNDG316pdEfTo7WD                                        -- address

knit> send-tx "UDpQX35yW72QVKcL4ufC7YtQMDcPghbX+nKcccrRpHNLZFggwgj8Zo0cbRTepMzHudhyuatPGP5isWJNuNu7vS1ONBs=" (tx-out LL4qKtG4mmZF4nC8hMiTpmsG7noszDtWZhiVHuqQsEqtghWdiCFmkkMp 10)
"6745d05dcd39a97faa735be93e64a669fc73ac569f301236e87ec85814ff227b"  -- transaction id

knit> send-tx "UBLf8FurpezJgVIFx6SHJMNQtz9eW8GTQs8y7eR5SzKDYVggFbb449bpHZIimt/H+bhjMyIRLj4aTKdpjFsFJPGS4Rg=" "passphrase" (tx-out LL4qKtG4mmZF4nC8hMiTpmsG7noszDtWZhiVHuqQsEqtghWdiCFmkkMp 10)
"a4355afd95cfde8ea84c42155dfe7018bcd7d41ce5a4fc1bcd3ddb2879f489ce"  -- transaction id

knit> get-balance LL4qKtG4mmZF4nC8hMiTpmsG7noszDtWZhiVHuqQsEqtghWdiCFmkkMp
1337
```
