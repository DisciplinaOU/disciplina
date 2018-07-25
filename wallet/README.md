# Disciplina Wallet

Ariadne-based wallet for Disciplina.

Short usage example:
```
knit> gen-key-pair
L z1fEXUH5atk6W1VPWkwMMk7rfWIOsK26GMFXIW6hovg=   -- secret key
  DpeMraH5KSkJpx79Rmt+w5ouhx8Yp0dxlBgOgg3bPBQ=   -- public key

knit> gen-key-pair "passphrase"
L UFQtH7e81aWSevzLT9pww1JQNZ2o/kaXzrYxV2AaR/oZBFggIj3ohhCpNXFrvPyxLT9R8r76kihL2l3Eu7O0aLswsQI=  -- encrypted secret key
  sLug89UoeND+Du7zriv+mBwZkDKYT1w420+k6szs6ZQ=                                                  -- public key

knit> send-tx z1fEXUH5atk6W1VPWkwMMk7rfWIOsK26GMFXIW6hovg= (tx-out LL4qKtG4mmZF4nC8hMiTpmsG7noszDtWZhiVHuqQsEqtghWdiCFmkkMp 10) 
"b67d7c65adb4310cf548bdc3440dee1c8730ea19b65ad50be4e8a2ef80932db8"  -- transaction id

knit> send-tx "passphrase" UFQtH7e81aWSevzLT9pww1JQNZ2o/kaXzrYxV2AaR/oZBFggIj3ohhCpNXFrvPyxLT9R8r76kihL2l3Eu7O0aLswsQI= (tx-out LL4qKtG4mmZF4nC8hMiTpmsG7noszDtWZhiVHuqQsEqtghWdiCFmkkMp 10)
"a187f6335885449a5743837c432c541e84fef0a5c2e3c6e044d1396961257e61"  -- transaction id

knit> get-balance LL4qKtG4mmZF4nC8hMiTpmsG7noszDtWZhiVHuqQsEqtghWdiCFmkkMp
1337
```
