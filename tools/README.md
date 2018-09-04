# Disciplina-tools

Tools for disciplina.

## txperf

An utility to flood witness node with valid transactions.

Run as follows:

```bash
dscp-txperf --config txperf.json
```

Contents of the config file must be in the following format:

```json
{
  "witness": "witness-1.disciplina.serokell.team",
  "accKeys": [
    "zD9OLyp86WwV9b/3HuSuxY9Q9FMB38ik/uGnIS0p/cU=",
    "W+DwF8TXAxy9vgZvcHK9/qIwcz1S4QIZLWyEB3ggEkc="
  ],
  "genAccs": 2,
  "txCount": 20,
  "txAsync": true
}
```

`genAccs` accounts will be generated, and then `txCount` random 1-coin transactions will be sent between the listed and generated accounts.

`txAsync` defines, whether `/tx` or `/tx/async` endpoint is used.