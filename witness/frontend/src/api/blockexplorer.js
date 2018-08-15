const _blocks = [
  { headerHash: 'HASH1', issuer: 'Issuer 1', difficulty: 1, slotId: 1, prevHash: 'PREVHASH1' },
  { headerHash: 'HASH2', issuer: 'Issuer 2', difficulty: 2, slotId: 2, prevHash: 'PREVHASH2' },
  { headerHash: 'HASH3', issuer: 'Issuer 3', difficulty: 3, slotId: 3, prevHash: 'PREVHASH3' },
  { headerHash: 'HASH4', issuer: 'Issuer 4', difficulty: 4, slotId: 4, prevHash: 'PREVHASH4' },
  { headerHash: 'HASH5', issuer: 'Issuer 5', difficulty: 5, slotId: 5, prevHash: 'PREVHASH5' },
  { headerHash: 'HASH1', issuer: 'Issuer 1', difficulty: 1, slotId: 1, prevHash: 'PREVHASH1' },
  { headerHash: 'HASH2', issuer: 'Issuer 2', difficulty: 2, slotId: 2, prevHash: 'PREVHASH2' },
  { headerHash: 'HASH3', issuer: 'Issuer 3', difficulty: 3, slotId: 3, prevHash: 'PREVHASH3' },
  { headerHash: 'HASH4', issuer: 'Issuer 4', difficulty: 4, slotId: 4, prevHash: 'PREVHASH4' },
  { headerHash: 'HASH5', issuer: 'Issuer 5', difficulty: 5, slotId: 5, prevHash: 'PREVHASH5' },
  { headerHash: 'HASH1', issuer: 'Issuer 1', difficulty: 1, slotId: 1, prevHash: 'PREVHASH1' },
  { headerHash: 'HASH2', issuer: 'Issuer 2', difficulty: 2, slotId: 2, prevHash: 'PREVHASH2' },
  { headerHash: 'HASH3', issuer: 'Issuer 3', difficulty: 3, slotId: 3, prevHash: 'PREVHASH3' },
  { headerHash: 'HASH4', issuer: 'Issuer 4', difficulty: 4, slotId: 4, prevHash: 'PREVHASH4' },
  { headerHash: 'HASH5', issuer: 'Issuer 5', difficulty: 5, slotId: 5, prevHash: 'PREVHASH5' },
  { headerHash: 'HASH1', issuer: 'Issuer 1', difficulty: 1, slotId: 1, prevHash: 'PREVHASH1' },
  { headerHash: 'HASH2', issuer: 'Issuer 2', difficulty: 2, slotId: 2, prevHash: 'PREVHASH2' },
  { headerHash: 'HASH3', issuer: 'Issuer 3', difficulty: 3, slotId: 3, prevHash: 'PREVHASH3' },
  { headerHash: 'HASH4', issuer: 'Issuer 4', difficulty: 4, slotId: 4, prevHash: 'PREVHASH4' },
  { headerHash: 'HASH5', issuer: 'Issuer 5', difficulty: 5, slotId: 5, prevHash: 'PREVHASH5' }
]

const _transactions = [
  { txType: 'money', txId: 'TXID1', money: { outValue: '100' } },
  { txType: 'money', txId: 'TXID2', money: { outValue: '200' } },
  { txType: 'money', txId: 'TXID3', money: { outValue: '300' } },
  { txType: 'money', txId: 'TXID4', money: { outValue: '400' } },
  { txType: 'money', txId: 'TXID5', money: { outValue: '500' } },
  { txType: 'educator', txId: 'ETXID5', publication: { author: 'AUTHORHASH1', block: 'BLOCKHASH1', prevBlock: 'PREVBLOCKHASH1' } },
  { txType: 'educator', txId: 'ETXID5', publication: { author: 'AUTHORHASH2', block: 'BLOCKHASH2', prevBlock: 'PREVBLOCKHASH2' } },
  { txType: 'educator', txId: 'ETXID5', publication: { author: 'AUTHORHASH3', block: 'BLOCKHASH3', prevBlock: 'PREVBLOCKHASH3' } },
  { txType: 'educator', txId: 'ETXID5', publication: { author: 'AUTHORHASH4', block: 'BLOCKHASH4', prevBlock: 'PREVBLOCKHASH4' } },
  { txType: 'educator', txId: 'ETXID5', publication: { author: 'AUTHORHASH5', block: 'BLOCKHASH5', prevBlock: 'PREVBLOCKHASH5' } }
]

const _address = {
  balances: {
    confirmed: 100
  },
  transactions: [
    {
      txType: 'money',
      txId: '8238fe78c20db47a78907795e379b6690202a120fc2543891a00ba9aaa7f00db',
      headerHash: '8238fe78c20db47a78907795e379b6690202a120fc2543891a00ba9aaa7f00db',
      money: {
        inAcc: {
          addr: '8238fe78c20db47a78907795e379b6690202a120fc2543891a00ba9aaa7f00db',
          nonce: 0
        },
        outValue: 100,
        outs: [
          {
            address: '8238fe78c20db47a78907795e379b6690202a120fc2543891a00ba9aaa7f00db',
            value: 10
          },
          {
            address: '8238fe78c20db47a78907795e379b6690202a120fc2543891a00ba9aaa7f00db',
            value: 20
          }
        ]
      }
    },
    {
      txType: 'educator',
      txId: '8238fe78c20db47a78907795e379b6690202a120fc2543891a00ba9aaa7f00db',
      headerHash: '8238fe78c20db47a78907795e379b6690202a120fc2543891a00ba9aaa7f00db',
      publication: {
        author: '8238fe78c20db47a78907795e379b6690202a120fc2543891a00ba9aaa7f00db',
        block: '8238fe78c20db47a78907795e379b6690202a120fc2543891a00ba9aaa7f00db',
        prevBlock: '8238fe78c20db47a78907795e379b6690202a120fc2543891a00ba9aaa7f00db'
      }
    }
  ]
}

export default {
  getBlocks (cb) {
    setTimeout(() => cb(_blocks), 100)
  },
  getBlock (cb, blockHash) {
    setTimeout(() => cb(_blocks[0]), 100)
  },
  getTransactions (cb) {
    setTimeout(() => cb(_transactions), 100)
  },
  getTransaction (cb, transactionHash) {
    setTimeout(() => cb(_transactions[0]), 100)
  },
  getEducatorTransaction (cb, transactionHash) {
    setTimeout(() => cb(_transactions[6]), 100)
  },
  getAddress (cb, addressHash) {
    cb(_address)
    // setTimeout(() => cb(_address), 100)
  }
}
