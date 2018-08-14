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

export default {
  getBlocks (cb) {
    setTimeout(() => cb(_blocks), 100)
  }
}
