<template>
  <div class="pseudoTable__row cursorPointer" @click="goToBlock">
      <div class="pseudoTable__cell cell cell__slot">
          {{ block.header.difficulty }}
      </div>
      <div class="pseudoTable__cell cell cell__since">{{ new Date(block.since/1000) | moment('from') }}</div>
      <div class="pseudoTable__cell cell cell__transactions">{{ block.transactionCount }}</div>
      <div class="pseudoTable__cell cell cell__totalSent">{{ block.totalOutput }}</div>
      <div class="pseudoTable__cell cell cell__slotHash hash">{{ block.headerHash }}</div>
      <div class="pseudoTable__cell cell cell__slotLeader">{{ block.header.issuer.substring(0, 8) }}</div>
      <div class="pseudoTable__cell cell cell__size">{{ block.size }}</div>
  </div>
</template>

<script>
export default {
  name: 'BlockItem',
  props: ['block'],
  methods: {
    goToBlock () {
      this.$router.push({name: 'blockShow', params: {hash: this.block.headerHash}})
    }
  }
}
</script>
