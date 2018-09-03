<template>
  <div class="pseudoTable__row cursorPointer" @click="goToBlock">
      <div class="pseudoTable__cell cell cell__slot">
          {{ block.header.difficulty }}
      </div>
      <div class="pseudoTable__cell cell cell__since"><from-now :dt="block.since/1000"/></div>
      <div class="pseudoTable__cell cell cell__transactions">{{ block.transactionCount }}</div>
      <div class="pseudoTable__cell cell cell__totalSent">{{ block.totalOutput }}</div>
      <div class="pseudoTable__cell cell cell__slotHash hash">{{ block.headerHash }}</div>
      <div class="pseudoTable__cell cell cell__slotLeader">{{ block.header.issuer.substring(0, 8) }}</div>
      <div class="pseudoTable__cell cell cell__size">{{ block.size }}</div>
  </div>
</template>

<script>
import FromNow from '@/components/FromNow'

export default {
  name: 'BlockItem',
  props: ['block'],
  components: { FromNow },
  methods: {
    goToBlock () {
      this.$router.push({name: 'blockShow', params: {hash: this.block.headerHash}})
    }
  }
}
</script>
