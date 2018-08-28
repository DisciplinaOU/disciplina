<template>
  <div class="pseudoTable__row">
      <div class="pseudoTable__cell cell cell__slot">
        <router-link :to="{ name: 'blockShow', params: {hash: block.headerHash} }" class="link link--colorBlue">
          {{ block.header.difficulty }}
        </router-link>
      </div>
      <div class="pseudoTable__cell cell cell__since">{{ new Date(block.since/1000) | moment('from') }}</div>
      <div class="pseudoTable__cell cell cell__transactions">{{ block.transactionCount }}</div>
      <div class="pseudoTable__cell cell cell__totalSent">{{ block.totalOutput }}</div>
      <div class="pseudoTable__cell cell cell__slotLeader hash">{{ block.header.issuer }}</div>
      <div class="pseudoTable__cell cell cell__size">{{ block.size }}</div>
  </div>
</template>

<script>
export default {
  name: 'BlockItem',
  props: ['block']
}
</script>
