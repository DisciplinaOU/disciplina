<template>
  <div class="transactionInformBlock">
    <div class="transactionInformBlock__title">
      <p class="transactionInformBlock__hash hash">
        <router-link :to="{ name: 'moneyTransactionShow', params: {hash: transaction.txId} }" class="link link--colorBlue">
          {{ transaction.txId }}
        </router-link>
      </p>
      <p class="transactionInformBlock__date"><datetime :dt="calcTimestamp"/></p>
      <p class="transactionInformBlock__totalSent btn btn--blue btn--noclick">
        <dscp-format :value="transaction.outValue"/>
      </p>
    </div>
    <div class="transactionInformBlock__fromTo">
      <div class="transactionInformBlock__from informBlock">
        <p class="informBlock__title">{{ $t("transaction.from") }}</p>
        <router-link
          :to="{ name: 'addressShow', params: {hash: transaction.money.inAcc.addr} }"
          :class="{fat: isCurrent(transaction.money.inAcc.addr) }"
          class="informBlock__hash hash">
          {{ transaction.money.inAcc.addr }}
        </router-link>
      </div>
      <div class="transactionInformBlock__to informBlock">
        <p class="informBlock__title">{{ $t("transaction.to") }}</p>
        <router-link class="informBlock__hash hash"
          :class="{fat: isCurrent(addr.outAddr) }"
          :to="{ name: 'addressShow', params: {hash: addr.outAddr} }"
          v-for="addr in transaction.money.outs"
          :key="addr.outAddr">
          {{ addr.outAddr }}
        </router-link >
      </div>
    </div>
  </div>
</template>

<script>
import DscpFormat from '@/components/DscpFormat'
import Datetime from '@/components/Datetime'

export default {
  name: 'TransactionItemMoney',
  props: {
    transaction: Object,
    timestamp: Number,
    currentAddress: String
  },
  components: { DscpFormat, Datetime },
  computed: {
    calcTimestamp () {
      if (this.timestamp) {
        return this.timestamp
      } else {
        return this.transaction.block.since / 1000
      }
    }
  },
  methods: {
    isCurrent (addr) {
      return (this.currentAddress && this.currentAddress === addr)
    }
  }
}
</script>

<style scoped>
.fat {
  font-weight: 500
}
</style>
