<template>
  <div class="transactionInformBlock">
    <div class="transactionInformBlock__title">
      <p class="transactionInformBlock__hash hash">
        <router-link :to="{ name: 'moneyTransactionShow', params: {hash: transaction.txId} }" class="link link--colorBlue">
          {{ transaction.txId }}
        </router-link>
      </p>
      <p class="transactionInformBlock__date">07/16/2018 09:33:11</p>
      <p class="transactionInformBlock__totalSent btn btn--blue">
        <dscp-format :value="transaction.money.outValue"/>
      </p>
    </div>
    <div class="transactionInformBlock__fromTo">
      <div class="transactionInformBlock__from informBlock">
        <p class="informBlock__title">From</p>
        <p class="informBlock__hash hash">{{ transaction.money.inAcc.addr }}</p>
      </div>
      <div class="transactionInformBlock__to informBlock">
        <p class="informBlock__title">To</p>
        <p class="informBlock__hash hash"
          v-for="addr in transaction.money.outs"
          :key="addr.address">
          {{ addr.address }}
        </p>
      </div>
    </div>
  </div>
</template>

<script>
import DscpFormat from '@/components/DscpFormat'

export default {
  name: 'TransactionItemMoney',
  props: {
    transaction: Object
  },
  components: { DscpFormat }
}
</script>
