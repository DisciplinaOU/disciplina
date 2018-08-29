<template>
  <div class="transactionInformBlock">
    <div class="transactionInformBlock__title">
      <p class="transactionInformBlock__hash hash">
        <router-link :to="{ name: 'moneyTransactionShow', params: {hash: transaction.txId} }" class="link link--colorBlue">
          {{ transaction.txId }}
        </router-link>
      </p>
      <p class="transactionInformBlock__date">{{ new Date(timestamp) | moment('DD/MM/YYYY HH:MM:SS') }}</p>
      <p class="transactionInformBlock__totalSent btn btn--blue">
        <dscp-format :value="transaction.outValue"/>
      </p>
    </div>
    <div class="transactionInformBlock__fromTo">
      <div class="transactionInformBlock__from informBlock">
        <p class="informBlock__title">{{ $t("transaction.from") }}</p>
        <router-link :to="{ name: 'addressShow', params: {hash: transaction.money.inAcc.addr} }" class="informBlock__hash hash">{{ transaction.money.inAcc.addr }}</router-link>
      </div>
      <div class="transactionInformBlock__to informBlock">
        <p class="informBlock__title">{{ $t("transaction.to") }}</p>
        <router-link class="informBlock__hash hash"
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

export default {
  name: 'TransactionItemMoney',
  props: {
    transaction: Object,
    timestamp: Number
  },
  components: { DscpFormat }
}
</script>
