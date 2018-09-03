<template>
  <div class="main mainTransaction" v-if="moneyLoaded">
      <div class="transaction marginTop30">
          <div class="container">
              <h3 class="lastSlots__title blockTitle marginBottom30">{{ $t("title") }}</h3>
              <div class="transactionInformBlock">
                  <div class="transactionInformBlock__title">
                      <p class="transactionInformBlock__hash hash">{{ moneyTransaction.txId }}</p>
                      <p class="transactionInformBlock__totalSent btn btn--blue btn--noclick">
                        <dscp-format :value="moneyTransaction.outValue"/>
                      </p>
                  </div>
                  <div class="transactionInformBlock__fromTo">
                      <div class="transactionInformBlock__from informBlock">
                          <p class="informBlock__title">{{ $t("from") }}</p>
                          <router-link :to="{ name: 'addressShow', params: {hash: moneyTransaction.money.inAcc.addr} }" class="informBlock__hash hash">{{ moneyTransaction.money.inAcc.addr }}</router-link>
                      </div>
                      <div class="transactionInformBlock__to informBlock">
                          <p class="informBlock__title">{{ $t("to") }}</p>
                          <div class="transactionInformBlock">
                          <router-link class="informBlock__hash hash"
                            :to="{ name: 'addressShow', params: {hash: addr.outAddr} }"
                            v-for="addr in moneyTransaction.money.outs"
                            :key="addr.outAddr">
                            {{ addr.outAddr }}
                          </router-link>
                      </div>
                  </div>
                </div>
              </div>
              <div class="transaction__summary marginTop30 summary">
                  <div class="summary__title">{{ $t("summary.title") }}</div>
                  <div class="summary__items itemsSummary">
                      <div class="itemsSummary_item itemSummary itemSummary--date">
                          <p class="itemSummary__title">{{ $t("summary.time") }}</p><p class="itemSummary__content"><datetime :dt="moneyTransaction.block.since / 1000"/></p>
                      </div>
                      <div class="itemsSummary_item itemSummary itemSummary--included">
                          <p class="itemSummary__title">{{ $t("summary.includedIn.title") }}</p>
                          <router-link class="itemSummary__content" :to="{ name: 'blockShow', params: {hash: moneyTransaction.block.headerHash} }">{{ $t("summary.includedIn.block") }} {{ moneyTransaction.block.header.difficulty }}</router-link>
                      </div>
                      <div class="itemsSummary_item itemSummary itemSummary--output">
                          <p class="itemSummary__title">{{ $t("summary.totalOutput") }}</p>
                          <p class="itemSummary__content"><dscp-format :value="moneyTransaction.block.totalOutput"/></p>
                      </div>
                      <div class="itemsSummary_item itemSummary itemSummary--fee">
                          <p class="itemSummary__title">{{ $t("summary.fee") }}</p>
                          <p class="itemSummary__content">
                            <dscp-format :value="moneyTransaction.block.totalFees"/>
                          </p>
                      </div>
                  </div>
              </div>
          </div>
      </div>
  </div>
</template>

<script>
import DscpFormat from '@/components/DscpFormat'
import Datetime from '@/components/Datetime'
import { mapGetters, mapActions } from 'vuex'

export default {
  name: 'Transaction',
  i18nOptions: {
    keyPrefix: 'transaction'
  },
  components: { DscpFormat, Datetime },
  beforeMount () {
    this.getMoneyTransaction(this.$route.params.hash)
  },
  beforeRouteUpdate (to, from, next) {
    this.getMoneyTransaction(to.params.hash)
    next()
  },
  computed: {
    ...mapGetters([
      'moneyTransaction',
      'moneyLoaded'
    ])
  },
  methods: {
    ...mapActions([
      'getMoneyTransaction'
    ])
  }
}
</script>
