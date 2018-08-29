<template>
  <div class="main slot" v-if="blockLoaded">
      <div class="slotInformationBlock marginTop30">
        <div class="container">
          <div class="paginationBlock">
            <router-link :to="{ name: 'blockShow', params: {hash: block.header.prevHash} }"
              class="paginationBlock__btnForward btn btn--arrow btn--forward"
              v-if="block.header.prevHash">
              {{ $t("nav.prev") }}
            </router-link>
            <router-link :to="{ name: 'blockShow', params: {hash: block.nextHash} }"
              class="paginationBlock__btnBack btn--arrow btn btn--back"
              v-if="block.nextHash">
              {{ $t("nav.next") }}
            </router-link>
          </div>
        </div>
          <div class="container">
              <h3 class="lastSlots__title blockTitle marginBottom30 marginTop30">{{ $t("title") }}</h3>
              <div class="slotInform">
                  <div class="slotInform__about slotInformBlock red">
                      <p class="slotInformBlock__title">{{ $t("about.title") }}</p>
                      <div class="slotInformBlock__item">
                          <span class="slotInformBlock__name">{{ $t("about.blockIndex") }}</span><span class="slotInformBlock__value">{{ block.header.difficulty }}</span>
                      </div>
                      <div class="slotInformBlock__item">
                          <span class="slotInformBlock__name">{{ $t("about.transactions") }}</span><span class="slotInformBlock__value">{{ block.transactionCount }}</span>
                      </div>
                      <div class="slotInformBlock__item">
                          <span class="slotInformBlock__name">{{ $t("about.totalOutput") }}</span><span class="slotInformBlock__value"><dscp-format :value="block.totalOutput || 0"/></span>
                      </div>
                      <div class="slotInformBlock__item">
                          <span class="slotInformBlock__name">{{ $t("about.fees") }}</span><span class="slotInformBlock__value"><dscp-format :value="block.totalFees || 0"/></span>
                      </div>
                  </div>
                  <div class="slotInform__hashes slotInformBlock">
                      <p class="slotInformBlock__title">{{ $t("hashes.title") }}</p>
                      <div class="slotInformBlock__item">
                          <span class="slotInformBlock__name">{{ $t("hashes.hash") }}</span><span class="slotInformBlock__value hash">{{ block.headerHash }}</span>
                      </div>
                      <div class="slotInformBlock__item">
                          <span class="slotInformBlock__name">{{ $t("hashes.prev") }}</span><span class="slotInformBlock__value hash">{{ block.header.prevHash }}</span>
                      </div>
                      <div class="slotInformBlock__item">
                          <span class="slotInformBlock__name">{{ $t("hashes.next") }}</span><span class="slotInformBlock__value hash">{{ block.nextHash }}</span>
                      </div>
                      <div class="slotInformBlock__item">
                          <span class="slotInformBlock__name">{{ $t("hashes.merkleRoot") }}</span><span class="slotInformBlock__value hash">{{ block.merkleRootHash }}</span>
                      </div>
                  </div>
              </div>
          </div>
      </div>
      <transaction-list
        :transactions="block.transactions"
        :timestamp="block.since/1000"
        v-if="block.transactions.length"
      />
  </div>
</template>

<script>
import { mapGetters, mapActions } from 'vuex'
import DscpFormat from '@/components/DscpFormat'
import TransactionList from '@/components/TransactionList'

export default {
  name: 'Block',
  i18nOptions: {
    keyPrefix: 'block'
  },
  components: { DscpFormat, TransactionList },
  mounted () {
    this.getBlock(this.$route.params.hash)
  },
  beforeRouteUpdate (to, from, next) {
    this.getBlock(to.params.hash)
    next()
  },
  computed: {
    ...mapGetters([
      'block',
      'blockLoaded'
    ])
  },
  methods: {
    ...mapActions([
      'getBlock'
    ])
  }
}
</script>
