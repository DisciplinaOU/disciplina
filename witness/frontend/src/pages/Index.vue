<template>
  <div class="main ">
  <div class="lastSlots">
      <div class="container">
          <h3 class="lastSlots__title blockTitle marginBottom30 marginTop30">{{ $t("blocks") }}</h3>
          <block-list/>
      </div>
  </div>

  <div class="lastTransactions">
      <div class="container">
          <h3 class="lastTransactions__title blockTitle marginBottom30">{{ $t("transactions") }}</h3>
          <transaction-list/>
      </div>
  </div>
</div>
</template>

<script>
import BlockList from '@/components/Index/BlockList'
import TransactionList from '@/components/Index/TransactionList'

export default {
  name: 'Index',
  i18nOptions: {
    keyPrefix: 'index'
  },
  components: { BlockList, TransactionList }
}
</script>
