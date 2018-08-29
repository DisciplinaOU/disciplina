<template>
  <div class="main">
    <div class="notFound">
      <div class="container">
        <h3 class="notFound__title blockTitle marginBottom15 marginTop30">No matching records for:</h3>
        <p class="notFound__text text-center marginBottom30">DdzFFzCqrhssmgBTif5DRb4NCXbz2FsZfEXXBdknGq1CTvovyXq9ECmx5amfjeWrftYEsayi9BCAahJ8dRgrx</p>
        <p class="notFound__button text-center marginBottom30">
          <a class="btn btn--blue-white btn--arrow btn--forward">Return to homepage</a>
        </p>
      </div>
    </div>
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
