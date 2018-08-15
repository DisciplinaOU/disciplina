<template>
  <div class="transactionInformBlock transactionInformBlock--educator">
      <div class="transactionInformBlock__title">
          <p class="transactionInformBlock__hash hash">
            <router-link :to="{ name: 'transactionShow', params: {hash: transaction.txId} }" class="link link--colorBlue">{{ transaction.txId }}</router-link>
          </p>
          <p class="transactionInformBlock__date">07/16/2018 09:33:11</p>
      </div>
      <div class="transactionInformBlock__fromTo">
          <div class="transactionInformBlock__from informBlock">
              <p class="informBlock__title">From</p>
              <p class="informBlock__hash hash">{{ transaction.publication.author }}</p>
          </div>
          <div class="transactionInformBlock__to informBlock">
              <p class="informBlock__title">To</p>
              <p class="informBlock__hash hash">{{ transaction.publication.block }}</p>
          </div>
      </div>
  </div>
</template>

<script>
export default {
  name: 'TransactionItemEducator',
  props: {
    transaction: Object
  }
}
</script>
