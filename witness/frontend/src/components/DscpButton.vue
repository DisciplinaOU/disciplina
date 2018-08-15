<template>
  <p class="transactionInformBlock__totalSent btn btn--blue">
    {{ wholePart }}
    <span class="small" v-if="realPart">.{{ realPart }}</span> DSCP
  </p>
</template>

<script>
export default {
  name: 'DscpButton',
  props: ['value'],
  computed: {
    wholePart () {
      return this.splitted[0].replace(/\B(?=(\d{3})+(?!\d))/g, ',')
    },
    realPart () {
      return this.splitted[1]
    },
    splitted () {
      return this.value.toString().split('.')
    }
  }
}
</script>
