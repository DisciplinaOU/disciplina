<template>
  <span>
    {{ wholePart }}
    <span class="small" v-if="realPart">.{{ realPart }}</span> DSCP
  </span>
</template>

<script>
export default {
  name: 'DscpFormat',
  props: ['value'],
  computed: {
    wholePart () {
      return this.splitted[0].replace(/\B(?=(\d{3})+(?!\d))/g, ',')
    },
    realPart () {
      return this.splitted[1]
    },
    splitted () {
      return this.value.toString().split('.') || []
    }
  }
}
</script>
