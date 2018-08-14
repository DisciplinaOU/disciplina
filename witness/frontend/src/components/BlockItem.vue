<template>
  <div class="pseudoTable__row">
      <div class="pseudoTable__cell cell cell__epoch">{{ block.hash }}</div>
      <div class="pseudoTable__cell cell cell__slot"><a class="link link--colorBlue">1653</a></div>
      <div class="pseudoTable__cell cell cell__since"> 2&nbsp;hours 7&nbsp;minutes </div>
      <div class="pseudoTable__cell cell cell__transactions">0</div>
      <div class="pseudoTable__cell cell cell__totalSent">0</div>
      <div class="pseudoTable__cell cell cell__slotLeader">874390ff</div>
      <div class="pseudoTable__cell cell cell__size">9843</div>
  </div>
</template>

<script>
export default {
  name: 'BlockItem',
  props: ['block']
}
</script>
