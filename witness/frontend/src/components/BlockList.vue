<template>
  <div class="pseudoTableWrapper">
      <div class="pseudoTable">
      <div class="pseudoTable__row pseudoTable__row--caption">
          <div class="pseudoTable__cell cell cell__epoch caption">Epoch</div>
          <div class="pseudoTable__cell cell cell__slot caption">Slot</div>
          <div class="pseudoTable__cell cell cell__since caption">Since</div>
          <div class="pseudoTable__cell cell cell__transactions caption">Transaction</div>
          <div class="pseudoTable__cell cell cell__totalSent caption">Total sent</div>
          <div class="pseudoTable__cell cell cell__slotLeader caption">Slot leader</div>
          <div class="pseudoTable__cell cell cell__size caption">Size (bytes)</div>
      </div>
      <block-item v-for="block in blocks" :key="block.hash" :block="block"/>
  </div>
  </div>
</template>

<script>
import { mapGetters, mapActions } from 'vuex'
import BlockItem from '@/components/BlockItem'

export default {
  name: 'BlockList',
  components: { BlockItem },
  mounted () {
    return this.getAllBlocks()
  },
  computed: {
    ...mapGetters([
      'blocks'
    ])
  },
  methods: {
    ...mapActions([
      'getAllBlocks'
    ])
  }
}
</script>
