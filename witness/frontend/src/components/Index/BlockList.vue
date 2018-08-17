<template>
  <div>
    <div class="pseudoTableWrapper">
      <div class="pseudoTable">
        <div class="pseudoTable__row pseudoTable__row--caption">
            <div class="pseudoTable__cell cell cell__block caption">Difficulty</div>
            <div class="pseudoTable__cell cell cell__since caption">Since</div>
            <div class="pseudoTable__cell cell cell__transactions caption">Transaction</div>
            <div class="pseudoTable__cell cell cell__totalSent caption">Total sent</div>
            <div class="pseudoTable__cell cell cell__slotLeader caption">Block leader</div>
            <div class="pseudoTable__cell cell cell__size caption">Size (bytes)</div>
        </div>
        <block-item v-for="block in blocks" :key="block.hash" :block="block"/>
      </div>
    </div>
    <div class="paginationBlock marginTop40">
        <div class="paginationBlock__btnForward btn btn--arrow btn--forward" :class="{ 'btn--gray': currentPage == 1 }" @click="fetchBlocks(fromBlockHashPrev, currentPage - 1)">Newer blocks</div>
        <div class="paginationBlock__pagination">Page <span class="paginationBlock__page">{{ currentPage }}</span> of&nbsp;<span class="paginationBlock__allPage">{{ totalPages }}</span></div>
        <div class="paginationBlock__btnBack btn btn--arrow btn--back" :class="{ 'btn--gray': currentPage == totalPages }" @click="fetchBlocks(fromBlockHash, currentPage + 1)">Older blocks</div>
    </div>
  </div>
</template>

<script>
import { mapGetters, mapActions } from 'vuex'
import BlockItem from './BlockItem'

export default {
  name: 'BlockList',
  components: { BlockItem },
  mounted () {
    if (!this.blocks.length) {
      return this.getAllBlocks()
    }
  },
  computed: {
    ...mapGetters([
      'blocks',
      'totalPages',
      'perPage',
      'fromBlockHash',
      'fromBlockHashPrev',
      'currentPage'
    ])
  },
  methods: {
    ...mapActions([
      'getAllBlocks',
      'getTotalPages',
      'setPage'
    ]),
    fetchBlocks (hash, page) {
      if (page < 1 || page > this.totalPages) {
        return false
      }

      this.getAllBlocks(hash, page)
      this.$store.commit('setPage', page)
    }
  }
}
</script>
