<template>
  <div class="main">
  <div class="lastSlots">
      <div class="container">
          <h3 class="lastSlots__title blockTitle marginBottom30 marginTop30">Last blocks</h3>
          <block-list/>
          <div class="paginationBlock marginTop40">
              <div class="paginationBlock__btnForward btn btn--forward">Go forward</div>
              <div class="paginationBlock__pagination">Page <span class="paginationBlock__page">12</span> of&nbsp;<span class="paginationBlock__allPage">123</span></div>
              <div class="paginationBlock__btnBack btn btn--back">Go back</div>
          </div>
      </div>
  </div>

  <div class="lastTransactions">
      <div class="container">
          <h3 class="lastTransactions__title blockTitle marginBottom30">Last transactions</h3>
          <div class="pseudoTableWrapper">
              <div class="pseudoTable">
                  <div class="pseudoTable__row">
                      <div class="pseudoTable__cell cell cell__hash hash"><a class="link">6dbd2e88a8d92d8fba2a8c3ad3eba20311cd0903c4afc9455498f7e9a9aa1c31</a></div>
                      <div class="pseudoTable__cell cell cell__time"> 07/16/2018 09:33:11 </div>
                      <div class="pseudoTable__cell cell cell__totalSent">139,638<span class="cell__small small">.430858</span> DSCP</div>
                  </div>
                  <div class="pseudoTable__row">
                      <div class="pseudoTable__cell cell cell__hash hash"><a class="link">6dbd2e88a8d92d8fba2a8c3ad3eba20311cd0903c4afc9455498f7e9a9aa1c31</a></div>
                      <div class="pseudoTable__cell cell cell__time"> 07/16/2018 09:33:11 </div>
                      <div class="pseudoTable__cell cell cell__totalSent">139,638<span class="cell__small small">.430858</span> DSCP</div>
                  </div>
                  <div class="pseudoTable__row">
                      <div class="pseudoTable__cell cell cell__hash hash"><a class="link">6dbd2e88a8d92d8fba2a8c3ad3eba20311cd0903c4afc9455498f7e9a9aa1c31</a></div>
                      <div class="pseudoTable__cell cell cell__time"> 07/16/2018 09:33:11 </div>
                      <div class="pseudoTable__cell cell cell__totalSent">139,638<span class="cell__small small">.430858</span> DSCP</div>
                  </div>
                  <div class="pseudoTable__row">
                      <div class="pseudoTable__cell cell cell__hash hash"><a class="link">6dbd2e88a8d92d8fba2a8c3ad3eba20311cd0903c4afc9455498f7e9a9aa1c31</a></div>
                      <div class="pseudoTable__cell cell cell__time"> 07/16/2018 09:33:11 </div>
                      <div class="pseudoTable__cell cell cell__totalSent">139,638<span class="cell__small small">.430858</span> DSCP</div>
                  </div>
                  <div class="pseudoTable__row">
                      <div class="pseudoTable__cell cell cell__hash hash"><a class="link">6dbd2e88a8d92d8fba2a8c3ad3eba20311cd0903c4afc9455498f7e9a9aa1c31</a></div>
                      <div class="pseudoTable__cell cell cell__time"> 07/16/2018 09:33:11 </div>
                      <div class="pseudoTable__cell cell cell__totalSent">139,638<span class="cell__small small">.430858</span> DSCP</div>
                  </div>
                  <div class="pseudoTable__row">
                      <div class="pseudoTable__cell cell cell__hash hash"><a class="link">6dbd2e88a8d92d8fba2a8c3ad3eba20311cd0903c4afc9455498f7e9a9aa1c31</a></div>
                      <div class="pseudoTable__cell cell cell__time"> 07/16/2018 09:33:11 </div>
                      <div class="pseudoTable__cell cell cell__totalSent">139,638<span class="cell__small small">.430858</span> DSCP</div>
                  </div>
                  <div class="pseudoTable__row">
                      <div class="pseudoTable__cell cell cell__hash hash"><a class="link">6dbd2e88a8d92d8fba2a8c3ad3eba20311cd0903c4afc9455498f7e9a9aa1c31</a></div>
                      <div class="pseudoTable__cell cell cell__time"> 07/16/2018 09:33:11 </div>
                      <div class="pseudoTable__cell cell cell__totalSent">139,638<span class="cell__small small">.430858</span> DSCP</div>
                  </div>
                  <div class="pseudoTable__row">
                      <div class="pseudoTable__cell cell cell__hash hash"><a class="link">6dbd2e88a8d92d8fba2a8c3ad3eba20311cd0903c4afc9455498f7e9a9aa1c31</a></div>
                      <div class="pseudoTable__cell cell cell__time"> 07/16/2018 09:33:11 </div>
                      <div class="pseudoTable__cell cell cell__totalSent">139,638<span class="cell__small small">.430858</span> DSCP</div>
                  </div>
                  <div class="pseudoTable__row">
                      <div class="pseudoTable__cell cell cell__hash hash"><a class="link">6dbd2e88a8d92d8fba2a8c3ad3eba20311cd0903c4afc9455498f7e9a9aa1c31</a></div>
                      <div class="pseudoTable__cell cell cell__time"> 07/16/2018 09:33:11 </div>
                      <div class="pseudoTable__cell cell cell__totalSent">139,638<span class="cell__small small">.430858</span> DSCP</div>
                  </div>
                  <div class="pseudoTable__row">
                      <div class="pseudoTable__cell cell cell__hash hash"><a class="link">6dbd2e88a8d92d8fba2a8c3ad3eba20311cd0903c4afc9455498f7e9a9aa1c31</a></div>
                      <div class="pseudoTable__cell cell cell__time"> 07/16/2018 09:33:11 </div>
                      <div class="pseudoTable__cell cell cell__totalSent">139,638<span class="cell__small small">.430858</span> DSCP</div>
                  </div>
              </div>
          </div>
          <div class="paginationBlock marginTop40">
              <div class="paginationBlock__btnBack btn btn--more">Show more</div>
          </div>
      </div>
  </div>
</div>
</template>

<script>
import BlockList from '@/components/BlockList'

export default {
  name: 'Index',
  components: { BlockList }
}
</script>
