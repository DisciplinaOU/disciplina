<template>
  <div class="wrapper">
    <preloader v-show="showPreloader"/>
    <the-header/>
    <router-view/>
    <the-footer/>
  </div>
</template>

<script>
import TheHeader from '@/components/TheHeader'
import TheFooter from '@/components/TheFooter'
import Preloader from '@/components/Preloader'

export default {
  name: 'Base',
  data () {
    return {
      showPreloader: true
    }
  },
  components: { TheHeader, TheFooter, Preloader },
  beforeRouteUpdate (to, from, next) {
    this.showPreloader = true
    this.hidePreloader()
    next()
  },
  created () {
    this.hidePreloader()
  },
  methods: {
    hidePreloader () {
      setTimeout(() => {
        this.showPreloader = false
      }, 500)
    }
  }
}
</script>
