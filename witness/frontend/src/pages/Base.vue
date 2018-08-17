<template>
  <div class="wrapper">
    <div class="preLoader"><img class="preLoader__img" src="../assets/img/loading.svg" alt="..."></div>
    <the-header/>
    <router-view/>
    <the-footer/>
  </div>
</template>

<script>
import TheHeader from '@/components/TheHeader'
import TheFooter from '@/components/TheFooter'

export default {
  name: 'Base',
  components: { TheHeader, TheFooter }
}
</script>
