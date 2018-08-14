<template>
  <div>
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
