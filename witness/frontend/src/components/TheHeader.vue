<template>
  <header class="header">
    <div class="container">
          <language-selector/>
        <div class="titleBlock">
            <div class="titleBlock__logo logo">
              <router-link to='/'>
                <img src="../assets/img/disc_logo.svg" alt="">
              </router-link>
            </div>
            <div class="titleBlock__text titleText">{{ $t("header.title") }}</div>
        </div>
        <div class="formBox marginTop20">
            <search-form/>
        </div>
    </div>
  </header>
</template>

<script>
import SearchForm from '@/components/SearchForm'
import LanguageSelector from '@/components/LanguageSelector'

export default {
  name: 'TheHeader',
  components: { SearchForm, LanguageSelector }
}
</script>
