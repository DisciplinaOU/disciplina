<template>
  <form action="#" v-on:submit.prevent="searchHash">
      <div class="mainForm">
          <input type="text" class="mainForm__input" v-model="query" placeholder="Search addresses, transactions, epochs & slots on the DISCIPLINA network">
          <button class="mainForm__send">Search</button>
      </div>
  </form>
</template>

<script>
export default {
  name: 'SearchForm',
  data () {
    return {
      query: ''
    }
  },
  methods: {
    searchHash () {
      return true
    }
  }
}
</script>
