<template>
  <div class="modalLayer">
    <div class="modalLayer__modal">
      <a class="closeModal" @click="$emit('close')">×</a>
      <div class="modalLayer__title">{{ $t("title") }}:</div>
      <div class="modalLayer__content modalContent">
        <div class="modalContent__name">{{ $t("address") }}:</div>
        <div class="modalContent__hash hash">{{ address }}</div>
        <div class="modalContent__button btn btn--blue-white" :class="{ 'btn--blue-white--active' : addressCopied }" @click="copyKey('address')">{{ $t(addressCopied ? "copied" : "copy") }}</div>
      </div>
      <div class="modalLayer__content modalContent">
        <div class="modalContent__name">{{ $t("publicKey") }}:</div>
        <div class="modalContent__hash hash">{{ publicKey }}</div>
        <div class="modalContent__button btn btn--blue-white" :class="{ 'btn--blue-white--active' : publicKeyCopied }" @click="copyKey('publicKey')">{{ $t(publicKeyCopied ? "copied" : "copy") }}</div>
      </div>
      <div class="modalLayer__content modalContent">
        <div class="modalContent__name">{{ $t("secretKey") }}:</div>
        <div class="modalContent__hash hash">{{ secretKey }}</div>
        <div class="modalContent__button btn btn--blue-white" :class="{ 'btn--blue-white--active' : secretKeyCopied }" @click="copyKey('secretKey')">{{ $t(secretKeyCopied ? "copied" : "copy") }}</div>
      </div>
    </div>
  </div>
</template>

<script>
import Vue from 'vue'
import VueClipboard from 'vue-clipboard2'

Vue.use(VueClipboard)

export default {
  i18nOptions: {
    keyPrefix: 'walletModal'
  },
  props: ['address', 'publicKey', 'secretKey'],
  data: function () {
    return {
      addressCopied: false,
      publicKeyCopied: false,
      secretKeyCopied: false
    }
  },
  name: 'Modal',
  methods: {
    copyKey: function (key) {
      let self = this
      self.$copyText(self[key])
      self[`${key}Copied`] = true
      setTimeout(function () {
        self[`${key}Copied`] = false
      }, 3000)
    }
  }
}
</script>
