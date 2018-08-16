<template>
  <div class="wrapper">
    <div class="page">
      <language-selector/>
      <div class="container">
        <div class="logo">
          <img class="logo__img" src="../assets/img/disc_logo.svg" alt="Disciplina"><span class="logo__text">{{ $t("faucet") }}</span>
        </div>
        <div class="pageLegend">
          <p class="pageLegend__text">{{ $t("welcomeMsg") }}</p>
        </div>
        <div class="formBox">
          <form action="#" v-on:submit.prevent="transferTokens">
            <div class="mainForm">
              <input type="text" id="js-search" class="mainForm__input" v-model="address" :placeholder="$t('walletPlaceholder')" @change="walletInputChanged = true">
              <button class="mainForm__send" id="js-send">{{ $t("getTokensBtn") }}</button>
            </div>
          </form>
        </div>
        <div class="checkResult" id="js-result">
          <b class="checkResult__title" v-if="transfer.amount">{{transfer.amount}} DSCP <span>{{ $t("transfer.amountMsg") }}</span></b>
          <p class="checkResult__text" v-if="transfer.txId">{{ $t("transfer.transactionId") }}: {{transfer.txId}}</p>
          <b class="checkResult__title color-alert" v-if="error">{{ $t(`transfer.errors.${error}`) }}</b>
        </div>
        <div class="pageHelper">
          <p class="pageHelper__nowallet">{{ $t("noWallet") }}</p>
          <a href="#" class="pageHelper__btn btn btn--green" @click.prevent="generateWallet">{{ $t("generateWalletBtn") }}</a>
          <p class="pageHelper__more">{{ $t("informationNote") }} <a class="link" href="#">{{ $t("informationLinkText") }}</a></p>
        </div>
      </div>
      <modal
        :publicKey="wallet.publicKey"
        :secretKey="wallet.secretKey"
        :address="wallet.address"
        v-if="showWalletModal"
        @close="showWalletModal = false"
      />
    </div>
    <background/>
    <the-footer/>
</div>
</template>

<script>
import Modal from '@/components/Modal'
import Background from '@/components/Background'
import TheFooter from '@/components/TheFooter'
import LanguageSelector from '@/components/LanguageSelector'

export default {
  components: { Modal, Background, TheFooter, LanguageSelector },
  name: 'Index',
  data: function () {
    return {
      transfer: {
        amount: '',
        txId: ''
      },
      error: '',
      wallet: {
        publicKey: '',
        secretKey: '',
        address: ''
      },
      address: '',
      showWalletModal: false,
      walletInputChanged: false
    }
  },
  methods: {
    transferTokens: function (event) {
      if (!this.walletInputChanged) {
        return false
      }

      this.error = ''

      this.transfer = {}
      this.$axios
        .post('/transfer', {
          destination: this.address
        })
        .then(response => (
          this.transfer = response.data
        ))
        .catch(error => (
          this.error = error.response.data.error
        ))
        .then(response => (
          this.walletInputChanged = false
        ))
    },
    generateWallet: function (event) {
      this.$axios
        .post('/keygen', {})
        .then(response => {
          this.wallet = response.data
          this.showWalletModal = true
          return true
        })
        .catch(error => (
          this.error = error.response.data.error
        ))
    }
  }
}
</script>
