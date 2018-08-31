<template>
  <div class="selectBox" :class="{ active: opened }" @click="opened = !opened" v-click-outside="hide">
    <div class="input">{{ currentLanguage }}</div>
    <div class="options" v-show="opened">
      <a class="option"
        v-for="lang in languages"
        :key="lang[0]"
        @click.stop="changeLanguage(lang)"
        :class="{ active: lang[1] == currentLanguage }">
        {{ lang[1] }}
      </a>
    </div>
  </div>
</template>

<script>
import ClickOutside from 'vue-click-outside'

export default {
  data: function () {
    return {
      languages: [
        ['en', 'EN'],
        ['zh', '中文'],
        ['ko', '한국어'],
        ['ja', '日本語']
      ],
      currentLanguage: 'EN',
      opened: false
    }
  },
  name: 'LanguageSelector',
  methods: {
    changeLanguage (lang) {
      this.currentLanguage = lang[1]
      this.opened = false
      this.$i18n.i18next.changeLanguage(lang[0])
    },
    hide () {
      this.opened = false
    }
  },
  directives: { ClickOutside }
}
</script>

<style lang="scss">
  .selectBox {
    width: 60px;
    font-weight: 400;
    color: #FFF;
    text-transform: uppercase;
    position: absolute;
    top: 15px;
    right: 15px;
    display: flex;
    flex-direction: column;
    align-items: flex-end;
    cursor: pointer;
  }

  .selectBox .input {
    padding-right: 15px;
    width: 65px;
    position: relative;
    text-align: right;
  }

  .selectBox .input::after {
    content: '';
    display: inline-block;
    position: absolute;
    top: 10px;
    right: 0;
    border-style: solid;
    border-color: transparent white transparent;
    height: 3px;
    width: 10px;
    border-width: 4px 8px 4px 0;
    transform: rotate(-90deg);
    vertical-align: 3px;
    transition: transform 0.3s ease-in-out;
    transform-origin: center center;
  }

  .selectBox.active .input::after{
    transform: rotate(90deg);
  }

  .selectBox .options {
    width: 66px;
    padding: 8px;
    box-shadow: 0 2px 4px 0 rgba(0, 0, 0, 0.5);
    border-radius:3px;
    margin-top: 10px;
    transition: transform 0.3s ease-in-out;

  }
  .selectBox .option {
    display: block;
    color: white;
    padding-top: 4px;
    transition: color 0.3s ease-in-out;
    cursor: pointer;
    &:first-child {
      padding-top: 0;
    }
    &:hover {
      color: #00efb7;
      text-decoration: none;
    }
  }

  .selectBox .option.active {
    color: #00efb7;
  }
</style>
