<template>
  <div class="selectBox">
    <div class="input">en</div>
    <div class="options">
      <a class="option active">EN</a>
      <a class="option">CN</a>
      <a class="option">CN</a>
      <a class="option">CN</a>
    </div>
  </div>
</template>

<script>
export default {
  data: function () {
    return {
      languages: ['en', 'cn'],
      language: 'en'
    }
  },
  name: 'LanguageSelector',
  methods: {
    changeLanguage (lang) {
      this.$i18n.i18next.changeLanguage(lang)
    }
  }
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
    width: 40px;
    position: relative;
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
    width: 65px;
    padding: 8px;
    box-shadow: 0 2px 4px 0 rgba(0, 0, 0, 0.5);
    border-radius:3px;
    margin-top: 10px;

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
