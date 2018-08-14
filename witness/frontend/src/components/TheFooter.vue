<template>
  <footer class="footer">
    <div class="container">
        <div class="footer__table">
            <div class="footer__info footerInfo">
                <p class="footer__p">
                    <a href="tel:+3728803644" target="__blank" class="footer_a link">+372 880 3644</a>
                </p>
                <p class="footer__p">
                    <a class="footer_a link" href="mailto:offers@disciplina.io">offers@disciplina.io</a>
                </p>
            </div>
            <span class="footer__sep"></span>
            <div class="footer__address footerAddres">
                <p class="footer__p">Narva mnt 7-634, 10117, Kesklinna linnaosa, Tallinn, Estonia;</p>
                <p class="footer__p">2018 &copy; Disciplina OÜ</p>
            </div>
            <span class="footer__sep"></span>
            <div class="footer__soc socials">
            <ul class="socials__list">
              <li class="socials__item" v-for="social in socials" v-bind:key="social.icon">
                <a :href="social.link" class="socials__link">
                  <img :src="social.icon" class="socials__icn" alt="">
                </a>
              </li>
            </ul>
          </div>
        </div>
    </div>
  </footer>
</template>

<script>
export default {
  name: 'TheFooter',
  i18nOptions: {
    keyPrefix: 'footer'
  },
  data: function () {
    return {
      socials: [
        { icon: require('../assets/img/soc_vk.svg'), link: 'https://vk.com/tchmpls.events' },
        { icon: require('../assets/img/soc_tw.svg'), link: 'https://twitter.com/tchmpls_events' },
        { icon: require('../assets/img/soc_tg.svg'), link: 'https://t.me/tchmpls' },
        { icon: require('../assets/img/soc_m.svg'), link: 'https://medium.com/@tchmpls_events' },
        { icon: require('../assets/img/soc_in.svg'), link: 'https://www.instagram.com/tchmpls.events/' },
        { icon: require('../assets/img/soc_fb.svg'), link: 'https://www.facebook.com/tchmpls.events/' },
        { icon: require('../assets/img/soc_bc.svg'), link: 'https://bitcointalk.org/index.php?topic=2325715' }
      ]
    }
  }
}
</script>
