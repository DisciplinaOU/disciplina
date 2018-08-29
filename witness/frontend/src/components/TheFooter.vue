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
                <p class="footer__p">{{ $t("address") }}</p>
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
<div class="footer__social">
                        <a class="footer__socbutton" target="_blank" href="https://www.facebook.com/tchmpls.events/">
                            <div class="footer__socbutton-icon footer__socbutton-icon--fb"></div>
                        </a>
                        <a class="footer__socbutton" target="_blank" href="https://twitter.com/tchmpls_events">
                            <div class="footer__socbutton-icon footer__socbutton-icon--tw"></div>
                        </a>
                        <a class="footer__socbutton" target="_blank" href="https://www.weibo.com/DISCIPLINA">
                            <div class="footer__socbutton-icon footer__socbutton-icon--wb"></div>
                        </a>
                        <a class="footer__socbutton" target="_blank" href="https://open.kakao.com/o/gpaA9eN">
                            <div class="footer__socbutton-icon footer__socbutton-icon--kt"></div>
                        </a>
                        <a class="footer__socbutton" target="_blank" href="https://www.reddit.com/user/tchmpls_events">
                            <div class="footer__socbutton-icon footer__socbutton-icon--rd"></div>
                        </a>
                        <a class="footer__socbutton" target="_blank" href="https://www.instagram.com/disciplina.io/">
                            <div class="footer__socbutton-icon footer__socbutton-icon--ig"></div>
                        </a>
                        <a class="footer__socbutton" target="_blank" href="https://t.me/tchmpls">
                            <div class="footer__socbutton-icon footer__socbutton-icon--tg"></div>
                        </a>
                        <a class="footer__socbutton" target="_blank" href="https://www.linkedin.com/company/disciplina-blockchain/">
                            <div class="footer__socbutton-icon footer__socbutton-icon--li"></div>
                        </a>
                        <a class="footer__socbutton" target="_blank" href="https://www.youtube.com/c/TCHMPLSEVENTS">
                            <div class="footer__socbutton-icon footer__socbutton-icon--yt"></div>
                        </a>
                        <a class="footer__socbutton" target="_blank" href="https://medium.com/@tchmpls_events">
                            <div class="footer__socbutton-icon footer__socbutton-icon--md"></div>
                        </a>
                        <a class="footer__socbutton" target="_blank" href="https://bitcointalk.org/index.php?topic=2325715">
                            <div class="footer__socbutton-icon footer__socbutton-icon--bt"></div>
                        </a>
                        <a class="footer__socbutton" target="_blank" href="https://github.com/DisciplinaOU/disciplina">
                            <div class="footer__socbutton-icon footer__socbutton-icon--gh"></div>
                        </a>
                    </div>
<script>
export default {
  name: 'TheFooter',
  i18nOptions: {
    keyPrefix: 'footer'
  },
  data () {
    return {
      socials: [
        { icon: require('../assets/img/soc_fb.svg'), link: 'https://www.facebook.com/tchmpls.events/' },
        { icon: require('../assets/img/soc_tw.svg'), link: 'https://twitter.com/tchmpls_events' },
        { icon: require('../assets/img/wb.svg'), link: 'https://www.weibo.com/DISCIPLINA' },
        { icon: require('../assets/img/kt.svg'), link: 'https://open.kakao.com/o/gpaA9eN' },
        { icon: require('../assets/img/rd.svg'), link: 'https://www.reddit.com/user/tchmpls_events' },
        { icon: require('../assets/img/soc_in.svg'), link: 'https://www.instagram.com/disciplina.io/' },
        { icon: require('../assets/img/soc_tg.svg'), link: 'https://t.me/tchmpls' },
        { icon: require('../assets/img/li.svg'), link: 'https://www.linkedin.com/company/disciplina-blockchain/' },
        { icon: require('../assets/img/yt.svg'), link: 'https://www.youtube.com/c/TCHMPLSEVENTS' },
        { icon: require('../assets/img/md.svg'), link: 'https://medium.com/@tchmpls_events' },
        { icon: require('../assets/img/soc_bc.svg'), link: 'https://bitcointalk.org/index.php?topic=2325715' },
        { icon: require('../assets/img/gh.svg'), link: 'https://github.com/DisciplinaOU/disciplina' }
      ]
    }
  }
}
</script>
