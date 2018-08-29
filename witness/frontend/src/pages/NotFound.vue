<template>
  <div class="notFound">
    <div class="container">
      <h3 class="notFound__title blockTitle marginBottom15 marginTop30">{{ $t("title") }}:</h3>
      <p class="notFound__text text-center marginBottom30">{{ $route.query.query }}</p>
      <p class="notFound__button text-center marginBottom30">
        <router-link to="/" class="btn btn--blue-white btn--arrow btn--forward">{{ $t("back") }}</router-link>
      </p>
    </div>
  </div>
</template>

<script>
export default {
  name: 'NotFound',
  i18nOptions: {
    keyPrefix: 'notFound'
  }
}
</script>
