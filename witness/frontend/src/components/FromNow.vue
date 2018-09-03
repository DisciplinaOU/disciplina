<script>
export default {
  template: '<span>{{ this.$moment(dt).fromNow() }}</span>',
  name: 'FromNow',
  props: {
    dt: {
      type: Number,
      required: true
    }
  },
  created () {
    this.$eventHub.$on('language-changed', this.updateDateTime)
  },
  beforeDestroy () {
    this.$eventHub.$off('language-changed')
  },
  methods: {
    updateDateTime () {
      this.$forceUpdate()
    }
  }
}
</script>
