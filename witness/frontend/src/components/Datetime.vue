<script>
export default {
  template: '<span>{{ new Date(dt) | moment("LLL") }}</span>',
  name: 'Datetime',
  props: {
    dt: {
      type: Number,
      required: true
    }
  },
  created () {
    this.$eventHub.$on('language-changed', this.updateDateTime)
  },
  methods: {
    updateDateTime () {
      this.$forceUpdate()
    }
  }
}
</script>
