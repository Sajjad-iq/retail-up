<script lang="ts">
  import "./app.css";
  import POSInterface from "./lib/components/pos/POSInterface.svelte";
</script>

<POSInterface />

<style>
  :global(html, body) {
    height: 100%;
    margin: 0;
    padding: 0;
  }

  :global(#app) {
    height: 100%;
  }
</style>
