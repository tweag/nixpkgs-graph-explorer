import { defineConfig } from "vite";

// https://vitejs.dev/config/
export default defineConfig({
  worker: {
    format: "es",
  },
  server: {
    proxy: {
      "/api": {
        target: "http://localhost:5000",
        changeOrigin: true,
        rewrite: (path) => path.replace(/^\/api/, ""),
      },
    },
  },
  build: {
    rollupOptions: {
      output: {
        manualChunks(id) {
          if (
            id.includes("node_modules/lit") ||
            id.includes("node_modules/@lit")
          )
            return "lit";
          if (id.includes("node_modules/@shoelace-style")) return "shoelance";
          if (
            id.includes("node_modules/cytoscape") ||
            id.includes("node_modules/dagre")
          )
            return "cytoscape";
        },
      },
    },
  },
  define: {
    __API_URL__: process.env.API_URL || JSON.stringify("/api"),
  },
});
