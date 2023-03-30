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
    lib: {
      entry: "src/app-main.ts",
      formats: ["es"],
    },
    rollupOptions: {
      external: /^lit/,
    },
  },
  define: {
    __API_URL__: process.env.API_URL || JSON.stringify("/api"),
  },
});
