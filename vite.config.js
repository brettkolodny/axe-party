import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";
import { VitePWA } from "vite-plugin-pwa";

export default defineConfig({
  plugins: [elmPlugin(), VitePWA()],
});
