import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import svgr from "vite-plugin-svgr";

// https://vitejs.dev/config/
export default defineConfig({
  base: "https://thomaswright.github.io/umpteen-games",
  plugins: [
    svgr(),
    react({
      include: ["**/*.res.mjs"],
    }),
  ],
});
