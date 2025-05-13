/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./index.html", "./src/**/*.res.mjs", "./src/**/*.jsx"],
  theme: {
    extend: {
      fontFamily: {
        display: ["Grenze Gotisch", "system-ui"],
        sans: ["Inter", "system-ui"],
      },
    },
  },
  plugins: [],
};
