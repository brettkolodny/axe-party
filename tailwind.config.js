/** @type {import('tailwindcss').Config} */
module.exports = {
  content: ["./src/**/*.{js,ts,jsx,tsx,elm}"],
  theme: {
    extend: {
      colors: {
        thunder: {
          DEFAULT: "#3B393B",
          50: "#D4D2D4",
          100: "#CAC8CA",
          200: "#B6B3B6",
          300: "#A29EA2",
          400: "#8E8A8E",
          500: "#797579",
          600: "#656165",
          700: "#504D50",
          800: "#3B393B",
          900: "#1E1D1E",
          950: "#101010",
        },
        "hatchets-green": {
          DEFAULT: "#7A590F",
          50: "#F8E8C3",
          100: "#F5E0B1",
          200: "#F1D28D",
          300: "#ECC469",
          400: "#E8B544",
          500: "#E4A720",
          600: "#C38E18",
          700: "#9E7413",
          800: "#7A590F",
          900: "#483509",
          950: "#2F2206",
        },
      },
    },
  },
  plugins: [],
};
