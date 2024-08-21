const defaultTheme = require('tailwindcss/defaultTheme');
const colors = require('tailwindcss/colors')

module.
  exports = {
  content: ['./src/**/*.lisp'],
  darkMode: ['variant', [
    '@media (prefers-color-scheme: dark) { &:not(.light *) }',
    '&:is(.dark *)',
  ]], 
  theme: {
    extend: {
      colors: {
        white: colors.white,
        primary: colors.rose,
        secondary: colors.yellow,
        neutral: colors.slate,
        blue: colors.sky,
      },
    } 
  },
  plugins: [
    require('@tailwindcss/forms'),
  ],
}
