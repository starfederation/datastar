import { defineConfig } from 'vite'

// https://vitejs.dev/config/
export default defineConfig({
  build: {
    outDir: '../static/webcomponents',
    lib: {
      entry: 'src/ds-starfield.ts',
      name: 'datastar',
      fileName: 'datastar',
      formats: ['es'],
    },
    sourcemap: true,
  },
})
