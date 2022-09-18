import { defineConfig } from 'vite'
import { svelte } from '@sveltejs/vite-plugin-svelte'

const wcs = [
  "./src/lib/Counter2.svelte", "./src/lib/Clock.svelte",
  "./src/lib/Slider.svelte"
]

export default defineConfig({
  plugins: [
    svelte({
      exclude: wcs,
    }),
    svelte({
      include: wcs,
      compilerOptions: { customElement: true }
    })
  ]
})
