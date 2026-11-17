import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

// https://vite.dev/config/
export default defineConfig({
  plugins: [react()],
  server: {
    port: 5173,
    proxy: {
      // Proxy /v1/* to PhaseDB server during development
      '/v1': {
        target: 'http://127.0.0.1:8484',
        changeOrigin: true,
      },
    },
  },
})
