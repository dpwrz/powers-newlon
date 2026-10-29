import {defineConfig} from 'vite';
import react from '@vitejs/plugin-react';

export default defineConfig({
  plugins:[react()],
  base:'/next/',
  build:{outDir:'../web/public/next',emptyOutDir:true,sourcemap:false},
  server:{port:5174,proxy:{'/data':'http://localhost:5173'}},
});
