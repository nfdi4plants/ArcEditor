import { defineConfig } from 'vite';
import react from '@vitejs/plugin-react';
import tailwindcss from '@tailwindcss/vite';

// https://vitejs.dev/config
export default defineConfig({
    plugins: [
        react({
            include: /\.(js|jsx|ts|tsx)$/,
        }),
        tailwindcss()
    ],
    optimizeDeps: {
        // Dependencies only reached through lazily imported pages (the
        // provenance table editor). Without pre-bundling, vite discovers them
        // on the first lazy import, re-optimizes, and force-reloads the page,
        // throwing the app back to its start screen.
        include: ['@dnd-kit/core', '@uidotdev/usehooks'],
    },
    server: {
        watch: {
            // Ignore raw F# source and non-renderer generated outputs to avoid unnecessary full reloads.
            ignored: (watchPath) => {
                const p = watchPath.replace(/\\/g, '/');

                const isFSharpSource =
                    p.endsWith('.fs') ||
                    p.endsWith('.fsx') ||
                    p.endsWith('.fsi') ||
                    p.endsWith('.fsproj');

                const isMainOrPreloadOutput =
                    p.includes('/src/fable_output/Main/') ||
                    p.includes('/src/fable_output/Preload/');

                return isFSharpSource || isMainOrPreloadOutput;
            },
            awaitWriteFinish: {
                stabilityThreshold: 150,
                pollInterval: 25,
            },
        },
    }
});