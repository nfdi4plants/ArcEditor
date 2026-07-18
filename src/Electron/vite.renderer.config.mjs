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
        // Every npm dependency the renderer's Fable output imports (Node-only
        // main/preload deps excluded). Discovering one of these mid-session -
        // through a lazily imported page or a rarely rendered component -
        // makes vite re-optimize and force-reload the window, throwing the
        // app back to its start screen and rejecting in-flight lazy chunks.
        include: [
            '@dnd-kit/core',
            '@dnd-kit/sortable',
            '@dnd-kit/utilities',
            '@floating-ui/react',
            '@nfdi4plants/exceljs',
            '@tanstack/react-virtual',
            '@uidotdev/usehooks',
            '@uiw/react-md-editor',
            '@uiw/react-markdown-preview',
            'mermaid',
            'rehype-rewrite',
            'rehype-sanitize',
        ],
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