// The renderer must never touch the real filesystem - ARC data arrives over
// IPC and disk access belongs to the main process. ProcessCore 0.1.2's
// WorkspaceProject.fs imports these node:fs functions at module scope
// (reached through ARC.fs), which aborted the whole renderer at boot under
// vite's browser externalization. This shim satisfies the module import and
// fails loudly only if renderer code actually tries to read the filesystem.
const deny =
    (name) =>
    (...args) => {
        throw new Error(
            `node:fs.${name} is unavailable in the ArcEditor renderer (arguments: ${JSON.stringify(args)}). ` +
                'Filesystem access belongs to the main process.',
        );
    };

export const readdirSync = deny('readdirSync');
export const lstatSync = deny('lstatSync');
export const existsSync = deny('existsSync');
export const readFileSync = deny('readFileSync');

export default { readdirSync, lstatSync, existsSync, readFileSync };
