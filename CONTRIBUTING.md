# Local Setup

-   [Requirements](#requirements)
-   [Setup](#setup)
-   [Start](#start)
    -   [Available commands](#available-commands)
-   [Contribute](#contribute)

## Requirements

-   [.NET SDK](https://dotnet.microsoft.com/en-us/download), >= 8.0.0
    -   verify with `dotnet --version`
-   [nodejs](https://nodejs.org/en/download), >=18
    -   verify with `node --version`
-   npm, >=9
    -   likely part of nodejs installation
    -   verify with `npm --version`
-   Any F# IDE, e.g. [Visual Studio Code](https://code.visualstudio.com/) + [Ionide extension](https://marketplace.visualstudio.com/items?itemName=Ionide.Ionide-fsharp#:~:text=Ionide-VSCode%20is%20a%20VSCode,powers%20language%20features%20is%20FSAutoComplete.), [Rider](https://www.jetbrains.com/rider/), [Visual Studio](https://visualstudio.microsoft.com/)

## Setup

This needs to be done once per repository download.

1. Clone this repo
2. Run dotnet tool restore
3. Run npm install

### Components playground

`src/Components` is a reusable React component library that can be published to npm and reused by `src/Client` and `src/Electron/src/Renderer`. It contains Storybook/Vitest-based component tests and usage documentation.

To run the Storybook playground for `src/Components`, run `npm run storybook` in the `src/Components` directory.

To run the playground , run `npm start` in `src/Components`. You can edit `src/Components/playground/App.tsx` to test out components in a live environment. This is useful for testing out new components or UI logic without needing to run the entire Client or Electron app.

### Electron app

The Electron app is still in early development and not fully integrated into the main build process. To run the Electron app, navigate to `src/Electron` and run `npm start`. This will start the Electron application with hot-reloading for the renderer process.

## Contribute

If you want to contribute to Swate, open an issue with the feature/bug you want to work on. This way you can ensure that your approach is in line with the project goals and you can get feedback from the maintainers.

Afterwards you can fork the repository and start working on your feature/bug. When you are done, describe your changes in [CHANGELOG.md](CHANGELOG.md) following the [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) format and open a pull request.

We are currently still working on a nice project structure. For now ask us if any questions arise in the related GitHub issue!

Check out the following files for more specific contribution guidelines:
- [docs/Structure.md](docs/Structure.md) for architecture and project structure guidelines.
- [docs/ReactComponentDesign.md](docs/ReactComponentDesign.md) for guidelines on designing reusable React components in `src/Components`.

> [!IMPORTANT]
> Use VS Code or Rider for development, as they allow autoformatting via Fantomas on file save. This ensures that the code style is consistent across the project and reduces the amount of formatting-related comments in pull requests.

## Zen

> The GitHub API consolidates the Zen of GitHub in its own codebase, in 14 aphorisms:

- Responsive is better than fast
- It’s not fully shipped until it’s fast
- Anything added dilutes everything else
- Practicality beats purity
- Approachable is better than simple
- Mind your words, they are important
- Speak like a human
- Half measures are as bad as nothing at all
- Encourage flow
- Non-blocking is better than blocking
- Favor focus over features
- Avoid administrative distraction
- Design for failure
- Keep it logically awesome
