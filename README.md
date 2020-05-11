# StrIoT GUI

Allows for Haskell-based pure functional stream processing in the Node-RED tool, utilising the [StrIoT](https://github.com/striot/striot/) system to generate stream processing programs from Node-RED exports.

With the nodes added in the [StrIoT Nodes](https://github.com/JonnySpruce/striot-nodes) package, you can define pure functional stream processing using the Node-RED interface. This tool then takes the export from Node-RED and generates the full code for the application. 

Once installed, using the program is as simple as running `striot-gui-exe /path/to/your/node-red/export` in the terminal.

**Features:**

- Use just one command to generate stream processing applications from a Node-RED export.
- Supports all nodes added by the [StrIoT Nodes](https://github.com/JonnySpruce/striot-nodes) project.
- Utilises the [StrIoT](https://github.com/striot/striot/) system for program generation.

## Getting Started

### Prerequisites

- [Haskell](https://www.haskell.org/)
- [Stack](https://docs.haskellstack.org/en/stable/README/)

Recommended for generating Node-RED output for the StrIoT system:

- [Node.js](https://nodejs.org/en/)
- [Node-RED](https://nodered.org/)
- [StrIoT Nodes](https://github.com/JonnySpruce/striot-nodes)

### Installation

Firstly, clone the repository:

```bash
git clone https://github.com/JonnySpruce/striot-gui.git
```

The project uses the Haskell tool Stack, and so can be built using the command `stack build` in your terminal (first `cd` to the cloned directory).

You can then use `stack install` to add `striot-gui` to your path, if desired.

### Running

StrIoT GUI takes an exported Node-RED project as a JSON file, and uses [StrIoT](https://github.com/striot/striot/) to generate the relevant Haskell files and Docker files.
Only nodes added by the [StrIoT Nodes](https://github.com/JonnySpruce/striot-nodes) project are supported, the default nodes in Node-RED should not be used.

If you have added the project to your path (as described in the **Installation** section), simply execute the command by running the following command:

```bash
striot-gui-exe /path/to/your/node-red/export
```

If you have not used `stack install`, you can still run the program from the root directory of the project using the following command:

```bash
stack exec striot-gui-exe /path/to/your/node-red/export
```

**Note:** The path you provide must be an absolute path, relative paths are not currently supported.

There will now be two new directories in your current location - `node1` and `node2`. These directories contain all the code generated by StrIoT.

### Examples

There are also example flows in the [examples](examples) directory, which you can inspect to see how the flow is structured and the code is written. The JSON file was created with the [StrIoT Nodes](https://github.com/JonnySpruce/striot-nodes) package. The examples can be run using the `docker-compose up` command in the directory of the example you want to run (**Note:** [Docker](https://www.docker.com/) must be installed).

## Contributing

All contributions are welcome! Any work on the issues raised in the [issue tracker](https://github.com/JonnySpruce/striot-gui/issues) is always greatly appreciated. Alternatively, if you have a feature that you would like to add or a bug that you'd like to fix which isn't mentioned in the [issue tracker](https://github.com/JonnySpruce/striot-gui/issues), feel free to [create a new issue](https://github.com/JonnySpruce/striot-gui/issues/new/choose) to discuss it.

For any pull request which adds new code, tests should also be included. All tests must pass before code can be merged.

### Testing

The tests are written using [Hspec](https://hspec.github.io/), and can be run locally with the command `stack test`.

## Bugs & Feature Requests

If you're having an issue with the tool or have a feature request to add, please feel free to raise an issue in the [issue tracker](https://github.com/JonnySpruce/striot-gui/issues).
