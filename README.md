# StrIoT GUI

Allows for Haskell-based pure functional stream processing in the Node-RED tool, allowing for quick and easy development for the [StrIoT](https://github.com/striot/striot/) system.

Takes output from Node-RED using the nodes added in the [StrIoT Nodes](https://github.com/JonnySpruce/striot-nodes) project, and automatically creates an input to the [StrIoT](https://github.com/striot/striot/) system, which can then generate a pure functional stream processing application.

## Getting Started

### Prerequisites

- [Haskell](https://www.haskell.org/)
- [Stack](https://docs.haskellstack.org/en/stable/README/)

Recommended for generating Node-RED output for the StrIoT system:

- [Node.js](https://nodejs.org/en/)
- [Node-RED](https://nodered.org/)
- [StrIoT Nodes](https://github.com/JonnySpruce/striot-nodes)

### Installation

Firstly, close the repository:

```bash
git clone https://github.com/JonnySpruce/striot-gui.git
```

The project uses the Haskell tool Stack, and so can be built using the command `stack build` in your terminal (first `cd` to the cloned directory).

You can then use `stack install` to add `striot-gui` to your path, if desired.

### Running

StrIoT GUI takes an exported Node-RED project as a JSON file, and converts it into a Haskell program that can be taken as an input by [StrIoT](https://github.com/striot/striot/). _(Note: currently still a work in progress, it currently converts it to a Haskell datatype and outputs this only)_

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

## Testing

The tests can be run locally with the command `stack test`.
