# Frida

Frida is a Scala-based compiler toolchain designed for the exploration of DHLS-RAs, described in this [paper](https://doi.org/10.1145/3706628.3708880). It uses [Dynamatic](https://github.com/EPFL-LAP/dynamatic) as a frontend Dynamically Scheduled High-Level Synthesis compiler and [VPR](https://docs.verilogtorouting.org/en/latest/vpr/) as back end for FPGA-like placement and routing.

## Building from source
### Install Scala
Frida is developped in Scala. Scala can be installed from [here](https://www.scala-sbt.org/download.html).

### Gurobi
We use Gurobi as an ILP solver in the toolchain. You can get a free academic licence [here](https://www.gurobi.com/academia/academic-program-and-licenses/). The `env.env` at the root of the repository expects the gurobi license to be placed in `submodule/gurobi/gurobi.lic`.

### Package Dependencies
On distributions using the apt package manager, run:

```sh
    apt-get update
    apt-get install clang lld ccache cmake ninja-build python3 openjdk-21-jdk graphviz libboost-regex-dev git curl gzip libreadline-dev bison flex libgtk-3-dev libx11-dev tcl-dev libreadline-dev sphinx-common graphviz-dev libboost-regex-dev
```

The following python packages are also required (available in submodules/vtr-verilog-to-routing/requirements.txt):
- prettytable
- lxm
- psutil
- black==20.8b1
- pylin==2.7.4

### Building
This project uses git submodules to track project dependencies. After cloning the repository run:

```sh
git submodule update --init --recursive
```

The building of project dependencies is automated in the build.sh script.
```sh
./build.sh
```
Finally, the env.env file provides the necessary environment variable to run the compiler.

```sh
source env.env
```

## Toy Example
All project commands can be run from the sbt shell.

```sh
sbt
```

For example, to map the vector_rescale benchmark on top of the architecture presented in the paper, run:
```sh
dot2blif/runMain frontend.apply bench arch=decoupled dot=vector_rescale cp=10.0 mode=all disp=on parallel
```

All generated files are placed within the `build` folder. In particular, the above command will generate the `build/decoupled/Default/vector_rescale/` folder. It will itself contain:
- `base/asts/`: The Abstract Syntax Trees (ASTs) representing the architecture clusters.
- `base/lowering/`: Various dump of the circuit at different stages in the compiler, in dot format, before buffer placement.
- `base/rrgs/`: The Routing Ressource Graph (RRG) corresponding to each architecture cluster.
- `base/PackCache/`: A cache for the packing results.
- `base/mlir/`: contains the mlir outputs of the Dynamatic frontend.
- `base/seed_1_cp_10.00/`: all files related to the run with the default seed (1) and with the given buffer placement critical path objective.
- `base/seed_1_cp_10.00/csv/`: all performance reports in csv format.
- `base/seed_1_cp_10.00/lowering/`: Various dump of the circuit at different stages in the compiler, in dot format, after buffer placement.
- `base/seed_1_cp_10.00/vpr/`: Contains all the files outputed by the VPR backend.
