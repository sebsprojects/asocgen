## asocgen: Overview
asocgen is a small, personal project that I use to explore **finite groups** and **programming in plain old C**. In this project groups are (mostly) treated as **black-box objects**, defined only by their multiplication table. The asocgen library is setup to handle groups **of order up to 65534** (that is 0xfffe; thus elements fitting into a 2-byte word).

The main features of asocgen include:
* **Construction** of well-known families of groups such as the symmetric groups or cyclic groups.
* **Discovery and analysis** of subgroups of larger groups such as S7.
* **Computing information** about groups (treated as black-box-objects) such as order, whether a groups is commutative or simple or computing (minimal) generating sets.
* **Testing for isomorphism** using an algorithm that uses minimal generating sets. This slightly improves on naive approach but the runtime is still infeasable for groups of higher order.
* **Storing and reading groups** via `.txt` files containing the full group multiplication table.

## Compile
All code is written in **plain C** using a subset of the **C99** features. For compilation it is recommended to use the **gcc compiler** on a **UNIX** system. In addition to **stdlib** the following **external dependencies** are required:
* [`elfclib`](https://lab.elfeck.com/seb/elfclib), a small C library containing generic utility and data structures.
* [`dirent.h`](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/dirent.h.html), POSIX header to read directory contents.

To **compile** asocgen do the following:
* Adapt the paths in lines 2 and 3 of the [`MAKEFILE`](https://lab.elfeck.com/seb/asocgen/src/master/Makefile) to include and link `elfclib`.
* Call `sh compile.sh` or alternatively call `make` with the appropriate command-line arguments manually.

## Application and Run
In the current state of the project there exists **no general asocgen application**. Instead `main.c` is used **to program** whatever application of the asocgen library is needed. By compiling, an executable `bin/asoc` is then produced executing this code.

This may change in the (near) future and asocgen may come with an executable and perhaps an interactive interface to provide easier access to the library functions.
