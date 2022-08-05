
# SICStus Prolog Jupyter Kernel

A [Jupyter](https://jupyter.org/) kernel for [SICStus Prolog](https://sicstus.sics.se/) based on the [IPython kernel](https://github.com/ipython/ipykernel).

Also see the [Jupyter kernel for SWI-Prolog](https://github.com/anbre/swi-prolog-jupyter-kernel) and the [JupyterLab Prolog CodeMirror Extension](https://github.com/anbre/jupyterlab-prolog-codemirror-extension) for syntax highlighting of Prolog code in JupyterLab.

The directory [notebooks](./notebooks) contains a notebook demonstrating the features of the kernel.

**Note:** The project is still under development and so far, only a development installation is possible.
Furthermore, the usage of the kernel is limited. In order to execute Prolog code, the Jupyter kernel needs to communicate with a Prolog server, of which the code can be found in the directory [prolog_server](./prolog_server). The kernel expects this folder to be in the same directory as the folder which contains the Jupyter notebook. If this is not the case, no code can be executed.


## Development Install

1. `git clone https://github.com/anbre/sicstus-prolog-jupyter-kernel`
2. Change to the root directory of the repository
3. `pip install .`
4. Install the kernel specification directory:
    - `python -m sicstus_kernel.install`
    - There are the following options which can be seen when running `python -m sicstus_kernel.install -h`
      - `--user`: install to the per-user kernel registry (default if not root and no prefix is specified)
      - `--sys-prefix`: install to Python's sys.prefix (e.g. virtualenv/conda env)
      - `--prefix PREFIX`: install to the given prefix: PREFIX/share/jupyter/kernels/ (e.g. virtualenv/conda env)


## Uninstall

1. Change to the root directory of the repository
2. `pip uninstall sicstus_kernel`
3. `jupyter kernelspec remove sicstus_kernel`
