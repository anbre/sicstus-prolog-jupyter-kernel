import argparse
import json
import os
import shutil
import sys

from jupyter_client.kernelspec import KernelSpecManager
from IPython.utils.tempdir import TemporaryDirectory
from .kernel import PrologKernel


def get_kernelspec_dir_path():
    """Get the path of the kernelspec directory where the kernel.js and kernel.css files are located"""
    dirname = os.path.dirname(__file__)
    kernelspec_dir_path = os.path.join(dirname, '..', 'kernelspec')
    return kernelspec_dir_path


def install_kernel_spec(user=True, prefix=None):
    # Content for the kernel.json file which is written to the kernel spec directory
    kernel_json = {
        "argv": [
            sys.executable,
            "-m",
            PrologKernel.kernel_name,
            "-f",
            "{connection_file}"
        ],
        "display_name": PrologKernel.prolog_implementation_name,
        "language": "prolog"
    }

    # Create a temporary directory to which kernelspec files are written which are needed for the installation of a Jupyter kernel
    with TemporaryDirectory() as temporary_directory:
        os.chmod(temporary_directory, 0o755) # Starts off as 700, not user readable
        # Create the file kernel.json
        with open(os.path.join(temporary_directory, 'kernel.json'), 'w') as f:
            json.dump(kernel_json, f, sort_keys=True)
        # There is a kernelspec directory from which files can be copied to the temporary directory
        # Retrieve the names of all files and directories in that directory
        kernelspec_dir_path = get_kernelspec_dir_path()
        kernelspec_dir_entries = os.scandir(kernelspec_dir_path)
        kernelspec_dirnames = []
        kernelspec_filenames = []
        for entry in kernelspec_dir_entries:
            if entry.is_dir():
                kernelspec_dirnames.append(entry.name)
            elif entry.is_file():
                kernelspec_filenames.append(entry.name)
        # Copy all the files from the kernelspec directory to the temporary directory
        for name in kernelspec_filenames:
            shutil.copyfile(os.path.join(kernelspec_dir_path, name), os.path.join(temporary_directory, name))

        # Install the kernel from the temporary directory
        print('Installing ' + PrologKernel.prolog_implementation_name + ' kernel spec')
        KernelSpecManager().install_kernel_spec(temporary_directory, PrologKernel.kernel_name, user=user, prefix=prefix)


def _is_root():
    try:
        return os.geteuid() == 0
    except AttributeError:
        # non-Unix platform -> assume not root
        return False


def main(argv=None):
    ap = argparse.ArgumentParser()
    ap.add_argument(
        '--user',
        action='store_true',
        help="install to the per-user kernel registry (default if not root and no prefix is specified)")
    ap.add_argument(
        '--sys-prefix',
        action='store_true',
        help="install to Python's sys.prefix (e.g. virtualenv/conda env)")
    ap.add_argument(
        '--prefix',
        help="install to the given prefix: PREFIX/share/jupyter/kernels/ (e.g. virtualenv/conda env)")
    args = ap.parse_args(argv)

    if args.sys_prefix:
        args.prefix = sys.prefix
    if not args.prefix and not _is_root():
        args.user = True

    install_kernel_spec(args.user, args.prefix)


if __name__ == '__main__':
    main()
