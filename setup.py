from distutils.core import setup
from sicstus_kernel.kernel import PrologKernel


setup(
	name=PrologKernel.kernel_name,
	version="0.0.0",
	packages=[PrologKernel.kernel_name],
	description="Jupyter kernel for " + PrologKernel.prolog_implementation_name,
	author="Anne Brecklinghaus",
	install_requires=[
		"jupyter_client",
		"IPython",
		"ipykernel",
	] + PrologKernel.additional_package_requirements,
	classifiers=[
		"Intended Audience :: Developers",
		"Programming Language :: Python :: 3",
	],
)
