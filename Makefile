
install:
	pip install -e .
	python -m sicstus_kernel.install

clean:
	pip uninstall sicstus_kernel
	jupyter kernelspec remove sicstus_kernel
