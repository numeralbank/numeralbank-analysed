from setuptools import setup, find_packages


setup(
    name='numeralbank_analysed',
    version='0.1.0.dev0',
    description='',
    author='',
    author_email='',
    long_description=open('README.md').read(),
    long_description_content_type='text/markdown',
    keywords='',
    license='MIT',
    url='https://github.com/lexibank/lexibank-analysed',
    py_modules=['cldfbench_numeralbank_analysed'],
    packages=find_packages(where='.'),
    include_package_data=True,
    zip_safe=False,
    entry_points={
        'cldfbench.dataset': [
            'lexibank-analysed=cldfbench_numeralbank_analysed:Dataset',
        ],
    },
    platforms='any',
    python_requires='>=3.6',
    install_requires=[
        'cldfbench>=1.13.0',
        'cltoolkit>=0.1.1',
        'cldfzenodo>=1.0.0',
        'pylexibank>=3.4.0',
        'attrs>=22.1.0',
        'clldutils>=3.14.0',
        'cldfcatalog>=1.5.1',
        'csvw>=3.1.3',
        'pycldf>=1.34.0',
        'lingpy>=2.6.9',
        'pyclts>=3.1.1',
    ],
    extras_require={
        'dev': ['flake8', 'wheel', 'twine'],
        'test': [
            'pytest>=7.2.0',
            'pytest-mock',
            'pytest-cov',
            'pytest-cldf',
            'coverage',
        ],
    },
    classifiers=[
        'Development Status :: 2 - Pre-Alpha',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: Apache Software License',
        'Natural Language :: English',
        'Operating System :: OS Independent',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.6',
        'Programming Language :: Python :: 3.7',
        'Programming Language :: Python :: 3.8',
        'Programming Language :: Python :: 3.9',
        'Programming Language :: Python :: Implementation :: CPython',
        'Programming Language :: Python :: Implementation :: PyPy'
    ],
)
