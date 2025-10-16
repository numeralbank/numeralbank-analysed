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
    url='https://github.com/numeralbank/numeralbank-analysed',
    py_modules=['lexibank_numeralbank_analysed'],
    packages=find_packages(where='.'),
    include_package_data=True,
    zip_safe=False,
    entry_points={
        'cldfbench.dataset': [
            'lexibank-analysed=lexibank_numeralbank_analysed:Dataset',
        ],
    },
    platforms='any',
    python_requires='>=3.6',
    install_requires=[
        'cldfbench>=1.14.0',
        'cltoolkit>=0.1.1',
        'cldfzenodo>=2.1.1',
        'pylexibank>=3.5.0',
        'attrs>=23.1.0',
        'clldutils>=3.22.2',
        'cldfcatalog>=1.5.1',
        'csvw>=3.3.0',
        'pycldf>=1.38.0',
        'lingpy>=2.6.13',
        'pyclts>=3.2.0',
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
