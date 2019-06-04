# SAFE - Surrogate Assisted Feature Extraction

[![Build Status](https://travis-ci.org/MI2DataLab/SAFE.svg?branch=master)](https://travis-ci.org/MI2DataLab/SAFE)
[![Coverage Status](https://codecov.io/gh/MI2DataLab/SAFE/branch/master/graph/badge.svg)](https://codecov.io/gh/MI2DataLab/SAFE)

The `SAFE` package is a model agnostic tool for making an interpretable white-box model more accurate using alternative black-box model called surrogate model. Based on the complicated model, such as neural network or random forest, new features are being extracted and then used in the process of fitting a simpler interpretable model, improving its overall performance.


## Getting started
The package can be installed from GitHub using the code below:
```
devtools::install_github("MI2DataLab/SAFE")
```

## References
* [Python version of SAFE package](https://github.com/ModelOriented/SAFE)
* [SAFE article](https://arxiv.org/abs/1902.11035) - the article about SAFE algorithm, including benchmark results obtained using Python version of SAFE package

The package was created as a part of master's diploma thesis at Warsaw University of Technology at Faculty of Mathematics and Information Science by Anna Gierlak.
