# Empirical Model Learning -- IJCAI 2018 Tutorial


## Installation Instructions

### Jupyter

Method 1: "regular" Python > v3.5

Method 2: Anaconda Python > v3.5


### Simulator

* Install `R`
* From an `R` prompt:

```R
install.packages("EpiModel", dependencies = TRUE)
```

* Install the [Jupyter R kernel](https://irkernel.github.io)
  * See also the [Github repository](https://github.com/IRkernel/IRkernel)

```R
install.packages(c('repr', 'IRdisplay', 'evaluate', 'crayon', 'pbdZMQ', 'devtools', 'uuid', 'digest'))
devtools::install_github('IRkernel/IRkernel')
```

* Register the kernel on Jupyter

```R
IRkernel::installspec()
```


### Neural Network Models

* Install [Tensorflow](https://www.tensorflow.org)

```sh
pip3 install tensorflow
```

* Install [Keras](https://keras.io)

```sh
pip3 install keras
```


### Decision Tree Models

* Install [scikit-learn](http://scikit-learn.org/stable/index.html)

```sh
pip3 install sklearn
```

