# `LinkOrgs`: An R package for linking linking records on organizations using half-a-billion open-collaborated records from LinkedIn 

[**What is LinkOrgs?**](#description)
| [**Installation**](#installation)
| [**Tutorial**](#tutorial)
| [**Comparison with Ground Truth**](#truth)
| [**References**](#references)
| [**Documentation**](https://github.com/cjerzak/LinkOrgs-software/blob/master/LinkOrgs.pdf)

[<img src="https://img.shields.io/badge/Demo-View%20Demo-blue" alt="Demo Button">](https://connorjerzak.com/wp-content/uploads/2025/01/MainVignette.html)

**NB: You can also check out alternative  implementation [here](https://github.com/beniaminogreen/linkorgsonnx).**

## Installation
The most recent version of `LinkOrgs` can be installed directly from the repository using the `devtools` package

```
# install package 
devtools::install_github("cjerzak/LinkOrgs-software/LinkOrgs")
```

The machine-learning based algorithm accessible via the `algorithm="ml"` option relies on `tensorflow` and `Rtensorflow`. For details about downloading, see `https://tensorflow.rstudio.com/installation/`. The network-based linkage approaches (`algorithm="bipartite"` and `algorithm = "markov"`) do not require these packages. To setup the machine learning backend, you can call 

```
# install ML backend  
LinkOrs::BuildBackend(conda_env = "LinkOrgsEnv", conda = "auto")
```

Note that most package options require Internet access in order to download the saved machine learning model parameters and LinkedIn-based network information. 

## Tutorial
After installing the package, let's get some experience with it in an example. 

```
# load in package 
library(LinkOrgs)

# set up synthetic data for the merge 
x_orgnames <- c("apple","oracle","enron inc.","mcdonalds corporation")
y_orgnames <- c("apple corp","oracle inc","enron","mcdonalds")
x <- data.frame("orgnames_x"=x_orgnames)
y <- data.frame("orgnames_y"=y_orgnames)
```
After creating these synthetic datasets, we're now ready to merge them. We can do this in a number of ways. See the paper listed in the reference for information about which may be most useful for your merge task.  

First, we'll try a merge using parallelized fast fuzzy matching via `LinkOrgs::LinkOrgs`. A key hyperparameter is `AveMatchNumberPerAlias`, which controls the number of matches per alias (in practice, we calibrate this with an initial random sampling step, the exact matched dataset size won't be a perfect multiple of `AveMatchNumberPerAlias`). Here, we set `AveMatchNumberPerAlias = 10` so that all observations in this small dataset are potentially matched against all others for illustration purposes. 
``` 
# perform merge using (parallelized) fast fuzzy matching
# LinkOrgs::LinkOrgs can be readily used for non-organizational name matches 
# when doing pure parallelized fuzzy matching 
z_linked_fuzzy <- LinkOrgs::LinkOrgs(x  = x,
                        y =  y,
                        by.x = "orgnames_x",
                        by.y = "orgnames_y",
                        algorithm = "fuzzy", 
                        DistanceMeasure = "jaccard", 
                        AveMatchNumberPerAlias = 4)
```
Next, we'll try using some of the LinkedIn-calibrated approaches using `LinkOrgs::LinkOrgs`: 
```
# perform merge using bipartite network approach
z_linked_bipartite <- LinkOrgs(x  = x, 
                     y =  y, 
                     by.x = "orgnames_x", 
                     by.y = "orgnames_y",
                     AveMatchNumberPerAlias = 10,
                     algorithm = "bipartite", 
                     DistanceMeasure = "jaccard")
                     
# perform merge using markov network approach
z_linked_markov <- LinkOrgs(x  = x, 
                     y =  y, 
                     by.x = "orgnames_x", 
                     by.y = "orgnames_y",
                     AveMatchNumberPerAlias = 10,
                     algorithm = "markov", 
                     DistanceMeasure = "jaccard")


# Build backend for ML model (do this only once)# try(LinkOrgs::BuildBackend( conda_env = "LinkOrgsEnv", conda = "auto" ),T)# if conda = "auto" fails, try to specify the path to the correct conda # LinkOrgs::BuildBackend( conda_env = "LinkOrgsEnv", conda = "/Users/cjerzak/miniforge3/bin/python" )
                     
# perform merge using a machine learning approach
z_linked_ml <- LinkOrgs(x  = x, 
                     y =  y, 
                     by.x = "orgnames_x", 
                     by.y = "orgnames_y",
                     AveMatchNumberPerAlias = 10, 
                     conda_env = "LinkOrgsEnv",  
		     algorithm = "ml", ml_version = "v4")
# note: change "tensorflow" to name of conda environment where a version of tensorflow v2 lives
                     
# perform merge using combined network + machine learning approach
z_linked_combined <- LinkOrgs(x  = x, 
                     y =  y, 
                     by.x = "orgnames_x", 
                     by.y = "orgnames_y",
                     AveMatchNumberPerAlias = 10, 
                     AveMatchNumberPerAlias_network = 1, 
                     algorithm = "markov",
                     conda_env = "LinkOrgsEnv", 
                     DistanceMeasure = "ml", ml_version = "v4")
# note: change "tensorflow" to name of conda environment where a version of tensorflow v2 lives
```

## Comparison of Results with Ground Truth 
Using the package, we can also assess performance against a ground-truth merged dataset (if available): 
``` 
# (After running the above code)
z_true <- data.frame("orgnames_x"=x_orgnames, "orgnames_y"=y_orgnames)

# Get performance matrix 
PerformanceMatrix <- AssessMatchPerformance(x  = x, 
                                            y =  y, 
                                            by.x = "orgnames_x", 
                                            by.y = "orgnames_y", 
                                            z = z_linked, 
                                            z_true = z_true)
``` 

## Improvements & Future Development Plan
We're always looking to improve the software in terms of ease-of-use and its capabilities. If you have any suggestions/feedback, or need further assistance in getting the package working for your analysis, please email <connor.jerzak@gmail.com>. 

In future releases, we will be expanding the merge capabilities (currently, we only allow inner joins [equivalent to setting `all = F` in the `merge` function from base `R`]; future releases will allow more complex inner, left, right, and outer joins).

## Acknowledgments
We thank [Beniamino Green](https://beniamino.org/about/), [Kosuke Imai](https://imai.fas.harvard.edu/),
[Gary King](https://garyking.org/), [Xiang Zhou](https://scholar.harvard.edu/xzhou/home),  members of the Imai Research Workshop for valuable feedback. We also would like to thank [Gil Tamir](https://www.linkedin.com/in/gil-tamir-4176161b7/) and [Xiaolong Yang](https://xiaolong-yang.com/) for excellent research assistance. 

## License
MIT License.

## References 

Brian Libgober, Connor T. Jerzak. "Linking Datasets on Organizations Using Half-a-billion Open-collaborated Records." *Political Science Methods and Research*, 2024.
[[PDF]](https://doi.org/10.1017/psrm.2024.55) [[Data]](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/EHRQQL&faces-redirect=true)
```
@article{libgober2024linking,
  title={Linking Datasets on Organizations Using Half a Billion Open-Collaborated Records},
  author={Libgober, Brian and Connor T. Jerzak},
  journal={Political Science Methods and Research},
  year={2024},
  pages={},
  publisher={Cambridge University Press}
}
```

## Related work
Green, Beniamino. "Zoomerjoin: Superlatively-Fast Fuzzy Joins." *Journal of Open Source Software* 8:89 5693-5698, 2023. [[PDF]](https://joss.theoj.org/papers/10.21105/joss.05693.pdf)

[<img src="https://i0.wp.com/connorjerzak.com/wp-content/uploads/2023/07/f1.png?w=738&ssl=1" width="400" height="400">](https://doi.org/10.1017/psrm.2024.55)

[<img src="https://i0.wp.com/connorjerzak.com/wp-content/uploads/2024/09/Screenshot-2024-09-01-at-8.37.04%E2%80%AFPM.png?w=1280&ssl=1" width="900" height="300">](https://doi.org/10.1017/psrm.2024.55)

[<img src="https://i0.wp.com/connorjerzak.com/wp-content/uploads/2024/09/Screenshot-2024-09-03-at-2.32.26%E2%80%AFPM.png?w=944&ssl=1" width="400" height="400">](https://doi.org/10.1017/psrm.2024.55)


