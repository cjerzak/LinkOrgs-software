# `LinkOrgs`: An R package for linking linking records on organizations using half-a-billion open-collaborated records from LinkedIn 

[**What is LinkOrgs?**](#description)
| [**Installation**](#installation)
| [**Tutorial**](#tutorial)
| [**Comparison with Ground Truth**](#truth)
| [**References**](#references)
| [**Documentation**](https://github.com/cjerzak/LinkOrgs-software/blob/master/LinkOrgs.pdf)

*Jan. 19, 2024:* `LinkOrgs` has been rebuilt with a [JAX](https://en.wikipedia.org/wiki/Google_JAX) machine learning backend for improved computational efficiency and we're currently adding locality sensitive hashing from [zoomerjoin](http://beniamino.org/zoomerjoin/) for better handling of massive merge tasks (> 1 million observations in each data corpus). The re-build is experimental, so let us know if you come across any issues. 

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
                     
# perform merge using machine learning approach
z_linked_ml <- LinkOrgs(x  = x, 
                     y =  y, 
                     by.x = "orgnames_x", 
                     by.y = "orgnames_y",
                     AveMatchNumberPerAlias = 10, 
                     conda_env = "tensorflow",  
                     algorithm = "ml")
# note: change "tensorflow" to name of conda environment where a version of tensorflow v2 lives
                     
# perform merge using combined network + machine learning approach
z_linked_combined <- LinkOrgs(x  = x, 
                     y =  y, 
                     by.x = "orgnames_x", 
                     by.y = "orgnames_y",
                     AveMatchNumberPerAlias = 10, 
                     AveMatchNumberPerAlias_network = 1, 
                     algorithm = "markov",
                     conda_env = "tensorflow", 
                     DistanceMeasure = "ml")
# note: change "tensorflow" to name of conda environment where a version of tensorflow v2 lives
```

### Integrating the LinkedIn Corpus with Large Language Models (LLMs) to Link Records on Organizations
*Code under beta release. Let us know how you find it!*

We have added a new option to obtained matched datasets while using the combined the power of large language models (LLMs) with the large LinkedIn corpus. At present, we have used Bidirectional Encoder Representations from Transformers (BERT) representations of organization names that are then re-trained using the vast pool of LinkIn organizational match examples.   

To use this functionality, you'd first want install the `text` package and set up the various Python packages used in the transfer learning setup: 
```
install.packages("text")
library(   text   )
textrpp_install()

# In some cases,  you may need to specify the conda and Python to use. For example: 
textrpp_install( rpp_version = c("torch", "transformers", "numpy", "nltk"),
                 conda = "/Users/cjerzak/miniforge3/bin/conda", 
                 python_path = "~/../../usr/local/bin/python3" ) 
# Replace conda and python_path with the path to your desired conda/python.
# You can find these by entering "which conda" and "which python" or "which python3" in your terminal
```
After successfully installing the pre-trained transfer learning models via `textrpp_install()`, you can then use the `algorithm = "transfer"` option: 
```
z_linked_LLM <- LinkOrgs(x  = x, 
                     y =  y, 
                     by.x = "orgnames_x", 
                     by.y = "orgnames_y",
                     AveMatchNumberPerAlias = 10,
                     algorithm = "transfer")
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

Brian Libgober, Connor T. Jerzak. "Linking Datasets on Organizations Using Half-a-billion Open-collaborated Records." *ArXiv Preprint*, 2023.
[[PDF]](https://arxiv.org/pdf/2302.02533.pdf) [[Data]](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/EHRQQL&faces-redirect=true)

Green, Beniamino. "Zoomerjoin: Superlatively-Fast Fuzzy Joins." *Journal of Open Source Software* 8:89 5693-5698, 2023. [[PDF]](https://joss.theoj.org/papers/10.21105/joss.05693.pdf)

[<img src="https://i0.wp.com/connorjerzak.com/wp-content/uploads/2023/07/f1.png?w=738&ssl=1" width="400" height="400">](https://connorjerzak.com/linkorgs-summary/)
