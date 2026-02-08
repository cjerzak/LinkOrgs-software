# `LinkOrgs`: An R package for linking records on organizations using half-a-billion open-collaborated records from LinkedIn

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

[**What is LinkOrgs?**](#what-is-linkOrgs)
| [**Installation**](#installation)
| [**Tutorial**](#tutorial)
| [**Comparison with Ground Truth**](#comparison-of-results-with-ground-truth)
| [**References**](#references)
| [**Documentation**](https://github.com/cjerzak/LinkOrgs-software/blob/master/LinkOrgs.pdf)

[<img src="https://img.shields.io/badge/Demo-View%20Demo-blue" alt="Demo Button">](https://connorjerzak.com/wp-content/uploads/2025/01/MainVignette.html)

[![Hugging Face
Dataset](https://img.shields.io/badge/Hugging%20Face-View%20Dataset-orange?style=flat-square&logo=huggingface&logoColor=white)](https://huggingface.co/datasets/cjerzak/LinkOrgs)

[![Hugging Face Space](https://img.shields.io/badge/%F0%9F%A4%96_Hugging%20Face-Launch%20Online%20App-purple?style=for-the-badge&logo=huggingface&logoColor=white)](https://huggingface.co/spaces/cjerzak/LinkOrgs_Online/)

**Note:** You can access a point-and-click implementation online [here](https://huggingface.co/spaces/cjerzak/LinkOrgs_Online/).

## What is LinkOrgs?

LinkOrgs is an R package for organizational record linkage that leverages half-a-billion open-collaborated records from LinkedIn. It provides multiple matching algorithms optimized for different use cases:

| Algorithm | Internet Required | ML-backend Required | Speed | Best For |
|-----------|------------------|---------------------|-------|----------|
| `fuzzy` | No | No | Fast | Simple name matching |
| `bipartite` | Yes | No | Medium | Network-informed matching best for organizations having LinkedIn presence, ~2017 |
| `markov` | Yes | No | Medium | Network-informed matching best for organizations having LinkedIn presence, ~2017 |
| `ml` | Yes | Yes | Slower | High-accuracy semantic matching |
| `transfer` | Yes | Yes | Slower | Combined network + ML approach |

- **Fuzzy matching** (`algorithm="fuzzy"`): Fast parallelized string distance matching using Jaccard, Jaro-Winkler, or other string distances
- **Network-based** (`algorithm="bipartite"` or `"markov"`): Uses LinkedIn's organizational network structure for improved accuracy
- **Machine learning** (`algorithm="ml"`): Transformer-based embeddings (requires JAX backend setup via `BuildBackend()`)
- **Combined** (`algorithm="markov"` + `DistanceMeasure="ml"`): Network + ML hybrid approach

## Installation
The most recent version of `LinkOrgs` can be installed directly from the repository using the `devtools` package

```
# install package 
devtools::install_github("cjerzak/LinkOrgs-software/LinkOrgs")
```

The machine-learning based algorithm accessible via the `algorithm="ml"` option relies on `jax`. The network-based linkage approaches (`algorithm="bipartite"` and `algorithm = "markov"`) do not require these packages. To setup the machine learning backend, you can call 

```
# install ML backend  
LinkOrgs::BuildBackend(conda = "auto")
```

Note that most package options require Internet access in order to download the saved machine learning model parameters and LinkedIn-based network information.

## Quick Start

```r
library(LinkOrgs)

# Sample data
x <- data.frame(org = c("Apple Inc", "Microsoft Corp"))
y <- data.frame(org = c("Apple", "Microsoft Corporation"))

# Link organizations using fuzzy matching
result <- LinkOrgs(x = x, y = y, by.x = "org", by.y = "org",
                   algorithm = "fuzzy", AveMatchNumberPerAlias = 2)
print(result)
```

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


# Build backend for ML model (run once before using algorithm="ml")
# LinkOrgs::BuildBackend(conda_env = "LinkOrgs_env", conda = "auto")
# If conda = "auto" fails, specify the path explicitly:
# LinkOrgs::BuildBackend(conda_env = "LinkOrgs_env",
#                        conda = "/path/to/miniforge3/bin/python")
                     
# perform merge using a machine learning approach
z_linked_ml <- LinkOrgs(x  = x, 
                     y =  y, 
                     by.x = "orgnames_x", 
                     by.y = "orgnames_y",
                     AveMatchNumberPerAlias = 10, 
                     algorithm = "ml", ml_version = "v1")
# note: use conda_env parameter to specify a different environment if needed
# note: ML versions v0-v4 are available with varying parameter counts (9M-31M). Default is v1.

# perform merge using combined network + machine learning approach
z_linked_combined <- LinkOrgs(x  = x, 
                     y =  y, 
                     by.x = "orgnames_x", 
                     by.y = "orgnames_y",
                     AveMatchNumberPerAlias = 10, 
                     AveMatchNumberPerAlias_network = 1, 
                     algorithm = "markov",
                     DistanceMeasure = "ml", ml_version = "v1")

  # Perform a merge using the ML approach, exporting name representations only
  # Returns list(embedx = ..., embedy = ...) for manual linkage.
  rep_joint <- LinkOrgs( 
    x = x, y = y,
    by.x = "orgnames_x",
	by.y = "orgnames_y",
    algorithm = "ml",
    ExportEmbeddingsOnly = TRUE
  )
  
  # returns list(embedx = ...)
  rep_x <- LinkOrgs( 
    x = x, y = NULL,
    by.x = "orgnames_x",
    algorithm = "ml",
    ExportEmbeddingsOnly = TRUE
  ) 
  
  # returns list(embedy = ...)
  rep_y <- LinkOrgs( 
    x = NULL, y = y,
    by.y = "orgnames_y",
    algorithm = "ml",
    ExportEmbeddingsOnly = TRUE) 
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
                                            z = z_linked_fuzzy, 
                                            z_true = z_true)
```

<a href="https://doi.org/10.1017/psrm.2024.55#gh-light-mode-only">
  <img src="https://i0.wp.com/connorjerzak.com/wp-content/uploads/2025/12/linkorgs1_light.webp#gh-light-mode-only" alt="Figure 1 – light" width="400" height="400">
</a>

<a href="https://doi.org/10.1017/psrm.2024.55#gh-dark-mode-only">
  <img src="https://i0.wp.com/connorjerzak.com/wp-content/uploads/2025/12/linkorgs1_dark.webp#gh-dark-mode-only" alt="Figure 1 – dark" width="400" height="400">
</a>

<a href="https://doi.org/10.1017/psrm.2024.55#gh-light-mode-only">
  <img src="https://i0.wp.com/connorjerzak.com/wp-content/uploads/2025/12/linkorgs2_light.webp#gh-light-mode-only" alt="Figure 2 – light" width="900" height="300">
</a>

<a href="https://doi.org/10.1017/psrm.2024.55#gh-dark-mode-only">
  <img src="https://i0.wp.com/connorjerzak.com/wp-content/uploads/2025/12/linkorgs2_dark.webp#gh-dark-mode-only" alt="Figure 2 – dark" width="900" height="300">
</a>

## Improvements & Future Development Plan
We're always looking to improve the software in terms of ease-of-use and its capabilities. If you have any suggestions/feedback, or need further assistance in getting the package working for your analysis, please email <connor.jerzak@gmail.com>. 

In future releases, we will be expanding the merge capabilities (currently, we only allow inner joins [equivalent to setting `all = F` in the `merge` function from base `R`]; future releases will allow more complex inner, left, right, and outer joins).

## Acknowledgments
We thank [Beniamino Green](https://beniamino.org/about/), [Kosuke Imai](https://imai.fas.harvard.edu/),
[Gary King](https://garyking.org/), [Xiang Zhou](https://scholar.harvard.edu/xzhou/home),  members of the Imai Research Workshop for valuable feedback. We also would like to thank [Gil Tamir](https://www.linkedin.com/in/gil-tamir-4176161b7/) and [Xiaolong Yang](https://xiaolong-yang.com/) for excellent research assistance. 

## License
Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International (CC BY-NC-ND 4.0).
This package is for academic and non-commercial use only.

## References 

Brian Libgober, Connor T. Jerzak. "Linking Datasets on Organizations Using Half-a-billion Open-collaborated Records." *Political Science Methods and Research*, 2025.
[[PDF]](https://doi.org/10.1017/psrm.2024.55) [[Dataverse]](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/EHRQQL&faces-redirect=true)
[[Hugging Face]](https://huggingface.co/datasets/cjerzak/LinkOrgs)
```
@article{LibgoberJerzak2025,
	title={Linking datasets on organizations using half a billion open-collaborated records},
	volume={13},
	DOI={10.1017/psrm.2024.55},
	number={4},
	journal={Political Science Research and Methods},
	author={Libgober, Brian and Jerzak, Connor T.},
	year={2025},
	pages={923-942}
}

```


