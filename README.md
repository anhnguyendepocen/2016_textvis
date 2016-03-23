# Text Visualiation at LSE / Imperial College: Why word clouds aren't always stupid

This repository contains the [source file](presentation.Rpres) for my presentation on text visualization at imperial college. 
You can view the generated slides [here](http://vanatteveldt.com/p/atteveldt_textvis.html)

You need to install [corpus-tools](http://github.com/kasperwelbers/corpus-tools) and [semnet](http://github.com/kasperwelbers/corpus-tools),
both of which can be installed directly from github:

```{r}
if (!require(devtools)) install.packages("devtools")
devtools::install_github("kasperwelbers/corpus-tools")
devtools::install_github("kasperwelbers/semnet")
```
