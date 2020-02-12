
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rddi

<!-- badges: start -->

<!-- badges: end -->

This developer-focused package provides R representations of [DDI
Codebook 2.5](https://ddialliance.org/Specification/DDI-Codebook/2.5/)
elements to safely construct fully-validated XML while still being
flexible. There are 351 elements in the codebook schema, and while it is
the intention of the package to ultimately cover most (if not all) of
these, rddi also provides safe node creation tools to quickly create
missing elements of the schema.

## Installation

rddi is not yet on CRAN, so please download the development version
with:

``` r
# install.packages("devtools")
devtools::install_github("Global-TIES-for-Children/rddi")
```

## Example

Building components is quick and simple:

``` r
library(rddi)
library(magrittr)

main_citation <- ddi_citation(ddi_titlStmt(ddi_titl("Study Title")))

ddi_codeBook(ddi_stdyDscr(main_citation)) %>% 
  as_xml() %>% 
  as.character() %>% 
  cat()
#> <?xml version="1.0" encoding="UTF-8"?>
#> <codeBook xmlns="ddi:codebook:2_5" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" version="2.5" xsi:schemaLocation="http://www.ddialliance.org/Specification/DDI-Codebook/2.5/XMLSchema/codebook.xsd">
#>   <stdyDscr>
#>     <citation>
#>       <titlStmt>
#>         <titl>Study Title</titl>
#>       </titlStmt>
#>     </citation>
#>   </stdyDscr>
#> </codeBook>
```

Assert expected or required elements:

``` r
tryCatch(
  ddi_codeBook(),
  error = function(e) {
    print(e$message)
  }
)
#> No children specified when some are required: [stdyDscr]

tryCatch(
  ddi_citation(ddi_titlStmt(ddi_titl("Oops")), ddi_dataDscr()),
  error = function(e) {
    print(e$message)
  }
)
#> 'dataDscr' is not an acceptable child element for citation.
#> These are the allowed children: [titlStmt, biblCit]
```

Validate your work against the DDI Codebook 2.5 schema:

``` r
ddi_codeBook(ddi_stdyDscr(main_citation)) %>% 
  validate_codebook()
#> [1] TRUE
#> attr(,"errors")
#> character(0)
```
