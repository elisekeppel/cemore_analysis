---
title: "HW feb2022 to oct 2022 comparison"
author: "EK"
date: "2/8/2023"
output: pdf_document
---



# Cemore line transect analysis – comparison of Feb2022 data and Oct 2022 data

Humpbacks, Truncation at 1.5 km

Oct N = 372, Ret only data N = 366. Maybe don’t need to keep 
Best model (all sgt) = hn with vis


## Oct 2022 all sgt
Best model = hn with visibility as cov, next best = uniform with cos (1) adj 


\begin{landscape}\begin{table}

\caption{\label{tab:oct-all-sgt}humpback  Comparison models to Oct 2022, all sgt. Truncation = 1.5 km.}
\centering
\begin{tabular}[t]{l|l|l|r|r|r|r}
\hline
Model & Key function & Formula & C-vM $p$-value & Average detectability & se(Average detectability) & Delta AIC\\
\hline
hw1.5.hn.vis & Half-normal & ~Visibility & 0.754 & 0.591 & 0.029 & 0.000\\
\hline
hw1.5.un.cos & Uniform with cosine adjustment term of order 1 & NA & 0.728 & 0.596 & 0.022 & 1.305\\
\hline
hw1.5.hn & Half-normal & ~1 & 0.665 & 0.598 & 0.028 & 1.672\\
\hline
hw1.5.hn.g90y & Half-normal & ~Glare90y & 0.736 & 0.597 & 0.028 & 1.799\\
\hline
hw1.5.hn.bf & Half-normal & ~Beaufort & 0.683 & 0.598 & 0.028 & 2.969\\
\hline
\end{tabular}
\end{table}
\end{landscape}

```
## # A tibble: 4 x 9
##   Season     n    ER cv.ER    GS     N  cv.N    L95   U95
##   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
## 1 Winter    14  0.01  0.35  1.17  18.0  0.36   9.07  35.8
## 2 Spring    58  0.04  0.3   1.23  62.8  0.3   35.5  111. 
## 3 Summer   132  0.1   0.27  1.22 159.   0.27  93.9  271. 
## 4 Fall     277  0.27  0.25  1.87 462.   0.25 281.   759.
```

## Oct 2022 - reticle only sgt


\begin{landscape}\begin{table}

\caption{\label{tab:oct-ret}humpback  Comparison of models to Oct 2022, ret only. Truncation = 1.5 km.}
\centering
\begin{tabular}[t]{l|l|l|r|r|r|r}
\hline
Model & Key function & Formula & C-vM $p$-value & Average detectability & se(Average detectability) & Delta AIC\\
\hline
hw1.5.un.cos & Uniform with cosine adjustment term of order 1 & NA & 0.752 & 0.600 & 0.023 & 0.000\\
\hline
hw1.5.hn & Half-normal & ~1 & 0.671 & 0.605 & 0.029 & 0.307\\
\hline
hw1.5.hn.g90y & Half-normal & ~Glare90y & 0.722 & 0.603 & 0.029 & 0.601\\
\hline
hw1.5.hn.vis & Half-normal & ~Visibility & 0.700 & 0.600 & 0.029 & 0.840\\
\hline
hw1.5.hn.bf & Half-normal & ~Beaufort & 0.691 & 0.604 & 0.029 & 1.471\\
\hline
\end{tabular}
\end{table}
\end{landscape}

```
## # A tibble: 4 x 9
##   Season     n    ER cv.ER    GS     N  cv.N    L95   U95
##   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
## 1 Winter    12  0.01  0.38  1.09  15.6  0.38   7.47  32.5
## 2 Spring    57  0.04  0.3   1.24  61.3  0.3   34.1  110. 
## 3 Summer   131  0.1   0.27  1.22 159.   0.28  93.3  273. 
## 4 Fall     274  0.27  0.25  1.89 437.   0.26 265.   720.
```

## Feb 2022 all sgt

Feb N = 193, Reticle only data N = 188, no obvious change in the data (hist) with the non-ret sgt’s so kept them in.


\begin{landscape}\begin{table}

\caption{\label{tab:feb-all-sgt}humpback  Comparison of models to Feb 2022, all sgt. Truncation = 1.5 km.}
\centering
\begin{tabular}[t]{l|l|l|r|r|r|r}
\hline
Model & Key function & Formula & C-vM $p$-value & Average detectability & se(Average detectability) & Delta AIC\\
\hline
hw1.5.un.cos & Uniform with cosine adjustment term of order 1 & NA & 0.982 & 0.58 & 0.028 & 0.000\\
\hline
hw1.5.hn.vis & Half-normal & ~Visibility & 0.958 & 0.57 & 0.040 & 0.131\\
\hline
hw1.5.hn & Half-normal & ~1 & 0.978 & 0.58 & 0.038 & 0.206\\
\hline
hw1.5.hn.g90y & Half-normal & ~Glare90y & 0.983 & 0.58 & 0.039 & 2.191\\
\hline
hw1.5.hn.bf & Half-normal & ~Beaufort & 0.979 & 0.58 & 0.038 & 2.203\\
\hline
\end{tabular}
\end{table}
\end{landscape}

```
## # A tibble: 4 x 9
##   Season     n    ER cv.ER    GS     N  cv.N    L95   U95
##   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
## 1 Winter    14  0.01  0.35  1.17  18.8  0.36   9.47  37.3
## 2 Spring    41  0.05  0.39  1.24  77.1  0.39  36.5  163. 
## 3 Summer    42  0.05  0.3   1.17  91.2  0.3   50.6  165. 
## 4 Fall     140  0.16  0.27  1.82 268.   0.28 156.   461.
```


\begin{landscape}\begin{table}

\caption{\label{tab:feb}humpback  Comparison of models to Feb 2022, ret only. Truncation = 1.5 km.}
\centering
\begin{tabular}[t]{l|l|l|r|r|r|r}
\hline
Model & Key function & Formula & C-vM $p$-value & Average detectability & se(Average detectability) & Delta AIC\\
\hline
hw1.5.un.cos & Uniform with cosine adjustment term of order 1 & NA & 0.952 & 0.586 & 0.030 & 0.000\\
\hline
hw1.5.hn & Half-normal & ~1 & 0.953 & 0.590 & 0.040 & 0.153\\
\hline
hw1.5.hn.bfc & Half-normal & ~Clumped_Beaufort & 0.799 & 0.583 & 0.039 & 0.540\\
\hline
hw1.5.hn.vis & Half-normal & ~Visibility & 0.965 & 0.585 & 0.040 & 2.120\\
\hline
hw1.5.hn.bf & Half-normal & ~Beaufort & 0.952 & 0.590 & 0.040 & 2.143\\
\hline
hw1.5.hn.g90y & Half-normal & ~Glare90y & 0.956 & 0.590 & 0.041 & 2.146\\
\hline
\end{tabular}
\end{table}
\end{landscape}

```
## # A tibble: 4 x 9
##   Season     n    ER cv.ER    GS     N  cv.N    L95   U95
##   <chr>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl>
## 1 Winter    12  0.01  0.38  1.09  15.9  0.39   7.63  33.3
## 2 Spring    40  0.04  0.4   1.25  74.4  0.4   34.6  160. 
## 3 Summer    42  0.05  0.3   1.17  90.3  0.3   50.0  163. 
## 4 Fall     137  0.16  0.28  1.85 259.   0.28 149.   451.
```
