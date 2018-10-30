# Replicating Mankiw, Romer and Weil 1992

This is my attempt in replicating the iconic economics paper by Mankiw, Romer and Weil (1992) titled "A Contribution to the Empirics of Economic Growth"

The whole thing is done in a combination of python and R. Unfortunately I had started with the intention of doing the whole thing in python, but once I got to running some basic extensions of the paper using updated data, I found out that it is much easier to deal with regressions in R as compared to python.

So apologies to anyone who gets confused (hello there mom).

## List of files:

1. .ipynb_checkpoints : This is a file created because I used a ipython notebook for the python analysis
2. data : Contains the datasets used.
  i) mrw.csv - Csv used to replicate paper.
  ii) mrwext_createcsv.py - Python script used for MRW extension. Uses as input the raw penn world tables csv. Outputs a regression ready csv to use in R.
  iii) panel_mrw.csv : The regression ready output received from mrwext_createcsv.py
  iv) pwt90.xlsx : Penn World Tables (current). Base data for the extension
3. rmd : Contains R markdown files. Go here to look at the panel regressions using the updated PWT. Input csv is panel_mrw.csv
4. MRW1992.ipynb : Python notebook walking through the original MRW (and a bit of commentary)
5. Readme.md : You are reading it right now

Please feel free to comment if anything is wrong. Especially with the extension. The original analysis framework itself seems to have so much endogeneity, on looking back, I may have overdone it in the panel section with the Arellano Bond Section.

