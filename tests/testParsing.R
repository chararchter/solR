if(!exists("interpretSolPanel", mode="function")) source("interpretSolPanel.R")
if(!exists("searchPattern", mode="function")) source("searchPattern.R")

# kāpēc neploto pv_w?
# test cases:
test = c("solA13JA_PV_V", "solA13JA_PV_A", "solA13JA_PV_W")

# print(interpretSolPanel(test[1]))
print(interpretSolPanel(test[2]))
print(interpretSolPanel(test[3]))


# x <- "Split the words in a sentence."
x = "solA13JA_PV_A"
x = strsplit(x, "_")