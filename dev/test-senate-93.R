library(partycalls)

# This package includes functions to code party calls as described in
# Hershberger, Minozzi, and Volden (n.d.).
# It requires votes to be in a rollcall object from the pscl package.
# The package includes Keith Poole's House and Senate data from the 93rd to
# 112th congresses, and already includes the party call coding for those
# terms, but this script refits the 93rd Senate as an example.

# You can run this if you want; it'll take a few minutes.
pc_sen93 <- code_party_calls(sen93, pval_threshold = .05, type = "lm")

# The output is already in the package here, in the form of a list of rollcall
# objects, each with some added features. To save time you can run the
# next line instead of line 11.
pc_sen93 <- senate_party_calls$sen93

# The added features are:

# a vector of indices of the party calls, which refer to the columns of the
# original vote table, sen93$votes...
pc_sen93$party_calls

# records of all iterations in the coding process, for the party call
# coding...
pc_sen93$record_of_coding

# the "party-free" ideal point estimates...
pc_sen93$record_of_ideals

# the p-values and t (or z) values from the coefficients on party from the
# vote-level tests...
pc_sen93$record_of_pvals
pc_sen93$record_of_tvals

# and the final categorization into noncall/party call/gray, in a data.table
pc_sen93$party_call_coding
