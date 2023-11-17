# Read data
# Updated:    2023-09-07
# Auhtors:    Christian Vedel [christian-vs@sam.sdu.dk],
#
# Purpose:    Reads church records
#
# Output:     

# ==== libraries ====
library(tidyverse)

# ==== Read =====
data0 = read_delim("../../../Parish Records DK/61607_Denmark Church Books_FullDelivery.txt", delim = "|")
