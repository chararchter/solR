string <- "aabbccccdd"
# total length of string
num.chars <- nchar(string)

# the indices where each substr will start
starts <- seq(1,num.chars, by=2)
print(starts)

# chop it up
sapply(starts, function(ii) {
    substr(string, ii, ii+1)
})


test1 = "solR13JA_PV_W"
test2 = "solR13JA_BatW"

# the indices where each substr will start
starts <- c(5,7,9,10,11,12)

sapply(starts, function(ii) {
    substr(test1, ii, ii+1)
})

substr("solR13JA_PV_W", 2, 4)
substring("solR13JA_PV_W", 4:4, 5:7)


solname = test2

dir = substr(solname, 4, 4)
type = substr(solname, 7, 8)
degree = substr(solname, 5, 6)
device = substr(solname, 10, 10)
unit = substr(solname, 13, 13)

if (device == "P"){
    device = substr(solname, 10, 11)
}
if (device == "B"){
    device = substr(solname, 10, 12)
}

parameters <- c(dir, degree, type, device, unit)
names(parameters) <- c("dir", "degree", "type", "device", "unit")

print(parameters)