library(tigris)
library(Matrix)
library(spdep)
#' connect observations, identified by GEOID
#' @param id a GEOID to identify the county to connect to its `neighbors'
#' @param neighbors character vector of neighboring county GEOIDs
#' @param C connectivity matrix
#' @param data  the data, with columns GEOID and county (name)
#'
#' @author Connor Donegan
connect <- function(id, neighbors, C, data) {
    stopifnot(all(dim(C) == nrow(data)))
    names(data)[grep("GEOID", names(data))] <- "GEOID"
    id1 <- which(data$GEOID == id)
    for (i in seq_along(neighbors)) {
        id2 <- which(data$GEOID == neighbors[i])
        message("Connecting ", as.data.frame(data)[c(id1, id2), grep("GEOID|^NAME", colnames(data))],"\n")
        C[id1, id2] <- C[id2, id1] <- 1
    }
    return(C)
}

year = commandArgs(trailingOnly = TRUE)
year = as.numeric(year)

## load counties, drop US Territories, move Alaska/Hawaii
sdf <- counties(year = year) %>%
    dplyr::filter(as.numeric(STATEFP) < 57) %>%
    shift_geometry(preserve_area = TRUE)

## check results
#plot(sdf[,'AWATER'])

## create adjacency matrix
nb <- poly2nb(sdf, queen = TRUE)
A <- nb2mat(nb, zero.policy = TRUE, style = "B")

## view connections
#plot(nb, st_geometry(sdf))

## connect Hawaiian islands to each other
fips_codes[which(fips_codes$state == "HI"),]
## Hawaii
# 15007 -> 15003
# 15003 -> 15005
# 15009 -> 15001
C <- connect("15007", "15003", C = A, data = sdf)
C <- connect("15003", "15005", C = C, data = sdf)
C <- connect("15009", "15001", C = C, data = sdf)

## check results: symmetric matrix with no "islands"
message("Symmetric matrix: ", isSymmetric(C, check.attributes=FALSE))
message("Median numbers of neighbors: ", median(rowSums(C)))
cat("Min, max numbers of neighbors: ", quantile(rowSums(C), probs=c(0,1)), "\n")

## visually check results
E <- Matrix::summary(Matrix::Matrix(C))
E <- E[which(E$i < E$j), ]
G <- list(np = nrow(C), # confrom to spdep graph structure, then back to nb to map it again
          from = E$i,
          to = E$j,
          nedges = nrow(E)
          )
class(G) <- "Graph"
nb <- spdep::graph2nb(G)
plot(nb, st_geometry(sdf))

## save results as sparse matrix
Cs <- Matrix::Matrix(C, sparse = TRUE)
colnames(Cs) <- rownames(Cs) <- sdf$GEOID
file.name <- paste0("county-connectivity-", year, ".rds")
readr::write_rds(Cs, file = file.name)

