#!/usr/bin/env Rscript
library(dplyr)
library(data.table)
library(parallel)
library(BiocManager)
cl <- makeCluster(10)

biocVersion <- BiocManager:::.version_map()
biocVersion <- biocVersion[biocVersion$R == getRversion()[, 1:2],c("Bioc", "BiocStatus")]
if ("release" %in% biocVersion$BiocStatus) {
  biocVersion <-  as.numeric(as.character(biocVersion[biocVersion$BiocStatus == "release", "Bioc"]))
} else {
  biocVersion <-  max(as.numeric(as.character(biocVersion$Bioc)))
}

mirrorUrls <- list( bioc=paste0("http://bioconductor.statistik.tu-dortmund.de/packages/", biocVersion, "/bioc/src/contrib/")
                  , "bioc-annotation"=paste0("http://bioconductor.statistik.tu-dortmund.de/packages/", biocVersion, "/data/annotation/src/contrib/")
                  , "bioc-experiment"=paste0("http://bioconductor.statistik.tu-dortmund.de/packages/", biocVersion, "/data/experiment/src/contrib/")
                  , cran="https://cran.r-project.org/src/contrib/"
                  )

mirrorType <- commandArgs(trailingOnly=TRUE)[1]
# stopifnot(mirrorType %in% names(mirrorUrls))
write(paste("downloading package lists"), stderr())
knownPackages <- lapply(mirrorUrls, function(url) as.data.table(available.packages(url, filters=c("R_version", "OS_type", "duplicates")), method="libcurl"))

# Compare CRAN sources ----------------------------------------------------

# new source
cran_DB <- tools::CRAN_package_db()

cran_from_available_packages <- knownPackages$cran$Package |> unique()
cran_from_tools_db <- cran_DB$Package

setdiff(cran_from_available_packages, cran_from_tools_db)
tools_only <- setdiff(cran_from_tools_db, cran_from_available_packages)
# tools_DB has more

cran_DB |>
    dplyr::filter(Package %in% tools_only) |>
    View()

# == all 13 windows platform packages

length(cran_from_tools_db) - length(cran_from_available_packages)
# Why is this 28 then?

anyDuplicated(cran_from_available_packages)
anyDuplicated(cran_from_tools_db)

length(unique(cran_from_tools_db)) - length(cran_from_available_packages)
# OK, some packages in tools db are duplicated
cran_DB |>
    dplyr::filter(Package %in% Package[duplicated(Package)]) |>
    dplyr::arrange(Package) |>
    View()
# These are quite basic packages (MASS, etc), they are available for 
# different minimal R versions. An older and the latest one. Probably supported
# for backwards compatibility.
# 
# More importantly, none of the cases is it different Imports/Depends/Suggests, etc
# So its' safe to filter out the windows packages, and summarise by package name 
# for all the others

# Bioconductor ------------------------------------------------------------

# new source
bioc_DB <- BiocPkgTools::biocPkgList(repo = "BioCsoft", version = biocVersion)
bioc_from_available_packages <- knownPackages$bioc$Package 
bioc_from_tools_db <- bioc_DB$Package

setdiff(bioc_from_available_packages, bioc_from_tools_db) # 0
bioctools_only <- setdiff(bioc_from_tools_db, bioc_from_available_packages) #72
bioc_DB |>
    dplyr::filter(Package %in% bioctools_only) |>
    View()
# Many (but not all) are deprecated
# bioc DB looks fresher, or more inclusive

anyDuplicated(bioc_from_available_packages) # 0
anyDuplicated(bioc_from_tools_db) # 0


# Bioc anno  ---------------------------------------------------------------

bioc_ann_DB <- BiocPkgTools::biocPkgList(repo = "BioCann")

bioc_ann_from_available_packages <- knownPackages$`bioc-annotation`$Package 
bioc_ann_from_tools_db <- bioc_ann_DB$Package
setdiff(bioc_ann_from_available_packages, bioc_ann_from_tools_db)
setdiff(bioc_ann_from_tools_db, bioc_ann_from_available_packages) 
all(bioc_ann_from_available_packages %in% bioc_ann_from_tools_db)

# identical package list


# Bioc experiments --------------------------------------------------------


bioc_exp_DB <- BiocPkgTools::biocPkgList(repo = "BioCexp")

bioc_exp_from_available_packages <- knownPackages$`bioc-experiment`$Package 
bioc_exp_from_tools_db <- bioc_exp_DB$Package
setdiff(bioc_exp_from_available_packages, bioc_exp_from_tools_db)
biocexp_tools_only <- setdiff(bioc_exp_from_tools_db, bioc_exp_from_available_packages) 

bioc_exp_DB |>
    dplyr::filter(Package %in% biocexp_tools_only) |>
    View()

# Many (but not all) are deprecated
# bioc DB looks fresher, or more inclusive

anyDuplicated(bioc_exp_from_available_packages)
anyDuplicated(bioc_exp_from_tools_db)

# Conclusion: we can use the new package source info.

# Apply system requirements regexp ----------------------------------------

sysrq_cran <- unique(cran_DB[["SystemRequirements"]])
sysrq_bioc <- unique(bioc_DB[["SystemRequirements"]])
sysrq_bioc_ann <- unique(bioc_ann_DB[["SystemRequirements"]])
sysrq_bioc_exp <- unique(bioc_exp_DB[["SystemRequirements"]])

lut <- pkgdepends::sysreqs_db_list() |>
    select(name, patterns)


get_sysrqs <- function(db, lut) {
    luv <- structure(lut[["patterns"]],
                     names = lut[["name"]])
    res_mx <- vapply(X = luv,
           FUN = \(p) {
               grepl(
                   pattern =  paste(p, collapse = "|"),
                   db[["SystemRequirements"]],
                   ignore.case = TRUE
               )
           },
           FUN.VALUE = rep(FALSE, nrow(db))
    )
    
    res_vec <- apply(res_mx,
          1,
          # \(x) paste(names(which(x)), collapse = " "))
          \(x) names(which(x)))
    return(res_vec)
}

syscran <- get_sysrqs(cran_DB, lut)
sysbioc <- get_sysrqs(bioc_DB, lut)

# Translate to nixpkgs ----------------------------------------------------
current_R_deps <- lapply(structure(names(mirrorUrls), names = names(mirrorUrls)),
       \(mirrorType) {
           packagesFile <- paste(mirrorType, 'packages.nix', sep='-')
           readFormatted <- as.data.table(read.table(skip=8, sep='"', text=head(readLines(packagesFile), -1))) 
       })

defa <- readLines("default.nix") |> trimws()


parse_nix_deplist <- function(x) {
    strsplit(x, split = '[=\\[]') |>
        vapply(
            FUN = \(l) {
                Package <- trimws(l[1])
                SystemDependencies <- l[3] |>
                    gsub(pattern = "pkgs\\.", replacement = "") |>
                    gsub(pattern = "\\.dev\\b", replacement = "") |>
                    gsub(pattern = "([\\w\\s]*?)\\].*", replacement = "\\1") |>
                    trimws()
                c(Package = Package, SystemDependencies = SystemDependencies)
            },
            FUN.VALUE = c(Package = "p", SystemDependencies = "d")) |>
        t() |>
        as.data.frame() |>
        dplyr::mutate(SystemDependencies = strsplit(SystemDependencies, " "))
} 
        
current_nativeBuildInputs <- parse_nix_deplist(defa[312:509]) 
current_buildInputs <- parse_nix_deplist(defa[514:650])    
current_sysdeps <- merge(current_buildInputs,
      current_nativeBuildInputs,
      by = "Package",
      all = TRUE, ) |>
    dplyr::rowwise() |>
    dplyr::transmute(
        Package,
        SystemDependencies = list(sort(union(
            SystemDependencies.x,
            SystemDependencies.y
        )))
    ) 

# Compare SysRqs ----------------------------------------------------------

# Make tibble
automated_sysdeps <- dplyr::bind_rows(
    tibble(Package = cran_DB$Package, SystemDependencies = syscran),
    tibble(Package = bioc_DB$Package, SystemDependencies = sysbioc)
)


to_check <- dplyr::full_join(
    automated_sysdeps,
    current_sysdeps,
    by = "Package", suffix = c("_auto", "_curr"), 
) |>
    dplyr::filter(
        vapply(SystemDependencies_auto, length, 1L) > 0L |
            vapply(SystemDependencies_curr, length, 1L) > 0L
        ) |>
    dplyr::mutate(across(starts_with("SystemDependencies"),
                         .fns = \(xs) vapply(xs, \(x) paste(sort(x), collapse = " "), " ")))

readr::write_csv(to_check, "./compare_sysdeps.csv")

# The rest of the script --------------------------------------------------




pkgs <- knownPackages[mirrorType][[1]]
setkey(pkgs, Package)
knownPackages <- c(unique(do.call("rbind", knownPackages)$Package))
knownPackages <- sapply(knownPackages, gsub, pattern=".", replacement="_", fixed=TRUE)

mirrorUrl <- mirrorUrls[mirrorType][[1]]
nixPrefetch <- function(name, version) {
  prevV <- readFormatted$V2 == name & readFormatted$V4 == version
  if (sum(prevV) == 1)
    as.character(readFormatted$V6[ prevV ])

  else {
    # avoid nix-prefetch-url because it often fails to fetch/hash large files
    url <- paste0(mirrorUrl, name, "_", version, ".tar.gz")
    tmp <- tempfile(pattern=paste0(name, "_", version), fileext=".tar.gz")
    cmd <- paste0("wget -q -O '", tmp, "' '", url, "'")
    if(mirrorType == "cran"){
      archiveUrl <- paste0(mirrorUrl, "Archive/", name, "/", name, "_", version, ".tar.gz")
      cmd <- paste0(cmd, " || wget -q -O '", tmp, "' '", archiveUrl, "'")
    }
    cmd <- paste0(cmd, " && nix-hash --type sha256 --base32 --flat '", tmp, "'")
    cmd <- paste0(cmd, " && echo >&2 '  added ", name, " v", version, "'")
    cmd <- paste0(cmd, " ; rm -rf '", tmp, "'")
    system(cmd, intern=TRUE)
  }

}

escapeName <- function(name) {
    switch(name, "import" = "r_import", "assert" = "r_assert", name)
}

formatPackage <- function(name, version, sha256, depends, imports, linkingTo) {
    attr <- gsub(".", "_", escapeName(name), fixed=TRUE)
    options(warn=5)
    depends <- paste( if (is.na(depends)) "" else gsub("[ \t\n]+", "", depends)
                    , if (is.na(imports)) "" else gsub("[ \t\n]+", "", imports)
                    , if (is.na(linkingTo)) "" else gsub("[ \t\n]+", "", linkingTo)
                    , sep=","
                    )
    depends <- unlist(strsplit(depends, split=",", fixed=TRUE))
    depends <- lapply(depends, gsub, pattern="([^ \t\n(]+).*", replacement="\\1")
    depends <- lapply(depends, gsub, pattern=".", replacement="_", fixed=TRUE)
    depends <- depends[depends %in% knownPackages]
    depends <- lapply(depends, escapeName)
    depends <- paste(depends)
    depends <- paste(sort(unique(depends)), collapse=" ")
    paste0("  ", attr, " = derive2 { name=\"", name, "\"; version=\"", version, "\"; sha256=\"", sha256, "\"; depends=[", depends, "]; };")
}

clusterExport(cl, c("nixPrefetch","readFormatted", "mirrorUrl", "mirrorType", "knownPackages"))

pkgs <- pkgs[order(Package)]

write(paste("updating", mirrorType, "packages"), stderr())
pkgs$sha256 <- parApply(cl, pkgs, 1, function(p) nixPrefetch(p[1], p[2]))
nix <- apply(pkgs, 1, function(p) formatPackage(p[1], p[2], p[18], p[4], p[5], p[6]))
write("done", stderr())

# Mark deleted packages as broken
setkey(readFormatted, V2)
markBroken <- function(name) {
  str <- paste0(readFormatted[name], collapse='"')
  if(sum(grep("broken = true;", str)))
    return(str)
  write(paste("marked", name, "as broken"), stderr())
  gsub("};$", "broken = true; };", str)
}
broken <- lapply(setdiff(readFormatted[[2]], pkgs[[1]]), markBroken)

cat("# This file is generated from generate-r-packages.R. DO NOT EDIT.\n")
cat("# Execute the following command to update the file.\n")
cat("#\n")
cat(paste("# Rscript generate-r-packages.R", mirrorType, ">new && mv new", packagesFile))
cat("\n\n")
cat("{ self, derive }:\n")
cat("let derive2 = derive ")
if (mirrorType == "cran") { cat("{  }")
} else if (mirrorType == "irkernel") { cat("{}")
} else { cat("{ biocVersion = \"", biocVersion, "\"; }", sep="") }
cat(";\n")
cat("in with self; {\n")
cat(paste(nix, collapse="\n"), "\n", sep="")
cat(paste(broken, collapse="\n"), "\n", sep="")
cat("}\n")

stopCluster(cl)
