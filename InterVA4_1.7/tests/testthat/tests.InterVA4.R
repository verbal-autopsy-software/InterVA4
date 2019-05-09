context("InterVA4 Algorithm")

# generate dummydata
source("InterVA4_dummy.R")

# A group
for (cod in rownames(dummydata)[grep("^A_", rownames(dummydata))]){
  test_that(paste0(cod, " (", dummydata[cod, "ID"], ")"), {

    out <- InterVA(dummydata[cod,, drop=FALSE], HIV = "l", Malaria = "l", write=F)

    expect_true(dummydata[cod, "ID"] %in% c(out$VA[[1]]$PREGSTAT))
    rm(out)
  })
}

# B group
for (cod in rownames(dummydata)[grep("^B_", rownames(dummydata))]){
  test_that(paste0(cod, " (", dummydata[cod, "ID"], ")"), {

    out <- InterVA(dummydata[cod,, drop=FALSE], HIV = "l", Malaria = "l", write=F)

    expect_true(dummydata[cod, "ID"] %in% c(out$VA[[1]]$CAUSE1))
    rm(out)
  })
}

