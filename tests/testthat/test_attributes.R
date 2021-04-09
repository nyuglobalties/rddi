context("DDI attribute tests")

test_that("Attribute is character string", {
    test_node <- build_branch_node("branchtest", attribs = list(attr = 3))
    expect_error(check_strings(test_node$attribs), class = "rddi_error")
})

test_that("XML NCname variables verifications", {
    test_node <- build_branch_node("branchtest", attribs = list(attr = "12"))
    expect_error(check_ncname(test_node$attribs), class = "rddi_error")

    test_node <- build_branch_node("branchtest", attribs = list(attr = "A5:12"))
    expect_error(check_ncname(test_node$attribs), class = "rddi_error")

    test_node <- build_branch_node("branchtest", attribs = list(weight = "A5 12"))
    expect_error(check_ncname(test_node$attribs), class = "rddi_error")

    test_node <- build_branch_node("branchtest", attribs = list(weight = "A5 _13:a"))
    expect_error(check_ncname(test_node$attribs), class = "rddi_error")
})

test_that("XML NMTOKEN variables verification", {
    test_node <- build_branch_node("branchtest", attribs = list(attr = "one two"))
    expect_error(check_nmtoken(test_node$attribs), class = "rddi_error")

    test_node <- build_branch_node("branchtest", attribs = list(attr = "one,two"))    
    expect_error(check_nmtoken(test_node$attribs), class = "rddi_error")
})

test_that("XML anyURI variables verification", {
    test_node <- build_branch_node("branchtest", attribs = list(attr = "www.test.com/test#one#two"))
    expect_error(check_anyURI(test_node$attribs), class = "rddi_error")
})

test_that("elementVersionDate verification", {
    test_node <- build_branch_node("branchtest", attribs = list(attr = "84"))
    expect_error(check_elementVersionDate(test_node$attribs), class = "rddi_error")    

    test_node <- build_branch_node("branchtest", attribs = list(attr = "2004-14"))
    expect_error(check_elementVersionDate(test_node$attribs), class = "rddi_error")    

    test_node <- build_branch_node("branchtest", attribs = list(attr = "2004-02-30"))
    expect_error(check_elementVersionDate(test_node$attribs), class = "rddi_error")    

    test_node <- build_branch_node("branchtest", attribs = list(attr = "2004-02-20 11:63:00"))
    expect_error(check_elementVersionDate(test_node$attribs), class = "rddi_error")    
})

test_that("xs:language variable verification", {
    test_node <- build_branch_node("branchtest", attribs = list(attr = "EN-UnitedStates"))
    expect_error(check_xmlLanguage(test_node$attribs), class = "rddi_error")    

    test_node <- build_branch_node("branchtest", attribs = list(attr = "EN GB"))
    expect_error(check_xmlLanguage(test_node$attribs), class = "rddi_error")    
})