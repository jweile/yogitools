library(yogitools)

context("getArg")

test_that("getArg require works",{
	expect_error(getArg("foobar",required=TRUE),"Required argument:foobar")
})

test_that("getArg default works",{
	expect_equal(getArg("foobar",default="foo"),"foo")
})


context("Counter")

test_that("Counter is working",{

	counter <- new.counter()

	counter$inc("foo")
	counter$add("bar",4)
	counter$inc("foo")

	expected <- list(foo=2,bar=4)
	expect_equal(counter$ls(),expected)

	expect_equal(counter$export(),"foo=2,bar=4")

})


context("zbind")

test_that("zbind is working",{

	mat1 <- matrix(1:4,nrow=2)
	mat2 <- matrix(5:8,nrow=2)
	mat3 <- zbind(mat1,mat2)

	expect_equal(mat3,array(1:8,dim=c(2,2,2)))

})


context("Exracting groups")

test_that("Local extraction is working",{

	input <- c("There are 2 trees","There are 4 elephants")
	regex <- "(\\d+) (\\w+)"
	out <- extract.groups(input,regex)
	expected <- rbind(c(2,"trees"),c(4,"elephants"))
	expect_equal(out,expected)

})

test_that("Global extraction is working",{

	input <- c("There are 2 trees","There are 4 elephants and 12 tigers")
	regex <- "(\\d+) (\\w+)"
	out <- global.extract.groups(input,regex)
	expected <- list(cbind(2,"trees"),rbind(c(4,"elephants"),c(12,"tigers")))
	expect_equivalent(out,expected)

})


context("MCC")

test_that("MCC is working", {

	patientHasDisease <- sample(c(TRUE,FALSE),100,replace=TRUE)
	patientDiganosticScore <- sapply(patientHasDisease,
		function(d) if (d) rnorm(1,20,3) else rnorm(1,18,3)
	)
	out <- mcc(21,patientDiganosticScore,patientHasDisease)
	expect_gt(out[["mcc"]],0)
	expect_gt(out[["prec"]],0)
	expect_gt(out[["recall"]],0)
})


context("data.frame conversion")

test_that("to.df is working",{
	input <- do.call(rbind,lapply(1:3,function(x) {
		list(a=x,b=x^2)
	}))
	out <- to.df(input)
	expected <- data.frame(a=1:3,b=(1:3)^2)
	expect_equal(out,expected)
}) 

test_that("as.df is working",{
	input <- lapply(1:3,function(x) {
		list(a=x,b=x^2)
	})
	out <- as.df(input)
	expected <- data.frame(a=1:3,b=(1:3)^2)
	expect_equal(out,expected)
})


context("infinite cleanup")

test_that("fin is working",{
	
	out <- fin(c(1,2,NA,3))
	expect_equal(out,1:3)

	out <- fin(data.frame(a=c(1,2,NA,3),b=c(4,5,6,7)))
	expect_equivalent(out,data.frame(a=1:3,b=c(4,5,7)))
})


context("Plate coordinates")

test_that("Quadrant to Coordinate is working",{
	expect_equal(q2c("C_A08"),"B15")
})

test_that("Coordinate to Quadrant is working",{
	expect_equal(c2q("B15"),"C_A08")
})


context("Set of subsets")

test_that("combo is working",{
	out <- combo(1:3)
	expected <- list(1,2,3,c(1,2),c(1,3),c(2,3),c(1,2,3))
	expect_equal(out,expected)
})


context("Color")

test_that("alpha addition is working",{
	out <- colAlpha("chartreuse3",0.5)
	expect_equal(out,"#66CD007F")
})


context("ClusterMap")

test_that("ClusterMap is working",{
	cmap <- new.cluster.map(10)
	cmap$addLink(1,5)
	cmap$addLink(3,5)
	cmap$addLink(1,5)
	cmap$addLink(2,4)
	cmap$addLink(4,7)
	cmap$addLink(4,6)
	cmap$addLink(8,9)
	cmap$addLink(8,10)
	cmap$addLink(1,10)
	out <- cmap$getClusters()
	expect_length(out,2)
	expect_setequal(out[[1]],c(2,4,6,7))
	expect_setequal(out[[2]],c(1,3,5,8,9,10))
})

