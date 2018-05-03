
test_that("LegendreTest", {

   L <- 36;
   dL <- pi/L;

   for (i in 0:(L-1))
   {
      value <- LegendrePolinom(1,0,-pi/2 + i*dL);
      expect_that(LegendrePolinom(1,0,-pi/2 + i*dL), equals(LegendrePolynom10[i+1]))
   }

})


test_that("SphFuncKTest", {

  l <- pi/4; b <- pi/4;

  expect_that(SphFuncK_NKP(0,0,1,l,b), equals(SphFuncKTestData[1]))
  expect_that(SphFuncK_NKP(1,1,0,l,b), equals(SphFuncKTestData[2]))
  expect_that(SphFuncK_NKP(4,1,1,l,b), equals(SphFuncKTestData[3]))
  expect_that(SphFuncK_NKP(2,2,0,l,b), equals(SphFuncKTestData[4]))
  expect_that(SphFuncK_NKP(4,2,1,l,b), equals(SphFuncKTestData[5]))
  expect_that(SphFuncK_NKP(3,3,1,l,b), equals(SphFuncKTestData[6]))
  expect_that(SphFuncK_NKP(5,3,0,l,b), equals(SphFuncKTestData[7]))
  expect_that(SphFuncK_NKP(2,0,1,l,b), equals(SphFuncKTestData[8]))

  expect_that(SphFuncK_NKP(0,0,1,l,b, where = HEMISPHERE), equals(SphFuncKTestDataHS[1]))
  expect_that(SphFuncK_NKP(1,1,0,l,b, where = HEMISPHERE), equals(SphFuncKTestDataHS[2]))
  expect_that(SphFuncK_NKP(4,1,1,l,b, where = HEMISPHERE), equals(SphFuncKTestDataHS[3]))
  expect_that(SphFuncK_NKP(2,2,0,l,b, where = HEMISPHERE), equals(SphFuncKTestDataHS[4]))
  expect_that(SphFuncK_NKP(4,2,1,l,b, where = HEMISPHERE), equals(SphFuncKTestDataHS[5]))
  expect_that(SphFuncK_NKP(3,3,1,l,b, where = HEMISPHERE), equals(SphFuncKTestDataHS[6]))
  expect_that(SphFuncK_NKP(5,3,0,l,b, where = HEMISPHERE), equals(SphFuncKTestDataHS[7]))
  expect_that(SphFuncK_NKP(2,0,1,l,b, where = HEMISPHERE), equals(SphFuncKTestDataHS[8]))

})

test_that("VSphFuncKTest", {

  l <- c(pi/4, pi/4, 7*pi/4, 7*pi/4); b <- c(pi/4, -pi/4, pi/4, -pi/4)

  index <- list();
  index$n <- c(0, 1, 4, 2, 4, 3, 5, 2)
  index$k <- c(0, 1, 1, 2, 2, 3, 3, 0)
  index$p <- c(1, 0, 1, 0, 1, 1, 0, 1)

  for (i in 1:length(l))
  {
    for (j in 1:length(index$n))
    {
      expect_that(VSphFuncTB_NKP(index$n[j],index$k[j],index$p[j],l[i],b[i]), equals(VSphFuncKTestDataTB[j,i]))
      expect_that(VSphFuncSL_NKP(index$n[j],index$k[j],index$p[j],l[i],b[i]), equals(VSphFuncKTestDataSL[j,i]))
      expect_that(VSphFuncSB_NKP(index$n[j],index$k[j],index$p[j],l[i],b[i]), equals(VSphFuncKTestDataSB[j,i]))
      expect_that(VSphFuncTL_NKP(index$n[j],index$k[j],index$p[j],l[i],b[i]), equals(VSphFuncKTestDataTL[j,i]))

      expect_that(VSphFuncTB_NKP(index$n[j],index$k[j],index$p[j],l[i],b[i], where = HEMISPHERE), equals(VSphFuncKTestDataTB_HS[j,i]))
      expect_that(VSphFuncSL_NKP(index$n[j],index$k[j],index$p[j],l[i],b[i], where = HEMISPHERE), equals(VSphFuncKTestDataSL_HS[j,i]))
      expect_that(VSphFuncSB_NKP(index$n[j],index$k[j],index$p[j],l[i],b[i], where = HEMISPHERE), equals(VSphFuncKTestDataSB_HS[j,i]))
      expect_that(VSphFuncTL_NKP(index$n[j],index$k[j],index$p[j],l[i],b[i], where = HEMISPHERE), equals(VSphFuncKTestDataTL_HS[j,i]))

    }
  }

})


