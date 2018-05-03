
# ===================================================================
#                            VSF routings
# ===================================================================

HEMISPHERE <- 0;
WHOLE_SPHERE <- 1;

# ------------------------------------------------------------------
#'
#' Calculate a part of the factorial, from i to j
#'
#' @param i - integer, from
#' @param j - integer, to
#'
#' @examples
#' factorialNK(3,7)
#'
#' @return  product from i to j
#'
#' @export
#'
factorialNK <- function(i, j)
{

  if (i>j)
    return(1);

  if ((i == 0) & (j == 0))
    return(1);

  result <- 1;
  for ( k in i:j)
  {
    result  <- result*k;
  }

  return(result)
}

# ===================================================================
#' Calculate of the associated Legendre polynom in b
#'
#' @param n - Legendre polinom index, integer
#' @param k - Legendre polinom index, integer
#' @param b - point where polynom value must be calculated, real number
#'
#' @return
#' value of associated Legendre polinom with indexes n and k in point b
#'
#' @export
#'
#' @examples
#' LegendrePolinom(3, 1, 0.5)
#'
LegendrePolinom <- function (n, k, b)
{
  result <- 0;

  if (n < k)
    return(0);

  if (n == k)
  {
    result <- factorialNK(k+1, 2*k) * (cos(b)^k) / (2^k)
  }
  else if (n == k+1)
  {
    result <- factorialNK(k+2, 2*k+2) * (cos(b)^k)*sin(b) / (2^(k+1))
  }
  else
  {
    result <- sin(b) * (2*n-1) * LegendrePolinom(n-1, k, b)/(n-k) - (n+k-1)*LegendrePolinom(n-2,k,b)/(n-k);
  }

  return(result)
}

# ===================================================================
GetJbyNKP <- function(n, k, p)
{
  return ( n*n + 2*k + p - 1 )
}
# ===================================================================
GetNKPbyJ <- function (j)
{
  n <- floor( sqrt(j) );
  a <- j - n*n + 1;
  k <- a %/% 2;
  p <- a %% 2;

  result <- list();

  result$n <- n
  result$k <- k
  result$p <- p

  return(result)
}
# ===================================================================
#' Calculation of the normalization factor of the spherical harmonic with indexes n and k
#'
#' @param n - index, integer
#' @param k - index, integer
#' @param where - flag, where the value must be calculated, 1 - whole sphere, 0 - hemisphere
#'
#' @return normalization factor value, real
#'
#' @export
SphNormaR <- function(n, k, where = WHOLE_SPHERE)
{
  result <- 0;
  if ( where == WHOLE_SPHERE)
    result <- sqrt((2*n+1)/(4*pi));  # whole sphere
  if ( where == HEMISPHERE)
    result <- sqrt((2*n+1)/(2*pi));    # hemisphere

  if (k>0)
    result <-  result * sqrt(2/factorialNK(n-k+1, n+k));

  return(result)
}

# --------------------------------------------------------------------------
#' Calculation of the spherical harmonic value with indexes n, k and p in the point (l, b)
#'
#' @inheritParams SphNormaR
#'
#' @param p - index, integer, can be 0 or 1
#' @param l - longitude, real
#' @param b - latitude, real
#'
#' @return spherical harmonic value with indexes n, k and p in the point(l, b)
#'
#' @export
SphFuncK_NKP <- function(n, k, p, l, b, where = WHOLE_SPHERE)
{
  result <- SphNormaR(n, k, where = where) * LegendrePolinom(n, k, b);

  if (k==0)
    return(result);

  if (p==0)
  {
    result <- result * sin(k*l)
  }
  else
  {
    result <- result * cos(k*l)
  }

  return(result)
}

#' Calculation of the spherical harmonic value with combined index j in the point (l, b)
#'
#' @param j - combined index, integer
#' @param l - longitude, real
#' @param b - latitude, real
#' @param where - flag, where the value must be calculated, 1 - whole sphere, 0 - hemisphere

#' @return spherical harmonic value with indexes n, k and p in the point(l, b)
#'
#' @export
SphFuncK_J <- function(j, l, b, where = WHOLE_SPHERE)
{
  index <- GetNKPbyJ(j);

  result <- SphFuncK_NKP(index$n, index$k, index$p, l, b, where = where);

  return(result);
}

# ================================================================================
#' Calculate the value of the tesseral vector spherical harmonic in longitude with index j in the point (l, b)
#'
#' @inheritParams SphFuncK_J
#'
#' @return tesseral vector spherical harmonic value with index j in the point(l, b)
#'
#' @export
VSphFuncTL_J <- function(j, l, b, where = WHOLE_SPHERE)
{

  index <- GetNKPbyJ(j);
  result <- VSphFuncTL_NKP(index$n, index$k, index$p, l, b, where = where)
  return(result)
}

#' Calculate the value of the tesseral vector spherical harmonic in longitude with indexes n, k and p in the point (l, b)
#'
#' @inheritParams SphFuncK_NKP
#'
#' @return tesseral vector spherical harmonic value with index j in the point(l, b)
#'@export
VSphFuncTL_NKP <- function(n ,k, p, l, b, where = WHOLE_SPHERE)
{

  if (n==0)
    return(0)

  result <- SphNormaR(n, k, where = where) / sqrt(n*(n+1));
  if ((k==0) & (p==1))
  {
    result <- result * LegendrePolinom(n, 1, b);
    return(result)
  }


  if (k != 0)
  {
    result <- result * (-k*tan(b)*LegendrePolinom(n,k,b) + LegendrePolinom(n, k+1, b));
    if (p==0)
      result <- result*sin(k*l)
    else
      result <- result*cos(k*l);
  }

  return(result)
}

# --------------------------------------------------------------------------
#' Calculate the value of the tesseral vector spherical harmonic in latitude with index j in the point (l, b)
#'
#' @inheritParams SphFuncK_J
#'
#' @return tesseral vector spherical harmonic value with index j in the point(l, b) in longitude
#'@export
VSphFuncTB_J <- function(j, l, b, where = WHOLE_SPHERE)
{
  index <- GetNKPbyJ(j);
  result <- VSphFuncTB_NKP(index$n, index$k, index$p, l, b, where = where);
  return(result)
}

#' Calculate the value of the tesseral vector spherical harmonic in latitude with indexes n, k and p in the point (l, b)
#'
#' @inheritParams SphFuncK_NKP
#'
#' @return tesseral vector spherical harmonic value with index j in the point(l, b) in latitude
#'@export
VSphFuncTB_NKP <- function(n, k, p, l, b, where = WHOLE_SPHERE)
{

  if (n==0)
    return(0)

  if ((k==0) & (p==1))
    return(0);

  result <- 0;

  if (k!=0)
  {
    result <- (SphNormaR(n, k, where = where) / sqrt(n*(n+1))) * (LegendrePolinom(n,k,b) * k / cos(b));
    if (p == 0)
      result <- result * (-cos(k*l))
    else
      result <- result * (sin(k*l));
  }
  return(result)
}

# --------------------------------------------------------------------------
#' Calculate the value of the sectorial vector spherical harmonic in longilude with index j in the point (l, b)
#'
#' @inheritParams SphFuncK_J
#'
#' @return sectorial vector spherical harmonic value with index j in the point(l, b)
#' @export
VSphFuncSL_J<-function(j, l, b, where = WHOLE_SPHERE)
{
  index <- GetNKPbyJ(j);
  result <- VSphFuncSL_NKP(index$n, index$k, index$p, l, b, where = where);
  return(result)
}

#' Calculate the value of the sectorial vector spherical harmonic in longitude with indexes n, k and p in the point (l, b)
#'
#' @inheritParams SphFuncK_NKP
#'
#' @return sectorial vector spherical harmonic value with index j in the point(l, b) in longitude
#'
#' @export
VSphFuncSL_NKP<-function(n, k, p, l, b, where = WHOLE_SPHERE)
{
  result <- 0;
  if (n == 0)
    return(0)

  if ((k == 0) & (p == 1))
      return(0)

  result <- SphNormaR(n, k, where = where) / sqrt(n*(n+1));
  if (k != 0)
  {
    result <- result * LegendrePolinom(n,k,b) * k / cos(b);
    if (p == 0)
      result <- result * (cos(k*l))
    else
      result <- result * (-sin(k*l));
  }
}
# --------------------------------------------------------------------------
#' Calculate the value of the sectorial vector spherical harmonic in latitude with index j in the point (l, b)
#'
#' @inheritParams SphFuncK_J
#'
#' @return sectorial vector spherical harmonic value with index j in the point(l, b) in latitude
#' @export
VSphFuncSB_J <- function(j, l, b, where = WHOLE_SPHERE)
{
  index <- GetNKPbyJ(j);
  result <- VSphFuncSB_NKP(index$n, index$k, index$p, l, b, where = where);
  return(result)
}

#' Calculate the value of the sectorial vector spherical harmonic in latitude with indexes n, k and p in the point (l, b)
#'
#' @inheritParams SphFuncK_NKP
#'
#' @return sectorial vector spherical harmonic value with index j in the point(l, b) in latitude
#'
#'@export

VSphFuncSB_NKP <- function(n, k, p, l, b, where = WHOLE_SPHERE)
{
  result <- 0;

  if (n ==0)
    return(0)

  result <- SphNormaR(n, k, where = where) / sqrt(n*(n+1));
  if ((k ==0) & (p==1))
  {
    result <- result * LegendrePolinom(n, 1, b);
    return(result)
  }

  if (k!=0)
  {
    result <- result * (-k*tan(b)*LegendrePolinom(n,k,b) + LegendrePolinom(n, k+1, b));
    if (p==0)
      result <- result*sin(k*l)
    else
      result <- result*cos(k*l);
  }

  return(result)
}

# ============================================================================

ro <- function(n, k, where = WHOLE_SPHERE)
{
  if (n != 0)
    result <- SphNormaR(n,k, where = where)/sqrt(n*(n+1))
  else
    result <- 0;

  return(result)
}

FL <- function(l, b, i)
{
  result <- switch(i,
  (-1)*VSphFuncSL_NKP(1,1,1, l, b)/ro(1,1),
  (-1)*VSphFuncSL_NKP(1,1,0, l, b)/ro(1,1),
  (-1)*VSphFuncSL_NKP(1,0,1, l, b)/ro(1,0),
  VSphFuncTL_NKP(1,1,1, l, b)/ro(1,1),
  VSphFuncTL_NKP(1,1,0, l, b)/ro(1,1),
  VSphFuncTL_NKP(1,0,1, l, b)/ro(1,0),
  (1/3)*VSphFuncSL_NKP(2,1,1, l, b)/ro(2,1),
  (1/3)*VSphFuncSL_NKP(2,1,0, l, b)/ro(2,1),
  (1/6)*VSphFuncSL_NKP(2,2,0, l, b)/ro(2,2),
  (1/12)*VSphFuncSL_NKP(2,2,1, l, b)/ro(2,2),
  (1/3)*VSphFuncSL_NKP(2,0,1, l, b)/ro(2,0),
  0)

  return(result)

}

FB <- function(l, b, i)
{
  result <- switch(i,
  (-1)*VSphFuncSB_NKP(1,1,1, l, b)/ro(1,1),
  (-1)*VSphFuncSB_NKP(1,1,0, l, b)/ro(1,1),
  (-1)*VSphFuncSB_NKP(1,0,1, l, b)/ro(1,0),
  VSphFuncTB_NKP(1,1,1, l, b)/ro(1,1),
  VSphFuncTB_NKP(1,1,0, l, b)/ro(1,1),
  VSphFuncTB_NKP(1,0,1, l, b)/ro(1,0),
  (1/3)*VSphFuncSB_NKP(2,1,1, l, b)/ro(2,1),
  (1/3)*VSphFuncSB_NKP(2,1,0, l, b)/ro(2,1),
  (1/6)*VSphFuncSB_NKP(2,2,0, l, b)/ro(2,2),
  (1/12)*VSphFuncSB_NKP(2,2,1, l, b)/ro(2,2),
  (1/3)*VSphFuncSB_NKP(2,0,1, l, b)/ro(2,0),
  0)

  return(result)
}

FR <- function(l, b, i)
{
  result <- switch(i,
  -SphFuncK_NKP(1, 1, 1, l, b)/SphNormaR(1,1),
  -SphFuncK_NKP(1, 1, 0, l, b)/SphNormaR(1,1),
  -SphFuncK_NKP(1, 0, 1, l, b)/SphNormaR(1,0),
  0,
  0,
  0,
  (2/3)*SphFuncK_NKP(2, 1, 1, l, b)/SphNormaR(2,1),
  (2/3)*SphFuncK_NKP(2, 1, 0, l, b)/SphNormaR(2,1),
  (1/3)*SphFuncK_NKP(2, 2, 0, l, b)/SphNormaR(2,2),
  (1/6)*SphFuncK_NKP(2, 2, 1, l, b)/SphNormaR(2,2),
  (2/3)*SphFuncK_NKP(2, 0, 1, l, b)/SphNormaR(2,0),
  SphFuncK_NKP(0, 0, 1, l, b)/SphNormaR(0,0))

  return(result)
}


# ---------------------------------------------------------------------------------
#' Calculate a scalar product of the spherical harmonics with indexes i and j
#'
#' @param i - index, integer
#' @param j - index, integer
#' @param where - flag, where the value must be calculated, 1 - whole sphere, 0 - hemisphere
#'
#'@export
FScalarProduct <- function(i, j, where = WHOLE_SPHERE)
{
  B <- 36;   dB <- pi/B;
  L <- 48;   dL <- 2*pi/L;

  value <- 0;

  b0 <- 0;
  if ( where == WHOLE_SPHERE)
    b0 <- 0;          # whole sphere
  if ( where == HEMISPHERE)
    b0 <- (B %/% 2);  # hemisphere


  for (q1 in 0:(L-1))
  {
    lq <- dL*(q1+0.5);
    for (q2  in 0:((B %/% 2)-1))
    {
      bq <- (pi/2) - dB*(q2+0.5);

      value <- value + (FL(lq, bq, i)*FL(lq, bq, j) +
                          FB(lq, bq, i)*FB(lq, bq, j))*cos(bq)*(2*pi*pi/(L*B));
    }
  }

  return(value);
}

# ==================================================================================
#' Calculation a of the scalar product of the vector sperical harmonics with indexes i and j
#'
#' @param i - index, integer
#' @param j - index, integer
#' @param what - what kind of function must be used
#' @param where - flag, where the value must be calculated, 1 - whole sphere, 0 - hemisphere
#'

#'@export
VSphScalarProduct <- function(i, j, what, where = WHOLE_SPHERE)
{
  B <- 144;   dB <- pi/B;
  L <- 192;   dL <- 2*pi/L;

  value <- 0;

  index1 <- GetNKPbyJ(i);
  index2 <- GetNKPbyJ(j);

  b0 <- 0;
  if ( where == WHOLE_SPHERE)
    b0 <- 0;          # whole sphere
  if ( where == HEMISPHERE)
    b0 <- (B %/% 2);  # hemisphere


  for (q1 in 0:(L-1))
  {

    lq <- dL*(q1+0.5);
    for (q2 in b0:(B-1))
    {
      bq <- (pi/2) - dB*(q2+0.5);

      bq2 <- asin(2*sin(bq)+1);

      value <- switch(what,
      #        (Ti,Tj)
      1: value + (VSphFuncTL_NKP(index1$n, index1$k, index1$p, lq, bq2)*VSphFuncTL_NKP(index2$n, index2$k, index2$p, lq, bq2) +
                             VSphFuncTB_NKP(index1$n, index1$k, index1$p, lq, bq2)*VSphFuncTB_NKP(index2$n, index2$k, index2$p, lq, bq2))*cos(bq)*(2*pi*pi/(L*B)),
      #        (Si,Sj)
      2: value + (VSphFuncSL_NKP(index1$n, index1$k, index1$p, lq, bq2)*VSphFuncSL_NKP(index2$n, index2$k, index2$p, lq, bq2) +
                            VSphFuncSB_NKP(index1$n, index1$k, index1$p, lq, bq2)*VSphFuncSB_NKP(index2$n, index2$k, index2$p, lq, bq2))*cos(bq)*(2*pi*pi/(L*B)),
      #        (Vi,Vj)
      3: value + (SphFuncK_NKP(index1$n, index1$k, index1$p, lq, bq2)*SphFuncK_NKP(index2$n, index2$k, index2$p, lq, bq2))*cos(bq)*(2*pi*pi/(L*B)),

      #        (Ti,Sj)
      4: value + (VSphFuncTL_NKP(index1$n, index1$k, index1$p, lq, bq2)*VSphFuncSL_NKP(index2$n, index2$k, index2$p, lq, bq2) +
                             VSphFuncTB_NKP(index1$n, index1$k, index1$p, lq, bq2)*VSphFuncSB_NKP(index2$n, index2$k, index2$p, lq, bq2))*cos(bq)*(2*pi*pi/(L*B)),
      #        (Fi,Si)
      5: value + (FL(lq, bq, i)*VSphFuncSL_NKP(index2$n, index2$k, index2$p, lq, bq) +
                             FB(lq, bq, i)*VSphFuncSB_NKP(index2$n, index2$k, index2$p, lq, bq))*cos(bq)*(2*pi*pi/(L*B)),
      #        (Fi,Ti)
      6: value + (FL(lq, bq, i)*VSphFuncTL_NKP(index2$n, index2$k, index2$p, lq, bq) +
                             FB(lq, bq, i)*VSphFuncTB_NKP(index2$n, index2$k, index2$p, lq, bq))*cos(bq)*(2*pi*pi/(L*B)),
      #        (Fi,Vi)
      7: value + (FR(lq, bq, i)*SphFuncK_NKP(index2$n, index2$k, index2$p, lq, bq))*cos(bq)*(2*pi*pi/(L*B))

      # #        (FOMi,Si)
      # 8: value + (FOgorodnikovMilnL(lq, bq, 1, i)*VSphFuncSL_NKP(index2$n, index2$k, index2$p, lq, bq2) +
      #                        FOgorodnikovMilnB(lq, bq, 1, i)*VSphFuncSB_NKP(index2$n, index2$k, index2$p, lq, bq2))*cos(bq)*(2*pi*pi/(L*B)),
      # #        (FOMi,Ti)
      # 9: value + (FOgorodnikovMilnL(lq, bq, 1, i)*VSphFuncTL_NKP(index2$n, index2$k, index2$p, lq, bq2) +
      #                        FOgorodnikovMilnB(lq, bq, 1, i)*VSphFuncTB_NKP(index2$n, index2$k, index2$p, lq, bq2))*cos(bq)*(2*pi*pi/(L*B)),
      # #        (FOMi,Vi)
      # 10: value + (FOgorodnikovMilnR(lq, bq, 1, i)*SphFuncK_NKP(index2$n, index2$k, index2$p, lq, bq2))*cos(bq)*(2*pi*pi/(L*B)),
      #
      # 11: value + (FOgorodnikovMilnL(lq, bq, 1, i)*SphFuncK_NKP(index2$n, index2$k, index2$p, lq, bq2))*cos(bq)*(2*pi*pi/(L*B)),
      #
      # 12: value + (FOgorodnikovMilnB(lq, bq, 1, i)*SphFuncK_NKP(index2$n, index2$k, index2$p, lq, bq2))*cos(bq)*(2*pi*pi/(L*B),
      #
      # # (Vi,Ti)
      # 13: value + (VSphFuncSL(index1$n, index1$k, index1$p, lq, bq)*VSphFuncSL(index2$n, index2$k, index2$p, lq, bq) +
      #               VSphFuncSB(index1$n, index1$k, index1$p, lq, bq)*VSphFuncSB(index2$n, index2$k, index2$p, lq, bq))*CosD(bq)*(2*pi*pi/(L*B));
      )
    }
  }

  return(value);
}

