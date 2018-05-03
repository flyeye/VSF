

PrintS <- function(l, b)
{
  cat("0, 0, 1: l=", l,", b=", b, ", SL=", VSphFuncSL_NKP(0,0,1,l,b), ",  SB=", VSphFuncSB_NKP(0,0,1,l,b), "\n");
  cat("1, 1, 0: l=", l,", b=", b, ", SL=", VSphFuncSL_NKP(1,1,0,l,b), ",  SB=", VSphFuncSB_NKP(1,1,0,l,b), "\n");
  cat("4, 1, 1: l=", l,", b=", b, ", SL=", VSphFuncSL_NKP(4,1,1,l,b), ",  SB=", VSphFuncSB_NKP(4,1,1,l,b), "\n");
  cat("2, 2, 0: l=", l,", b=", b, ", SL=", VSphFuncSL_NKP(2,2,0,l,b), ",  SB=", VSphFuncSB_NKP(2,2,0,l,b), "\n");
  cat("4, 2, 1: l=", l,", b=", b, ", SL=", VSphFuncSL_NKP(4,2,1,l,b), ",  SB=", VSphFuncSB_NKP(4,2,1,l,b), "\n");
  cat("3, 3, 1: l=", l,", b=", b, ", SL=", VSphFuncSL_NKP(3,3,1,l,b), ",  SB=", VSphFuncSB_NKP(3,3,1,l,b), "\n");
  cat("5, 3, 0: l=", l,", b=", b, ", SL=", VSphFuncSL_NKP(5,3,0,l,b), ",  SB=", VSphFuncSB_NKP(5,3,0,l,b), "\n");
  cat("2, 0, 1: l=", l,", b=", b, ", SL=", VSphFuncSL_NKP(2,0,1,l,b), ",  SB=", VSphFuncSB_NKP(2,0,1,l,b), "\n");
}

PrintT <- function(l, b)
{
  cat("0, 0, 1: l=", l,", b=", b, ", TL=", VSphFuncTL_NKP(0,0,1,l,b), ",  TB=", VSphFuncTB_NKP(0,0,1,l,b), "\n");
  cat("1, 1, 0: l=", l,", b=", b, ", TL=", VSphFuncTL_NKP(1,1,0,l,b), ",  TB=", VSphFuncTB_NKP(1,1,0,l,b), "\n");
  cat("4, 1, 1: l=", l,", b=", b, ", TL=", VSphFuncTL_NKP(4,1,1,l,b), ",  TB=", VSphFuncTB_NKP(4,1,1,l,b), "\n");
  cat("2, 2, 0: l=", l,", b=", b, ", TL=", VSphFuncTL_NKP(2,2,0,l,b), ",  TB=", VSphFuncTB_NKP(2,2,0,l,b), "\n");
  cat("4, 2, 1: l=", l,", b=", b, ", TL=", VSphFuncTL_NKP(4,2,1,l,b), ",  TB=", VSphFuncTB_NKP(4,2,1,l,b), "\n");
  cat("3, 3, 1: l=", l,", b=", b, ", TL=", VSphFuncTL_NKP(3,3,1,l,b), ",  TB=", VSphFuncTB_NKP(3,3,1,l,b), "\n");
  cat("5, 3, 0: l=", l,", b=", b, ", TL=", VSphFuncTL_NKP(5,3,0,l,b), ",  TB=", VSphFuncTB_NKP(5,3,0,l,b), "\n");
  cat("2, 0, 1: l=", l,", b=", b, ", TL=", VSphFuncTL_NKP(2,0,1,l,b), ",  TB=", VSphFuncTB_NKP(2,0,1,l,b), "\n");
}

