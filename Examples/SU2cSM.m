<|"BetaFunctions" -> {g1sq -> (41*g1sq^2)/(48*Pi^2), 
   g2sq -> (-19*g2sq^2)/(48*Pi^2), gXsq -> (-41*gXsq^2)/(48*Pi^2), 
   g3sq -> (-7*g3sq^2)/(8*Pi^2), 
   a2 -> (8*a2^2 + 20*a2*b4 - 33*a2*gXsq + 12*gXsq^2 + 24*a2*\[Lambda]\[Psi])/
     (32*Pi^2), b4 -> (a2^2 + 11*b4^2 - 12*b4*gXsq + 6*gXsq^2)/(8*Pi^2), 
   \[Lambda]h -> (3*g1sq^2 + 9*g2sq^2 + 6*g1sq*(g2sq - 4*\[Lambda]h) - 
      72*g2sq*\[Lambda]h + 96*\[Lambda]h*(yt^2 + 2*\[Lambda]h) + 
      16*(-3*yt^4 + \[Lambda]h\[Psi]^2))/(128*Pi^2), 
   \[Lambda]h\[Psi] -> (\[Lambda]h\[Psi]*(-3*g1sq - 9*g2sq - 9*gXsq + 
       12*yt^2 + 24*\[Lambda]h + 8*\[Lambda]h\[Psi] + 24*\[Lambda]\[Psi]))/
     (32*Pi^2), \[Lambda]\[Psi] -> 
    (12*a2^2 + 9*gXsq^2 + 16*\[Lambda]h\[Psi]^2 - 72*gXsq*\[Lambda]\[Psi] + 
      192*\[Lambda]\[Psi]^2)/(128*Pi^2), 
   yt -> (yt*(-17*g1sq - 27*g2sq - 96*g3sq + 54*yt^2))/(192*Pi^2), 
   m1sq -> (-3*m1sq*(g1sq + 3*g2sq - 4*(yt^2 + 2*\[Lambda]h)) + 
      8*m2sq*\[Lambda]h\[Psi])/(32*Pi^2), 
   m2sq -> (-9*gXsq*m2sq + 6*a2*mx0sq + 8*m1sq*\[Lambda]h\[Psi] + 
      24*m2sq*\[Lambda]\[Psi])/(32*Pi^2), 
   mx0sq -> (2*a2*m2sq + 5*b4*mx0sq - 6*gXsq*mx0sq)/(8*Pi^2)}, 
 "BetaFunctions3d" -> 
  {m1sq -> (5*g1sq3d^2 - 39*g2sq3d^2 + 6*g1sq3d*(3*g2sq3d - 8*\[Lambda]h3d) - 
      48*g2sq3d*(3*\[Lambda]h3d + 2*\[Lambda]VL[4]) + 
      8*(24*\[Lambda]h3d^2 + 4*\[Lambda]h\[Psi]3d^2 + 8*\[Lambda]VL[3]*
         (-6*g3sq3d + \[Lambda]VL[3]) + 3*\[Lambda]VL[4]^2 + 
        \[Lambda]VL[5]^2 + 6*\[Lambda]VL[6]^2 + 3*\[Lambda]VL[9]^2 - 
        6*gXsq3d*(\[Lambda]h\[Psi]3d + 2*\[Lambda]VL[9])))/(256*Pi^2), 
   m2sq -> (24*a23d^2 - 96*a23d*gXsq3d - 27*gXsq3d^2 - 
      48*gXsq3d*(3*\[Lambda]\[Psi]3d + 2*\[Lambda]VL[10]) + 
      8*(-2*g1sq3d*\[Lambda]h\[Psi]3d + 4*\[Lambda]h\[Psi]3d^2 + 
        24*\[Lambda]\[Psi]3d^2 + \[Lambda]VL[7]^2 - 
        6*g2sq3d*(\[Lambda]h\[Psi]3d + 2*\[Lambda]VL[8]) + 
        3*(\[Lambda]VL[8]^2 + \[Lambda]VL[10]^2)))/(256*Pi^2), 
   mx0sq -> (4*a23d^2 + 20*b43d^2 - 6*a23d*gXsq3d - 40*b43d*gXsq3d + 
      6*gXsq3d^2 + 8*gXsq3d*\[Lambda]VL[1] + 8*\[Lambda]VL[1]^2 - 
      12*gXsq3d*\[Lambda]VL[2] - 4*\[Lambda]VL[1]*\[Lambda]VL[2] + 
      3*\[Lambda]VL[2]^2)/(32*Pi^2)}, "Constants3d" -> 
  {Lb -> 2*EulerGamma - 2*Log[4*Pi] + Log[\[Mu]^2/T^2], 
   Lf -> 2*EulerGamma + 4*Log[2] - 2*Log[4*Pi] + Log[\[Mu]^2/T^2]}, 
 "Couplings3d" -> {g1sq3d -> g1sq*T - (g1sq^2*(Lb + 40*Lf)*T)/(96*Pi^2), 
   g2sq3d -> g2sq*T + (g2sq^2*(4 + 43*Lb - 24*Lf)*T)/(96*Pi^2), 
   gXsq3d -> gXsq*T + (gXsq^2*(4 + 41*Lb)*T)/(96*Pi^2), 
   g3sq3d -> g3sq*T + (g3sq^2*(1 + 11*Lb - 4*Lf)*T)/(16*Pi^2), 
   a23d -> (T*(gXsq^2*(8 - 12*Lb) + 33*a2*gXsq*Lb - 
       4*a2*(2*a2*Lb + 5*b4*Lb - 16*Pi^2 + 6*Lb*\[Lambda]\[Psi])))/(64*Pi^2), 
   b43d -> ((gXsq^2*(4 - 6*Lb) - (a2^2 + 11*b4^2)*Lb + 12*b4*gXsq*Lb + 
       16*b4*Pi^2)*T)/(16*Pi^2), \[Lambda]h3d -> 
    (T*((g1sq^2 + 2*g1sq*g2sq + 3*g2sq^2)*(2 - 3*Lb) + 48*Lf*yt^4 + 
       256*Pi^2*\[Lambda]h + 24*(g1sq*Lb + 3*g2sq*Lb - 4*Lf*yt^2)*
        \[Lambda]h - 16*Lb*(12*\[Lambda]h^2 + \[Lambda]h\[Psi]^2)))/
     (256*Pi^2), \[Lambda]h\[Psi]3d -> 
    (T*\[Lambda]h\[Psi]*(64*Pi^2 - 12*Lf*yt^2 + 
       Lb*(3*g1sq + 9*g2sq + 9*gXsq - 8*(3*\[Lambda]h + \[Lambda]h\[Psi] + 
           3*\[Lambda]\[Psi]))))/(64*Pi^2), \[Lambda]\[Psi]3d -> 
    (T*(gXsq^2*(6 - 9*Lb) + 72*gXsq*Lb*\[Lambda]\[Psi] - 
       4*(3*a2^2*Lb + 4*Lb*\[Lambda]h\[Psi]^2 - 64*Pi^2*\[Lambda]\[Psi] + 
         48*Lb*\[Lambda]\[Psi]^2)))/(256*Pi^2)}, 
 "TempScalar3d" -> {\[Lambda]VLL[1] -> (-371*g1sq^2*T)/(72*Pi^2), 
   \[Lambda]VLL[2] -> (-3*g1sq*g2sq*T)/(8*Pi^2), 
   \[Lambda]VLL[3] -> (5*g2sq^2*T)/(24*Pi^2), \[Lambda]VLL[4] -> 
    (-11*g1sq*g3sq*T)/(12*Pi^2), \[Lambda]VLL[5] -> 
    (-3*g2sq*g3sq*T)/(4*Pi^2), \[Lambda]VLL[6] -> 
    -1/4*(Sqrt[g1sq]*g3sq^(3/2)*T)/Pi^2, \[Lambda]VLL[7] -> 
    (g3sq^2*T)/(4*Pi^2), \[Lambda]VLL[8] -> (25*gXsq^2*T)/(24*Pi^2), 
   \[Lambda]VL[1] -> (gXsq*(24*b4 + gXsq*(-46 + 41*Lb) + 96*Pi^2)*T)/
     (96*Pi^2), \[Lambda]VL[2] -> 
    (gXsq*(6*a2 + 48*b4 + 26*gXsq + 41*gXsq*Lb + 96*Pi^2)*T)/(48*Pi^2), 
   \[Lambda]VL[3] -> -1/4*(g3sq*T*yt^2)/Pi^2, 
   \[Lambda]VL[4] -> (g2sq*T*(3*g1sq + g2sq*(75 + 43*Lb - 24*Lf) + 
       12*(8*Pi^2 - 3*yt^2 + 6*\[Lambda]h)))/(192*Pi^2), 
   \[Lambda]VL[5] -> -1/192*(g1sq*T*(-9*g2sq + g1sq*(-41 + Lb + 40*Lf) - 
        96*Pi^2 + 68*yt^2 - 72*\[Lambda]h))/Pi^2, 
   \[Lambda]VL[6] -> -1/384*(Sqrt[g1sq]*Sqrt[g2sq]*T*
       (g2sq*(-12 - 43*Lb + 24*Lf) + g1sq*(-44 + Lb + 40*Lf) - 
        24*(8*Pi^2 + yt^2 + 2*\[Lambda]h)))/Pi^2, 
   \[Lambda]VL[7] -> (g1sq*T*\[Lambda]h\[Psi])/(8*Pi^2), 
   \[Lambda]VL[8] -> (g2sq*T*\[Lambda]h\[Psi])/(8*Pi^2), 
   \[Lambda]VL[9] -> (gXsq*T*\[Lambda]h\[Psi])/(8*Pi^2), 
   \[Lambda]VL[10] -> (gXsq*T*(48*a2 + gXsq*(47 + 41*Lb) + 96*Pi^2 + 
       72*\[Lambda]\[Psi]))/(192*Pi^2)}, "DebyeMass3dLO" -> 
  {\[Mu]sqSU2P1 -> (11*g2sq*T^2)/6, \[Mu]sqSU2P2 -> (7*gXsq*T^2)/6, 
   \[Mu]sqSU3 -> 2*g3sq*T^2, \[Mu]sqU1 -> (11*g1sq*T^2)/6, 
   \[Mu]SU2Mix1 -> 0}, "DebyeMass3dNLO" -> 
  {\[Mu]sqSU2P1 -> 
    (g2sq*(144*m1sq + T^2*(-27*g1sq + 6*(-72*g3sq - 3*yt^2 + 12*\[Lambda]h + 
           4*\[Lambda]h\[Psi]) + 11*g2sq*(57 + 76*EulerGamma - 344*Log[2])) + 
       4*g2sq*T^2*(-63*Log[Pi*T] + 63*Log[\[Mu]] + 146*Log[\[Mu]/(Pi*T)])))/
     (1152*Pi^2), \[Mu]sqSU2P2 -> 
    (gXsq*(144*m2sq + 288*mx0sq + T^2*(66*a2 + 120*b4 + 
         24*(\[Lambda]h\[Psi] + 3*\[Lambda]\[Psi]) + 
         gXsq*(367 + 1148*EulerGamma - 2296*Log[2])) + 
       4*gXsq*T^2*(-111*Log[Pi*T] + 111*Log[\[Mu]] + 176*Log[\[Mu]/(Pi*T)])))/
     (1152*Pi^2), \[Mu]sqSU3 -> 
    (g3sq*T^2*(-11*g1sq + 3*(-9*g2sq - 4*yt^2 + 8*g3sq*(5 + 14*EulerGamma - 
           22*Log[2])) + 24*g3sq*(-3*Log[Pi*T] + 3*Log[\[Mu]] + 
         11*Log[\[Mu]/(4*Pi*T)])))/(192*Pi^2), 
   \[Mu]sqU1 -> (g1sq*(144*m1sq + T^2*(-81*g2sq - 528*g3sq - 66*yt^2 + 
         72*\[Lambda]h + 24*\[Lambda]h\[Psi] + g1sq*(465 - 1804*EulerGamma + 
           44*Log[4*Pi^41])) + 4*g1sq*T^2*(401*(Log[T] - Log[\[Mu]]) - 
         50*Log[\[Mu]/T])))/(1152*Pi^2), \[Mu]SU2Mix1 -> 0}, 
 "ScalarMass3dLO" -> 
  {m1sq3d -> m1sq + (T^2*(3*g1sq + 9*g2sq + 12*yt^2 + 24*\[Lambda]h + 
        8*\[Lambda]h\[Psi]))/48, 
   m2sq3d -> m2sq + (T^2*(6*a2 + 9*gXsq + 8*\[Lambda]h\[Psi] + 
        24*\[Lambda]\[Psi]))/48, mx0sq3d -> 
    mx0sq + ((2*a2 + 5*b4 + 6*gXsq)*T^2)/12}, 
 "ScalarMass3dNLO" -> 
  {m1sq3d -> 
    (g1sq*(Lb*(216*m1sq + T^2*(216*g2sq + 47*yt^2 + 
            36*(-6*\[Lambda]h + \[Lambda]h\[Psi]))) + 
        T^2*(11*(-6 + 5*Lf)*yt^2 + 72*\[Lambda]h*(1 + 6*EulerGamma - 
            72*Log[Glaisher]) - 54*g2sq*(1 + 5*EulerGamma - 
            60*Log[Glaisher]))) + g1sq^2*T^2*(41 - 63*EulerGamma - 147*Lb + 
        60*Lf + 756*Log[Glaisher]) + 
      3*(9*g2sq*(24*Lb*m1sq - (2 + Lf)*T^2*yt^2 + 
          Lb*T^2*(7*yt^2 + 4*(-6*\[Lambda]h + \[Lambda]h\[Psi])) + 
          8*T^2*\[Lambda]h*(1 + 6*EulerGamma - 72*Log[Glaisher])) + 
        g2sq^2*T^2*(191 + 243*EulerGamma - 249*Lb + 36*Lf - 
          2916*Log[Glaisher]) - 4*(16*g3sq*(3 + Lb)*T^2*yt^2 - 
          9*Lb*T^2*yt^4 + 144*Lb*m1sq*\[Lambda]h + 54*Lb*T^2*yt^2*
           \[Lambda]h + 144*EulerGamma*T^2*\[Lambda]h^2 + 
          48*Lb*m2sq*\[Lambda]h\[Psi] - 6*gXsq*T^2*\[Lambda]h\[Psi] - 
          36*EulerGamma*gXsq*T^2*\[Lambda]h\[Psi] + 6*a2*Lb*T^2*
           \[Lambda]h\[Psi] + 27*gXsq*Lb*T^2*\[Lambda]h\[Psi] + 
          24*Lb*T^2*\[Lambda]h*\[Lambda]h\[Psi] + 24*EulerGamma*T^2*
           \[Lambda]h\[Psi]^2 - 4*Lb*T^2*\[Lambda]h\[Psi]^2 + 
          2*Lf*yt^2*(36*m1sq + T^2*(-32*g3sq + 9*\[Lambda]h + 
              6*\[Lambda]h\[Psi])) + 24*Lb*T^2*\[Lambda]h\[Psi]*
           \[Lambda]\[Psi] - 1728*T^2*\[Lambda]h^2*Log[Glaisher] + 
          432*gXsq*T^2*\[Lambda]h\[Psi]*Log[Glaisher] - 
          288*T^2*\[Lambda]h\[Psi]^2*Log[Glaisher])) + 
      18*Log[\[Mu]3/\[Mu]]*(5*g1sq3d^2 - 39*g2sq3d^2 + 
        6*g1sq3d*(3*g2sq3d - 8*\[Lambda]h3d) - 48*g2sq3d*
         (3*\[Lambda]h3d + 2*\[Lambda]VL[4]) + 
        8*(24*\[Lambda]h3d^2 + 4*\[Lambda]h\[Psi]3d^2 - 
          48*g3sq3d*\[Lambda]VL[3] + 8*\[Lambda]VL[3]^2 + 
          3*\[Lambda]VL[4]^2 + \[Lambda]VL[5]^2 + 6*\[Lambda]VL[6]^2 + 
          3*\[Lambda]VL[9]^2 - 6*gXsq3d*(\[Lambda]h\[Psi]3d + 
            2*\[Lambda]VL[9]))))/(4608*Pi^2), 
   m2sq3d -> (3*gXsq*(72*Lb*m2sq - 3*Lb*T^2*(21*a2 - 4*\[Lambda]h\[Psi] + 
          24*\[Lambda]\[Psi]) + 8*T^2*(2*a2 + 3*\[Lambda]\[Psi])*
         (1 + 6*EulerGamma - 72*Log[Glaisher])) - 
      4*(Lb*(48*m1sq*\[Lambda]h\[Psi] + 9*g1sq*T^2*\[Lambda]h\[Psi] + 
          27*g2sq*T^2*\[Lambda]h\[Psi] + 18*T^2*yt^2*\[Lambda]h\[Psi] + 
          24*T^2*\[Lambda]h*\[Lambda]h\[Psi] - 4*T^2*\[Lambda]h\[Psi]^2 + 
          144*m2sq*\[Lambda]\[Psi] + 24*T^2*\[Lambda]h\[Psi]*
           \[Lambda]\[Psi]) + 3*a2*Lb*(12*mx0sq + 
          T^2*(5*b4 + 6*\[Lambda]\[Psi])) - 
        2*T^2*(3*(Lf*yt^2*\[Lambda]h\[Psi] + g2sq*\[Lambda]h\[Psi]*
             (1 + 6*EulerGamma - 72*Log[Glaisher]) - 
            4*(\[Lambda]h\[Psi]^2 + 6*\[Lambda]\[Psi]^2)*(EulerGamma - 
              12*Log[Glaisher])) + g1sq*\[Lambda]h\[Psi]*(1 + 6*EulerGamma - 
            72*Log[Glaisher])) + 3*a2^2*T^2*(6*EulerGamma - Lb - 
          72*Log[Glaisher])) + gXsq^2*T^2*(187 + 207*EulerGamma - 147*Lb - 
        2484*Log[Glaisher]) + 6*Log[\[Mu]3/\[Mu]]*
       (24*a23d^2 - 96*a23d*gXsq3d - 27*gXsq3d^2 - 
        16*g1sq3d*\[Lambda]h\[Psi]3d + 32*\[Lambda]h\[Psi]3d^2 + 
        192*\[Lambda]\[Psi]3d^2 + 8*\[Lambda]VL[7]^2 + 24*\[Lambda]VL[8]^2 - 
        48*g2sq3d*(\[Lambda]h\[Psi]3d + 2*\[Lambda]VL[8]) + 
        24*\[Lambda]VL[10]^2 - 48*gXsq3d*(3*\[Lambda]\[Psi]3d + 
          2*\[Lambda]VL[10])))/(1536*Pi^2), 
   mx0sq3d -> (-360*b4*Lb*mx0sq + 432*gXsq*Lb*mx0sq - 
      360*b4^2*EulerGamma*T^2 + 120*b4*gXsq*T^2 + 720*b4*EulerGamma*gXsq*
       T^2 + 284*gXsq^2*T^2 - 36*EulerGamma*gXsq^2*T^2 + 30*b4^2*Lb*T^2 - 
      360*b4*gXsq*Lb*T^2 + 66*gXsq^2*Lb*T^2 - 
      3*a2*(48*Lb*m2sq + Lb*T^2*(20*b4 + 3*gXsq + 8*\[Lambda]h\[Psi] + 
          24*\[Lambda]\[Psi]) - 6*gXsq*T^2*(1 + 6*EulerGamma - 
          72*Log[Glaisher])) + 4320*b4^2*T^2*Log[Glaisher] - 
      8640*b4*gXsq*T^2*Log[Glaisher] + 432*gXsq^2*T^2*Log[Glaisher] + 
      18*a2^2*T^2*(-4*EulerGamma + Lb + 48*Log[Glaisher]) + 
      36*Log[\[Mu]3/\[Mu]]*(4*a23d^2 + 20*b43d^2 - 6*a23d*gXsq3d - 
        40*b43d*gXsq3d + 6*gXsq3d^2 + 8*gXsq3d*\[Lambda]VL[1] + 
        8*\[Lambda]VL[1]^2 - 12*gXsq3d*\[Lambda]VL[2] - 
        4*\[Lambda]VL[1]*\[Lambda]VL[2] + 3*\[Lambda]VL[2]^2))/(1152*Pi^2)}, 
 "Pressure3dLO" -> (479*Pi^2*T^4)/360, "Pressure3dNLO" -> 
  -1/576*(T^2*(96*m1sq + 96*m2sq + 72*mx0sq + 
     T^2*(12*a2 + 15*b4 + 55*g1sq + 129*g2sq + 336*g3sq + 69*gXsq + 30*yt^2 + 
       24*\[Lambda]h + 16*\[Lambda]h\[Psi] + 24*\[Lambda]\[Psi]))), 
 "Pressure3dNNLO" -> -1/552960*(8640*Lb*(4*m1sq^2 + 4*m2sq^2 + 3*mx0sq^2) + 
     60*T^4*(33*a2*gXsq + 4*(15*b4*gXsq + g1sq*(3*\[Lambda]h + 
           \[Lambda]h\[Psi]) + 3*g2sq*(3*\[Lambda]h + \[Lambda]h\[Psi]) + 
         3*gXsq*(\[Lambda]h\[Psi] + 3*\[Lambda]\[Psi])))*
      (2 + 12*EulerGamma - 9*Lb - 144*Log[Glaisher]) + 
     1440*T^4*yt^2*(3*\[Lambda]h + \[Lambda]h\[Psi])*
      (8*EulerGamma - 7*Lb + Lf - 96*Log[Glaisher]) + 
     8640*m1sq*T^2*yt^2*(4*EulerGamma - 5*Lb + Lf - 48*Log[Glaisher]) + 
     2880*(g1sq*m1sq + 3*g2sq*m1sq + 3*gXsq*(m2sq + 2*mx0sq))*T^2*
      (1 + 3*EulerGamma - 3*Lb - 36*Log[Glaisher]) + 
     1440*T^2*(3*g1sq*m1sq + 9*g2sq*m1sq - 6*a2*m2sq + 9*gXsq*m2sq - 
       6*a2*mx0sq - 15*b4*mx0sq + 18*gXsq*mx0sq - 12*m1sq*yt^2 - 
       24*m1sq*\[Lambda]h - 8*m1sq*\[Lambda]h\[Psi] - 
       8*m2sq*\[Lambda]h\[Psi] - 24*m2sq*\[Lambda]\[Psi])*
      (2*EulerGamma - Lb - 24*Log[Glaisher]) + 
     2880*T^2*(15*b4*mx0sq + 6*a2*(m2sq + mx0sq) + 
       8*(3*m1sq*\[Lambda]h + m1sq*\[Lambda]h\[Psi] + m2sq*\[Lambda]h\[Psi] + 
         3*m2sq*\[Lambda]\[Psi]))*(EulerGamma - Lb - 12*Log[Glaisher]) + 
     60*T^4*(-369*g1sq^2 - (4510*EulerGamma*g1sq^2)/3 + 475*g2sq^2 + 
       1634*EulerGamma*g2sq^2 + 3024*g3sq^2 + 9408*EulerGamma*g3sq^2 + 
       574*gXsq^2 + 1886*EulerGamma*gXsq^2 + (2870*g1sq^2*Lb)/3 - 
       988*g2sq^2*Lb - 5712*g3sq^2*Lb - 943*gXsq^2*Lb - 205*g1sq^2*Lf + 
       171*g2sq^2*Lf + 1008*g3sq^2*Lf + 170*EulerGamma*g1sq*yt^2 + 
       270*EulerGamma*g2sq*yt^2 + 960*EulerGamma*g3sq*yt^2 - 
       (221*g1sq*Lb*yt^2)/2 - (351*g2sq*Lb*yt^2)/2 - 624*g3sq*Lb*yt^2 + 
       (51*g1sq*Lf*yt^2)/2 + (81*g2sq*Lf*yt^2)/2 + 144*g3sq*Lf*yt^2 - 
       540*EulerGamma*yt^4 + 351*Lb*yt^4 - 81*Lf*yt^4 + 
       2736*a2^2*Log[Glaisher] + 2880*a2*b4*Log[Glaisher] + 
       7920*b4^2*Log[Glaisher] + 18256*g1sq^2*Log[Glaisher] + 
       432*g1sq*g2sq*Log[Glaisher] - 18960*g2sq^2*Log[Glaisher] - 
       112896*g3sq^2*Log[Glaisher] - 4752*a2*gXsq*Log[Glaisher] - 
       8640*b4*gXsq*Log[Glaisher] - 15936*gXsq^2*Log[Glaisher] - 
       2040*g1sq*yt^2*Log[Glaisher] - 3240*g2sq*yt^2*Log[Glaisher] - 
       11520*g3sq*yt^2*Log[Glaisher] + 3024*yt^4*Log[Glaisher] - 
       1728*g1sq*\[Lambda]h*Log[Glaisher] - 5184*g2sq*\[Lambda]h*
        Log[Glaisher] + 6912*yt^2*\[Lambda]h*Log[Glaisher] + 
       13824*\[Lambda]h^2*Log[Glaisher] + 1152*a2*\[Lambda]h\[Psi]*
        Log[Glaisher] - 576*g1sq*\[Lambda]h\[Psi]*Log[Glaisher] - 
       1728*g2sq*\[Lambda]h\[Psi]*Log[Glaisher] - 1728*gXsq*\[Lambda]h\[Psi]*
        Log[Glaisher] + 2304*yt^2*\[Lambda]h\[Psi]*Log[Glaisher] + 
       4608*\[Lambda]h*\[Lambda]h\[Psi]*Log[Glaisher] + 
       3840*\[Lambda]h\[Psi]^2*Log[Glaisher] + 3456*a2*\[Lambda]\[Psi]*
        Log[Glaisher] - 5184*gXsq*\[Lambda]\[Psi]*Log[Glaisher] + 
       4608*\[Lambda]h\[Psi]*\[Lambda]\[Psi]*Log[Glaisher] + 
       13824*\[Lambda]\[Psi]^2*Log[Glaisher] - 
       2*(114*a2^2 + 330*b4^2 + 9*g1sq^2 + 18*g1sq*g2sq + 27*g2sq^2 - 
         360*b4*gXsq + 279*gXsq^2 - 144*yt^4 - 72*g1sq*\[Lambda]h - 
         216*g2sq*\[Lambda]h + 288*yt^2*\[Lambda]h + 576*\[Lambda]h^2 - 
         24*g1sq*\[Lambda]h\[Psi] - 72*g2sq*\[Lambda]h\[Psi] - 
         72*gXsq*\[Lambda]h\[Psi] + 96*yt^2*\[Lambda]h\[Psi] + 
         192*\[Lambda]h*\[Lambda]h\[Psi] + 160*\[Lambda]h\[Psi]^2 - 
         216*gXsq*\[Lambda]\[Psi] + 192*\[Lambda]h\[Psi]*\[Lambda]\[Psi] + 
         576*\[Lambda]\[Psi]^2 + 6*a2*(20*b4 - 33*gXsq + 8*\[Lambda]h\[Psi] + 
           24*\[Lambda]\[Psi]))*Log[4*Pi*T] + 
       (114*a2^2 + 330*b4^2 + 9*g1sq^2 + 18*g1sq*g2sq + 27*g2sq^2 - 
         360*b4*gXsq + 279*gXsq^2 - 144*yt^4 - 72*g1sq*\[Lambda]h - 
         216*g2sq*\[Lambda]h + 288*yt^2*\[Lambda]h + 576*\[Lambda]h^2 - 
         24*g1sq*\[Lambda]h\[Psi] - 72*g2sq*\[Lambda]h\[Psi] - 
         72*gXsq*\[Lambda]h\[Psi] + 96*yt^2*\[Lambda]h\[Psi] + 
         192*\[Lambda]h*\[Lambda]h\[Psi] + 160*\[Lambda]h\[Psi]^2 - 
         216*gXsq*\[Lambda]\[Psi] + 192*\[Lambda]h\[Psi]*\[Lambda]\[Psi] + 
         576*\[Lambda]\[Psi]^2 + 6*a2*(20*b4 - 33*gXsq + 8*\[Lambda]h\[Psi] + 
           24*\[Lambda]\[Psi]))*Log[\[Mu]^2]) - 
     T^4*(30*g1sq*(9*g2sq + 44*g3sq)*(35 + 72*EulerGamma - 28*Lb - 8*Lf - 
         48*Log[2] - 48*Log[4] - 72*Log[Pi]) + 
       120*(1885*g1sq^2 + 18*g1sq*(9*g2sq + 44*g3sq) + 
         9*(469*g2sq^2 + 216*g2sq*g3sq + 1680*g3sq^2))*Log[\[Mu]/T] + 
       9*(360*g2sq*g3sq*(35 + 72*EulerGamma - 28*Lb - 8*Lf - 48*Log[2] - 
           48*Log[4] - 72*Log[Pi]) + g2sq^2*(-12547 + 36600*EulerGamma - 
           16260*Lb + 1320*Lf - 30384*Log[2] - 57360*Log[4] + 
           261120*Log[Glaisher] - 56280*Log[Pi] - 249600*Derivative[1][Zeta][
             -3]) + 32*g3sq^2*(-1209 + 4320*EulerGamma - 1880*Lb + 80*Lf - 
           2940*Log[2] - 6360*Log[4] + 25920*Log[Glaisher] - 6300*Log[Pi] - 
           21600*Derivative[1][Zeta][-3])) + 
       25*g1sq^2*(-1335 + 5208*EulerGamma - 2452*Lb + 328*Lf - 8976*Log[2] - 
         8592*Log[4] + 53760*Log[Glaisher] - 9048*Log[Pi] - 
         76800*Derivative[1][Zeta][-3])) - 27*T^4*yt^4*
      (211 + 760*EulerGamma - 100*Lb - 200*Lf - 1944*Log[2] - 720*Log[4] + 
       9600*Log[Glaisher] - 1440*Log[Pi] + 1440*Log[\[Mu]/T] - 
       14400*Derivative[1][Zeta][-3]) - 1152*(g2sq^2 + 6*g3sq^2 + gXsq^2)*T^4*
      (-107 + 40*EulerGamma - 15*Lb - 310*Log[2] + 1760*Log[Glaisher] - 
       155*Log[Pi] + 155*Log[\[Mu]/T] - 3800*Derivative[1][Zeta][-3]) - 
     2*(5*g1sq^2 + 9*g2sq^2)*T^4*(-997 + 1920*EulerGamma - 840*Lb + 120*Lf - 
       1188*Log[2] - 2940*Log[4] + 12480*Log[Glaisher] - 2940*Log[Pi] + 
       2940*Log[\[Mu]/T] - 2400*Derivative[1][Zeta][-3]) - 
     12*T^4*(-348*a2^2 - 870*b4^2 + 210*a2^2*EulerGamma + 
       600*a2*b4*EulerGamma + 750*b4^2*EulerGamma - 1392*\[Lambda]h^2 + 
       1440*EulerGamma*\[Lambda]h^2 + 240*a2*EulerGamma*\[Lambda]h\[Psi] + 
       960*EulerGamma*\[Lambda]h*\[Lambda]h\[Psi] - 464*\[Lambda]h\[Psi]^2 + 
       320*EulerGamma*\[Lambda]h\[Psi]^2 + 720*a2*EulerGamma*
        \[Lambda]\[Psi] + 960*EulerGamma*\[Lambda]h\[Psi]*\[Lambda]\[Psi] - 
       1392*\[Lambda]\[Psi]^2 + 1440*EulerGamma*\[Lambda]\[Psi]^2 - 
       3420*a2^2*Log[2] - 3600*a2*b4*Log[2] - 9900*b4^2*Log[2] - 
       17280*\[Lambda]h^2*Log[2] - 1440*a2*\[Lambda]h\[Psi]*Log[2] - 
       5760*\[Lambda]h*\[Lambda]h\[Psi]*Log[2] - 4800*\[Lambda]h\[Psi]^2*
        Log[2] - 4320*a2*\[Lambda]\[Psi]*Log[2] - 5760*\[Lambda]h\[Psi]*
        \[Lambda]\[Psi]*Log[2] - 17280*\[Lambda]\[Psi]^2*Log[2] + 
       22320*a2^2*Log[Glaisher] + 14400*a2*b4*Log[Glaisher] + 
       61200*b4^2*Log[Glaisher] + 103680*\[Lambda]h^2*Log[Glaisher] + 
       5760*a2*\[Lambda]h\[Psi]*Log[Glaisher] + 23040*\[Lambda]h*
        \[Lambda]h\[Psi]*Log[Glaisher] + 30720*\[Lambda]h\[Psi]^2*
        Log[Glaisher] + 17280*a2*\[Lambda]\[Psi]*Log[Glaisher] + 
       23040*\[Lambda]h\[Psi]*\[Lambda]\[Psi]*Log[Glaisher] + 
       103680*\[Lambda]\[Psi]^2*Log[Glaisher] - 1080*a2^2*Log[Pi] - 
       2700*b4^2*Log[Pi] - 4320*\[Lambda]h^2*Log[Pi] - 
       1440*\[Lambda]h\[Psi]^2*Log[Pi] - 4320*\[Lambda]\[Psi]^2*Log[Pi] - 
       30*(21*a2^2 + 75*b4^2 + 12*a2*(5*b4 + 2*\[Lambda]h\[Psi] + 
           6*\[Lambda]\[Psi]) + 16*(9*\[Lambda]h^2 + 6*\[Lambda]h*
            \[Lambda]h\[Psi] + 2*\[Lambda]h\[Psi]^2 + 6*\[Lambda]h\[Psi]*
            \[Lambda]\[Psi] + 9*\[Lambda]\[Psi]^2))*Log[Pi*T] + 
       180*(6*a2^2 + 15*b4^2 + 8*(3*\[Lambda]h^2 + \[Lambda]h\[Psi]^2 + 
           3*\[Lambda]\[Psi]^2))*Log[\[Mu]/T] + 315*a2^2*Log[\[Mu]^2] + 
       900*a2*b4*Log[\[Mu]^2] + 1125*b4^2*Log[\[Mu]^2] + 
       2160*\[Lambda]h^2*Log[\[Mu]^2] + 360*a2*\[Lambda]h\[Psi]*
        Log[\[Mu]^2] + 1440*\[Lambda]h*\[Lambda]h\[Psi]*Log[\[Mu]^2] + 
       480*\[Lambda]h\[Psi]^2*Log[\[Mu]^2] + 1080*a2*\[Lambda]\[Psi]*
        Log[\[Mu]^2] + 1440*\[Lambda]h\[Psi]*\[Lambda]\[Psi]*Log[\[Mu]^2] + 
       2160*\[Lambda]\[Psi]^2*Log[\[Mu]^2] - 43200*a2^2*
        Derivative[1][Zeta][-3] - 108000*b4^2*Derivative[1][Zeta][-3] - 
       172800*\[Lambda]h^2*Derivative[1][Zeta][-3] - 57600*\[Lambda]h\[Psi]^2*
        Derivative[1][Zeta][-3] - 172800*\[Lambda]\[Psi]^2*
        Derivative[1][Zeta][-3]) - 2*T^4*yt^2*
      (90*(53*g1sq + 135*g2sq + 96*g3sq)*Log[\[Mu]/T] + 
       27*g2sq*(199 + 300*EulerGamma - 90*Lf + 150*Log[2] - 450*Log[4] + 
         1440*Log[Glaisher] - 450*Log[Pi] + 3600*Derivative[1][Zeta][-3]) + 
       192*g3sq*(212 + 270*EulerGamma - 105*Lb - 30*Lf + 381*Log[2] - 
         90*Log[4] - 3600*Log[Glaisher] - 45*Log[Pi] + 
         9000*Derivative[1][Zeta][-3]) + g1sq*(5183 + 7020*EulerGamma - 
         1680*Lb - 1290*Lf + 7446*Log[2] - 5490*Log[4] - 
         44640*Log[Glaisher] - 4770*Log[Pi] + 176400*Derivative[1][Zeta][
           -3])) + T^4*(-30*(169*g1sq^2 + 594*g1sq*g2sq + 
         45*(45*g2sq^2 + 229*gXsq^2))*Log[\[Mu]/T] - 
       27*gXsq^2*(-6557 + 2380*EulerGamma - 465*Lb - 22900*Log[2] + 
         133520*Log[Glaisher] - 11450*Log[Pi] - 
         246800*Derivative[1][Zeta][-3]) + 18*g1sq*g2sq*
        (499 + 180*EulerGamma - 135*Lb + 1980*Log[2] - 18000*Log[Glaisher] + 
         990*Log[Pi] + 39600*Derivative[1][Zeta][-3]) + 
       g1sq^2*(2543 - 420*EulerGamma - 45*Lb + 10140*Log[2] - 
         70320*Log[Glaisher] + 5070*Log[Pi] + 145200*Derivative[1][Zeta][
           -3]) + 3*g2sq^2*(12589 - 7020*EulerGamma + 2025*Lb + 
         40500*Log[2] - 187920*Log[Glaisher] + 20250*Log[Pi] + 
         291600*Derivative[1][Zeta][-3])))/Pi^2, 
 "Parameters3d" -> {g1sq3d, g2sq3d, gXsq3d, g3sq3d, a23d, b43d, \[Lambda]h3d, 
   \[Lambda]h\[Psi]3d, \[Lambda]\[Psi]3d, m1sq3d, m2sq3d, mx0sq3d}, 
 "BetaFunctionsUS" -> 
  {m1sq3dUS -> (5*g1sq3dUS^2 + 18*g1sq3dUS*g2sq3dUS - 51*g2sq3dUS^2 - 
      48*(g1sq3dUS + 3*g2sq3dUS)*\[Lambda]h3dUS + 192*\[Lambda]h3dUS^2 + 
      16*\[Lambda]h\[Psi]3dUS*(-3*gXsq3dUS + 2*\[Lambda]h\[Psi]3dUS))/
     (256*Pi^2), m2sq3dUS -> -1/256*(-24*a23dUS^2 + 96*a23dUS*gXsq3dUS + 
       39*gXsq3dUS^2 + 16*(g1sq3dUS + 3*g2sq3dUS - 2*\[Lambda]h\[Psi]3dUS)*
        \[Lambda]h\[Psi]3dUS + 144*gXsq3dUS*\[Lambda]\[Psi]3dUS - 
       192*\[Lambda]\[Psi]3dUS^2)/Pi^2, 
   mx0sq3dUS -> (2*(a23dUS^2 + 5*b43dUS^2) - (3*a23dUS + 20*b43dUS)*
       gXsq3dUS + gXsq3dUS^2)/(16*Pi^2)}, 
 "CouplingsUS" -> 
  {a23dUS -> a23d + ((2*\[Lambda]VL[1] - 3*\[Lambda]VL[2])*\[Lambda]VL[10])/
      (16*Pi*Sqrt[\[Mu]sqSU2P2]), 
   b43dUS -> b43d + (-4*\[Lambda]VL[1]^2 + 4*\[Lambda]VL[1]*\[Lambda]VL[2] - 
       3*\[Lambda]VL[2]^2)/(32*Pi*Sqrt[\[Mu]sqSU2P2]), 
   \[Lambda]h3dUS -> \[Lambda]h3d - ((8*\[Lambda]VL[3]^2)/Sqrt[\[Mu]sqSU3] + 
       (3*\[Lambda]VL[4]^2)/Sqrt[\[Mu]sqSU2P1] + \[Lambda]VL[5]^2/
        Sqrt[\[Mu]sqU1] + (4*\[Lambda]VL[6]^2)/(Sqrt[\[Mu]sqSU2P1] + 
         Sqrt[\[Mu]sqU1]) + (3*\[Lambda]VL[9]^2)/Sqrt[\[Mu]sqSU2P2])/(32*Pi), 
   \[Lambda]h\[Psi]3dUS -> \[Lambda]h\[Psi]3d - 
     ((\[Lambda]VL[5]*\[Lambda]VL[7])/Sqrt[\[Mu]sqU1] + 
       (3*\[Lambda]VL[4]*\[Lambda]VL[8])/Sqrt[\[Mu]sqSU2P1] + 
       (3*\[Lambda]VL[9]*\[Lambda]VL[10])/Sqrt[\[Mu]sqSU2P2])/(16*Pi), 
   \[Lambda]\[Psi]3dUS -> \[Lambda]\[Psi]3d - 
     (\[Lambda]VL[7]^2/Sqrt[\[Mu]sqU1] + (3*\[Lambda]VL[8]^2)/
        Sqrt[\[Mu]sqSU2P1] + (3*\[Lambda]VL[10]^2)/Sqrt[\[Mu]sqSU2P2])/
      (32*Pi), g1sq3dUS -> g1sq3d, g2sq3dUS -> 
    g2sq3d - g2sq3d^2/(24*Pi*Sqrt[\[Mu]sqSU2P1]), 
   gXsq3dUS -> gXsq3d - gXsq3d^2/(24*Pi*Sqrt[\[Mu]sqSU2P2]), 
   g3sq3dUS -> g3sq3d - g3sq3d^2/(16*Pi*Sqrt[\[Mu]sqSU3])}, 
 "ScalarMassUSLO" -> 
  {m1sq3dUS -> m1sq3d - (8*Sqrt[\[Mu]sqSU3]*\[Lambda]VL[3] + 
       3*Sqrt[\[Mu]sqSU2P1]*\[Lambda]VL[4] + Sqrt[\[Mu]sqU1]*\[Lambda]VL[5] + 
       3*Sqrt[\[Mu]sqSU2P2]*\[Lambda]VL[9])/(8*Pi), 
   m2sq3dUS -> m2sq3d - (Sqrt[\[Mu]sqU1]*\[Lambda]VL[7] + 
       3*Sqrt[\[Mu]sqSU2P1]*\[Lambda]VL[8] + 3*Sqrt[\[Mu]sqSU2P2]*
        \[Lambda]VL[10])/(8*Pi), mx0sq3dUS -> 
    mx0sq3d + (Sqrt[\[Mu]sqSU2P2]*(2*\[Lambda]VL[1] - 3*\[Lambda]VL[2]))/
      (8*Pi)}, "ScalarMassUSNLO" -> 
  {m1sq3dUS -> (-6*g2sq3d^2*Log[\[Mu]3/(2*Sqrt[\[Mu]sqSU2P1])] + 
      48*g3sq3d*(1 + 4*Log[\[Mu]3/(2*Sqrt[\[Mu]sqSU3])])*\[Lambda]VL[3] - 
      16*(1 + 2*Log[\[Mu]3/(2*Sqrt[\[Mu]sqSU3])])*\[Lambda]VL[3]^2 + 
      12*g2sq3d*(1 + 4*Log[\[Mu]3/(2*Sqrt[\[Mu]sqSU2P1])])*\[Lambda]VL[4] - 
      6*(1 + 2*Log[\[Mu]3/(2*Sqrt[\[Mu]sqSU2P1])])*\[Lambda]VL[4]^2 - 
      2*(1 + 2*Log[\[Mu]3/(2*Sqrt[\[Mu]sqU1])])*\[Lambda]VL[5]^2 - 
      12*(1 + 2*Log[\[Mu]3/(Sqrt[\[Mu]sqSU2P1] + Sqrt[\[Mu]sqU1])])*
       \[Lambda]VL[6]^2 + 12*gXsq3d*
       (1 + 4*Log[\[Mu]3/(2*Sqrt[\[Mu]sqSU2P2])])*\[Lambda]VL[9] - 
      6*(1 + 2*Log[\[Mu]3/(2*Sqrt[\[Mu]sqSU2P2])])*\[Lambda]VL[9]^2 + 
      (\[Lambda]VL[5]*(Sqrt[\[Mu]sqU1]*\[Lambda]VLL[1] + 
         3*Sqrt[\[Mu]sqSU2P1]*\[Lambda]VLL[2] + 8*Sqrt[\[Mu]sqSU3]*
          \[Lambda]VLL[4]))/Sqrt[\[Mu]sqU1] + 
      (3*\[Lambda]VL[4]*(Sqrt[\[Mu]sqU1]*\[Lambda]VLL[2] + 
         5*Sqrt[\[Mu]sqSU2P1]*\[Lambda]VLL[3] + 8*Sqrt[\[Mu]sqSU3]*
          \[Lambda]VLL[5]))/Sqrt[\[Mu]sqSU2P1] + 
      (8*\[Lambda]VL[3]*(Sqrt[\[Mu]sqU1]*\[Lambda]VLL[4] + 
         3*Sqrt[\[Mu]sqSU2P1]*\[Lambda]VLL[5] + 10*Sqrt[\[Mu]sqSU3]*
          \[Lambda]VLL[7]))/Sqrt[\[Mu]sqSU3] + 15*\[Lambda]VL[9]*
       \[Lambda]VLL[8])/(128*Pi^2), m2sq3dUS -> 
    (-6*gXsq3d^2*Log[\[Mu]3/(2*Sqrt[\[Mu]sqSU2P2])] - 
      2*(1 + 2*Log[\[Mu]3/(2*Sqrt[\[Mu]sqU1])])*\[Lambda]VL[7]^2 + 
      12*g2sq3d*(1 + 4*Log[\[Mu]3/(2*Sqrt[\[Mu]sqSU2P1])])*\[Lambda]VL[8] - 
      6*(1 + 2*Log[\[Mu]3/(2*Sqrt[\[Mu]sqSU2P1])])*\[Lambda]VL[8]^2 + 
      12*gXsq3d*(1 + 4*Log[\[Mu]3/(2*Sqrt[\[Mu]sqSU2P2])])*\[Lambda]VL[10] - 
      6*(1 + 2*Log[\[Mu]3/(2*Sqrt[\[Mu]sqSU2P2])])*\[Lambda]VL[10]^2 + 
      (\[Lambda]VL[7]*(Sqrt[\[Mu]sqU1]*\[Lambda]VLL[1] + 
         3*Sqrt[\[Mu]sqSU2P1]*\[Lambda]VLL[2] + 8*Sqrt[\[Mu]sqSU3]*
          \[Lambda]VLL[4]))/Sqrt[\[Mu]sqU1] + 
      (3*\[Lambda]VL[8]*(Sqrt[\[Mu]sqU1]*\[Lambda]VLL[2] + 
         5*Sqrt[\[Mu]sqSU2P1]*\[Lambda]VLL[3] + 8*Sqrt[\[Mu]sqSU3]*
          \[Lambda]VLL[5]))/Sqrt[\[Mu]sqSU2P1] + 15*\[Lambda]VL[10]*
       \[Lambda]VLL[8])/(128*Pi^2), mx0sq3dUS -> 
    -1/128*(16*\[Lambda]VL[1]^2 + 4*gXsq3d*(2*\[Lambda]VL[1] - 
         3*\[Lambda]VL[2]) - 8*\[Lambda]VL[1]*\[Lambda]VL[2] + 
       6*\[Lambda]VL[2]^2 + 4*Log[\[Mu]3/(2*Sqrt[\[Mu]sqSU2P2])]*
        (4*gXsq3d^2 + 8*\[Lambda]VL[1]^2 + 4*gXsq3d*(2*\[Lambda]VL[1] - 
           3*\[Lambda]VL[2]) - 4*\[Lambda]VL[1]*\[Lambda]VL[2] + 
         3*\[Lambda]VL[2]^2) + 10*\[Lambda]VL[1]*\[Lambda]VLL[8] - 
       15*\[Lambda]VL[2]*\[Lambda]VLL[8])/Pi^2}, 
 "PressureUSLO" -> \[Mu]sqSU2P1^(3/2)/(4*Pi) + \[Mu]sqSU2P2^(3/2)/(4*Pi) + 
   (2*\[Mu]sqSU3^(3/2))/(3*Pi) + \[Mu]sqU1^(3/2)/(12*Pi), 
 "PressureUSNLO" -> 
  (-3*g2sq3d*(6*\[Mu]sqSU2P1 + 8*\[Mu]sqSU2P1*
       Log[\[Mu]3/(2*Sqrt[\[Mu]sqSU2P1])]))/(64*Pi^2) - 
   (3*gXsq3d*(6*\[Mu]sqSU2P2 + 8*\[Mu]sqSU2P2*
       Log[\[Mu]3/(2*Sqrt[\[Mu]sqSU2P2])]))/(64*Pi^2) - 
   (3*g3sq3d*(6*\[Mu]sqSU3 + 8*\[Mu]sqSU3*Log[\[Mu]3/(2*Sqrt[\[Mu]sqSU3])]))/
    (16*Pi^2) - (\[Mu]sqU1*\[Lambda]VLL[1] + 6*Sqrt[\[Mu]sqSU2P1]*
      Sqrt[\[Mu]sqU1]*\[Lambda]VLL[2] + 15*\[Mu]sqSU2P1*\[Lambda]VLL[3] + 
     16*Sqrt[\[Mu]sqSU3]*Sqrt[\[Mu]sqU1]*\[Lambda]VLL[4] + 
     48*Sqrt[\[Mu]sqSU2P1]*Sqrt[\[Mu]sqSU3]*\[Lambda]VLL[5] + 
     80*\[Mu]sqSU3*\[Lambda]VLL[7] + 15*\[Mu]sqSU2P2*\[Lambda]VLL[8])/
    (128*Pi^2), "pressureUSNNLO" -> Null, "USrules" -> parameters3d, 
 "VeffLO" -> (m2sq3dUS*w^2)/2 + (w^4*\[Lambda]\[Psi]3dUS)/4, 
 "VeffNLO" -> -1/16*(gXsq3dUS*w^2)^(3/2)/Pi - 
   (mx0sq3dUS + (a23dUS*w^2)/2)^(3/2)/(4*Pi) - 
   (m1sq3dUS + (w^2*\[Lambda]h\[Psi]3dUS)/2)^(3/2)/(3*Pi) - 
   (m2sq3dUS + w^2*\[Lambda]\[Psi]3dUS)^(3/2)/(4*Pi) - 
   (m2sq3dUS + 3*w^2*\[Lambda]\[Psi]3dUS)^(3/2)/(12*Pi), 
 "VeffNNLO" -> (gXsq3dUS^2*w^2)/(16*Pi^2) + 
   (3*gXsq3dUS*Sqrt[gXsq3dUS*w^2]*Sqrt[mx0sq3dUS + (a23dUS*w^2)/2])/
    (16*Pi^2) + (15*b43dUS*(mx0sq3dUS + (a23dUS*w^2)/2))/(64*Pi^2) + 
   (3*\[Lambda]h3dUS*(m1sq3dUS + (w^2*\[Lambda]h\[Psi]3dUS)/2))/(8*Pi^2) + 
   (9*gXsq3dUS*Sqrt[gXsq3dUS*w^2]*Sqrt[m2sq3dUS + w^2*\[Lambda]\[Psi]3dUS])/
    (128*Pi^2) + (9*a23dUS*Sqrt[mx0sq3dUS + (a23dUS*w^2)/2]*
     Sqrt[m2sq3dUS + w^2*\[Lambda]\[Psi]3dUS])/(64*Pi^2) + 
   (3*\[Lambda]h\[Psi]3dUS*Sqrt[m1sq3dUS + (w^2*\[Lambda]h\[Psi]3dUS)/2]*
     Sqrt[m2sq3dUS + w^2*\[Lambda]\[Psi]3dUS])/(16*Pi^2) + 
   (15*\[Lambda]\[Psi]3dUS*(m2sq3dUS + w^2*\[Lambda]\[Psi]3dUS))/(64*Pi^2) + 
   (3*gXsq3dUS*Sqrt[gXsq3dUS*w^2]*Sqrt[m2sq3dUS + 3*w^2*\[Lambda]\[Psi]3dUS])/
    (128*Pi^2) + (3*a23dUS*Sqrt[mx0sq3dUS + (a23dUS*w^2)/2]*
     Sqrt[m2sq3dUS + 3*w^2*\[Lambda]\[Psi]3dUS])/(64*Pi^2) + 
   (\[Lambda]h\[Psi]3dUS*Sqrt[m1sq3dUS + (w^2*\[Lambda]h\[Psi]3dUS)/2]*
     Sqrt[m2sq3dUS + 3*w^2*\[Lambda]\[Psi]3dUS])/(16*Pi^2) + 
   (3*\[Lambda]\[Psi]3dUS*Sqrt[m2sq3dUS + w^2*\[Lambda]\[Psi]3dUS]*
     Sqrt[m2sq3dUS + 3*w^2*\[Lambda]\[Psi]3dUS])/(32*Pi^2) + 
   (3*\[Lambda]\[Psi]3dUS*(m2sq3dUS + 3*w^2*\[Lambda]\[Psi]3dUS))/(64*Pi^2) + 
   (3*gXsq3dUS^2*w^2*(1/2 + Log[(2*\[Mu]3US)/Sqrt[gXsq3dUS*w^2]]))/
    (256*Pi^2) + (8*((5*gXsq3dUS^4*w^8)/(2048*Pi^2) + 
      ((-69*gXsq3dUS^4*w^8)/(4096*Pi^2) - 
        (3*gXsq3dUS^2*w^4*((57*gXsq3dUS^2*w^4)/(512*Pi^2) - 
           (63*gXsq3dUS^2*w^4*(1/2 + Log[(2*\[Mu]3US)/(3*Sqrt[gXsq3dUS*
                   w^2])]))/(256*Pi^2)))/16 + 
        (3*gXsq3dUS^4*w^8*(1/(32*Pi^2) - 
           (3*(1/2 + Log[(2*\[Mu]3US)/Sqrt[gXsq3dUS*w^2]]))/(16*Pi^2)))/256)/
       3))/(gXsq3dUS^2*w^6) + 
   (6*(-1/128*(gXsq3dUS*w^2*Sqrt[gXsq3dUS*w^2]*
         Sqrt[mx0sq3dUS + (a23dUS*w^2)/2])/Pi^2 + 
      (Sqrt[mx0sq3dUS + (a23dUS*w^2)/2]*
        (-1/32*(gXsq3dUS*w^2*Sqrt[gXsq3dUS*w^2])/Pi + 
         (gXsq3dUS*w^2*Sqrt[mx0sq3dUS + (a23dUS*w^2)/2])/(16*Pi)))/(4*Pi) - 
      (((gXsq3dUS^2*w^4)/16 - (gXsq3dUS*w^2*(2*mx0sq3dUS + a23dUS*w^2))/2)*
        (1/2 + Log[\[Mu]3US/(Sqrt[gXsq3dUS*w^2]/2 + 
            2*Sqrt[mx0sq3dUS + (a23dUS*w^2)/2])]))/(16*Pi^2)))/w^2 + 
   (g1sq3dUS*(-1/32*(2*m1sq3dUS + w^2*\[Lambda]h\[Psi]3dUS)/Pi^2 - 
      2*(-1/16*(m1sq3dUS + (w^2*\[Lambda]h\[Psi]3dUS)/2)/Pi^2 - 
        ((2*m1sq3dUS + w^2*\[Lambda]h\[Psi]3dUS)*
          (1/2 + Log[\[Mu]3US/(2*Sqrt[m1sq3dUS + (w^2*\[Lambda]h\[Psi]3dUS)/
                 2])]))/(16*Pi^2))))/4 + 
   (3*g2sq3dUS*(-1/32*(2*m1sq3dUS + w^2*\[Lambda]h\[Psi]3dUS)/Pi^2 - 
      2*(-1/16*(m1sq3dUS + (w^2*\[Lambda]h\[Psi]3dUS)/2)/Pi^2 - 
        ((2*m1sq3dUS + w^2*\[Lambda]h\[Psi]3dUS)*
          (1/2 + Log[\[Mu]3US/(2*Sqrt[m1sq3dUS + (w^2*\[Lambda]h\[Psi]3dUS)/
                 2])]))/(16*Pi^2))))/4 - 
   (3*w^2*\[Lambda]\[Psi]3dUS^2*
     (1/2 + Log[\[Mu]3US/(3*Sqrt[m2sq3dUS + 3*w^2*\[Lambda]\[Psi]3dUS])]))/
    (16*Pi^2) + 
   (3*(-1/128*(gXsq3dUS*w^2*Sqrt[gXsq3dUS*w^2]*
         Sqrt[m2sq3dUS + w^2*\[Lambda]\[Psi]3dUS])/Pi^2 + 
      (Sqrt[m2sq3dUS + w^2*\[Lambda]\[Psi]3dUS]*
        (-1/32*(gXsq3dUS*w^2*Sqrt[gXsq3dUS*w^2])/Pi + 
         (gXsq3dUS*w^2*Sqrt[m2sq3dUS + w^2*\[Lambda]\[Psi]3dUS])/(16*Pi)))/
       (4*Pi) - (((gXsq3dUS^2*w^4)/16 - (gXsq3dUS*w^2*(2*m2sq3dUS + 
            2*w^2*\[Lambda]\[Psi]3dUS))/2)*
        (1/2 + Log[\[Mu]3US/(Sqrt[gXsq3dUS*w^2]/2 + 
            2*Sqrt[m2sq3dUS + w^2*\[Lambda]\[Psi]3dUS])]))/(16*Pi^2)))/
    (2*w^2) + (3*((gXsq3dUS^2*w^4)/(128*Pi^2) - 
      (gXsq3dUS*w^2*(-m2sq3dUS + (gXsq3dUS*w^2)/2 - 
         3*w^2*\[Lambda]\[Psi]3dUS))/(64*Pi^2) + 
      (gXsq3dUS*w^2*Sqrt[gXsq3dUS*w^2]*Sqrt[m2sq3dUS + 
          3*w^2*\[Lambda]\[Psi]3dUS])/(64*Pi^2) - 
      ((m2sq3dUS + 3*w^2*\[Lambda]\[Psi]3dUS)^2*
        (1/2 + Log[\[Mu]3US/Sqrt[m2sq3dUS + 3*w^2*\[Lambda]\[Psi]3dUS]]))/
       (16*Pi^2) + ((-m2sq3dUS + (gXsq3dUS*w^2)/4 - 
          3*w^2*\[Lambda]\[Psi]3dUS)^2*
        (1/2 + Log[\[Mu]3US/(Sqrt[gXsq3dUS*w^2]/2 + 
            Sqrt[m2sq3dUS + 3*w^2*\[Lambda]\[Psi]3dUS])]))/(8*Pi^2) - 
      (((7*gXsq3dUS^2*w^4)/16 + (-m2sq3dUS + (gXsq3dUS*w^2)/4 - 
           3*w^2*\[Lambda]\[Psi]3dUS)^2 - 
         (gXsq3dUS*w^2*(m2sq3dUS + 3*w^2*\[Lambda]\[Psi]3dUS))/2)*
        (1/2 + Log[\[Mu]3US/(Sqrt[gXsq3dUS*w^2] + 
            Sqrt[m2sq3dUS + 3*w^2*\[Lambda]\[Psi]3dUS])]))/(16*Pi^2)))/
    (4*w^2) - (3*a23dUS^2*w^2*
     (1/2 + Log[\[Mu]3US/(2*Sqrt[mx0sq3dUS + (a23dUS*w^2)/2] + 
         Sqrt[m2sq3dUS + 3*w^2*\[Lambda]\[Psi]3dUS])]))/(64*Pi^2) - 
   (w^2*\[Lambda]h\[Psi]3dUS^2*
     (1/2 + Log[\[Mu]3US/(2*Sqrt[m1sq3dUS + (w^2*\[Lambda]h\[Psi]3dUS)/2] + 
         Sqrt[m2sq3dUS + 3*w^2*\[Lambda]\[Psi]3dUS])]))/(16*Pi^2) + 
   (3*(-1/32*(Sqrt[gXsq3dUS*w^2]*((gXsq3dUS*w^2)/4 - 
          2*w^2*\[Lambda]\[Psi]3dUS)*Sqrt[m2sq3dUS + 
           w^2*\[Lambda]\[Psi]3dUS])/Pi^2 + 
      (Sqrt[m2sq3dUS + 3*w^2*\[Lambda]\[Psi]3dUS]*
        ((gXsq3dUS*w^2*Sqrt[m2sq3dUS + w^2*\[Lambda]\[Psi]3dUS])/(16*Pi) - 
         (Sqrt[gXsq3dUS*w^2]*((gXsq3dUS*w^2)/4 + 2*w^2*\[Lambda]\[Psi]3dUS))/
          (8*Pi)))/(4*Pi) + (w^4*\[Lambda]\[Psi]3dUS^2*
        (1/2 + Log[\[Mu]3US/(Sqrt[m2sq3dUS + w^2*\[Lambda]\[Psi]3dUS] + 
            Sqrt[m2sq3dUS + 3*w^2*\[Lambda]\[Psi]3dUS])]))/(4*Pi^2) - 
      (((gXsq3dUS^2*w^4)/16 + 4*w^4*\[Lambda]\[Psi]3dUS^2 - 
         (gXsq3dUS*w^2*(2*m2sq3dUS + 4*w^2*\[Lambda]\[Psi]3dUS))/2)*
        (1/2 + Log[\[Mu]3US/(Sqrt[gXsq3dUS*w^2]/2 + 
            Sqrt[m2sq3dUS + w^2*\[Lambda]\[Psi]3dUS] + 
            Sqrt[m2sq3dUS + 3*w^2*\[Lambda]\[Psi]3dUS])]))/(16*Pi^2)))/
    (4*w^2) + 
   (3*(-1/32*(Sqrt[gXsq3dUS*w^2]*((gXsq3dUS*w^2)/4 + 2*w^2*\[Lambda]\[Psi]3dU\
S)*Sqrt[m2sq3dUS + 3*w^2*\[Lambda]\[Psi]3dUS])/Pi^2 + 
      (Sqrt[m2sq3dUS + w^2*\[Lambda]\[Psi]3dUS]*
        (-1/8*(Sqrt[gXsq3dUS*w^2]*((gXsq3dUS*w^2)/4 - 
             2*w^2*\[Lambda]\[Psi]3dUS))/Pi + 
         (gXsq3dUS*w^2*Sqrt[m2sq3dUS + 3*w^2*\[Lambda]\[Psi]3dUS])/(16*Pi)))/
       (4*Pi) + (w^4*\[Lambda]\[Psi]3dUS^2*
        (1/2 + Log[\[Mu]3US/(Sqrt[m2sq3dUS + w^2*\[Lambda]\[Psi]3dUS] + 
            Sqrt[m2sq3dUS + 3*w^2*\[Lambda]\[Psi]3dUS])]))/(4*Pi^2) - 
      (((gXsq3dUS^2*w^4)/16 + 4*w^4*\[Lambda]\[Psi]3dUS^2 - 
         (gXsq3dUS*w^2*(2*m2sq3dUS + 4*w^2*\[Lambda]\[Psi]3dUS))/2)*
        (1/2 + Log[\[Mu]3US/(Sqrt[gXsq3dUS*w^2]/2 + 
            Sqrt[m2sq3dUS + w^2*\[Lambda]\[Psi]3dUS] + 
            Sqrt[m2sq3dUS + 3*w^2*\[Lambda]\[Psi]3dUS])]))/(16*Pi^2)))/
    (4*w^2) - (3*w^2*\[Lambda]\[Psi]3dUS^2*
     (1/2 + Log[\[Mu]3US/(2*Sqrt[m2sq3dUS + w^2*\[Lambda]\[Psi]3dUS] + 
         Sqrt[m2sq3dUS + 3*w^2*\[Lambda]\[Psi]3dUS])]))/(16*Pi^2), 
 "\[Phi]Vev" -> {w}|>
