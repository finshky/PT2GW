<|"BetaFunctions" -> {gsq -> (gsq^2*Y^2)/(24*Pi^2), 
   \[Lambda] -> (3*gsq^2*Y^4 - 6*gsq*Y^2*\[Lambda] + 10*\[Lambda]^2)/
     (8*Pi^2), \[Mu]sq -> -1/8*((3*gsq*Y^2 - 4*\[Lambda])*\[Mu]sq)/Pi^2}, 
 "BetaFunctions3d" -> 
  {\[Mu]sq -> (8*gsq3d^2*Y^4 - 16*gsq3d*Y^2*\[Lambda]3d + 16*\[Lambda]3d^2 + 
      \[Lambda]VL[1]^2)/(32*Pi^2)}, "Constants3d" -> 
  {Lb -> 2*EulerGamma - 2*Log[4*Pi] + Log[\[Mu]^2/T^2], 
   Lf -> 2*EulerGamma + 4*Log[2] - 2*Log[4*Pi] + Log[\[Mu]^2/T^2]}, 
 "Couplings3d" -> {gsq3d -> gsq*T - (gsq^2*Lb*T*Y^2)/(48*Pi^2), 
   \[Lambda]3d -> (T*(gsq^2*(2 - 3*Lb)*Y^4 + 6*gsq*Lb*Y^2*\[Lambda] + 
       2*\[Lambda]*(8*Pi^2 - 5*Lb*\[Lambda])))/(16*Pi^2)}, 
 "TempScalar3d" -> {\[Lambda]VLL[1] -> (gsq^2*T*Y^4)/Pi^2, 
   \[Lambda]VL[1] -> (gsq*T*Y^2*(48*Pi^2 - gsq*(-4 + Lb)*Y^2 + 24*\[Lambda]))/
     (24*Pi^2)}, "DebyeMass3dLO" -> {\[Mu]sqU1 -> (gsq*T^2*Y^2)/3}, 
 "DebyeMass3dNLO" -> 
  {\[Mu]sqU1 -> (gsq*Y^2*(7*gsq*T^2*Y^2 - 2*EulerGamma*gsq*T^2*Y^2 + 
       12*T^2*\[Lambda] + 36*\[Mu]sq + 2*gsq*T^2*Y^2*Log[4*Pi*T] - 
       2*gsq*T^2*Y^2*Log[\[Mu]]))/(144*Pi^2)}, 
 "ScalarMass3dLO" -> {\[Mu]sq3d -> (gsq*T^2*Y^2)/4 + (T^2*\[Lambda])/3 + 
     \[Mu]sq}, "ScalarMass3dNLO" -> 
  {\[Mu]sq3d -> (12*gsq*Y^2*(9*Lb*\[Mu]sq + 2*T^2*\[Lambda]*
         (1 + 6*EulerGamma - 3*Lb - 72*Log[Glaisher])) + 
      gsq^2*T^2*Y^4*(-8 - 108*EulerGamma + 69*Lb + 1296*Log[Glaisher]) - 
      24*\[Lambda]*(6*EulerGamma*T^2*\[Lambda] - Lb*T^2*\[Lambda] + 
        6*Lb*\[Mu]sq - 72*T^2*\[Lambda]*Log[Glaisher]) + 
      18*Log[\[Mu]3/\[Mu]]*(8*gsq3d^2*Y^4 - 16*gsq3d*Y^2*\[Lambda]3d + 
        16*\[Lambda]3d^2 + \[Lambda]VL[1]^2))/(576*Pi^2)}, 
 "Pressure3dLO" -> (2*Pi^2*T^4)/45, "Pressure3dNLO" -> 
  (-(T^4*(5*gsq*Y^2 + 4*\[Lambda])) - 24*T^2*\[Mu]sq)/288, 
 "Pressure3dNNLO" -> -1/69120*(2000*gsq^2*T^4*Y^4 - 
     40*EulerGamma*gsq^2*T^4*Y^4 - 175*gsq^2*Lb*T^4*Y^4 + 
     240*gsq*T^4*Y^2*\[Lambda] + 1440*EulerGamma*gsq*T^4*Y^2*\[Lambda] - 
     1080*gsq*Lb*T^4*Y^2*\[Lambda] + 696*T^4*\[Lambda]^2 - 
     480*EulerGamma*T^4*\[Lambda]^2 + 720*gsq*T^2*Y^2*\[Mu]sq + 
     4320*EulerGamma*gsq*T^2*Y^2*\[Mu]sq - 3240*gsq*Lb*T^2*Y^2*\[Mu]sq - 
     1440*Lb*T^2*\[Lambda]*\[Mu]sq + 2160*Lb*\[Mu]sq^2 + 
     8040*gsq^2*T^4*Y^4*Log[2] + 7200*T^4*\[Lambda]^2*Log[2] - 
     52320*gsq^2*T^4*Y^4*Log[Glaisher] - 34560*gsq*T^4*Y^2*\[Lambda]*
      Log[Glaisher] - 17280*T^4*\[Lambda]^2*Log[Glaisher] - 
     51840*gsq*T^2*Y^2*\[Mu]sq*Log[Glaisher] + 4020*gsq^2*T^4*Y^4*Log[Pi] + 
     2160*T^4*\[Lambda]^2*Log[Pi] + 1440*T^4*\[Lambda]^2*Log[Pi*T] - 
     720*gsq^2*T^4*Y^4*Log[4*Pi*T] + 1440*gsq*T^4*Y^2*\[Lambda]*Log[4*Pi*T] - 
     2400*T^4*\[Lambda]^2*Log[4*Pi*T] - 4020*gsq^2*T^4*Y^4*Log[\[Mu]/T] - 
     2160*T^4*\[Lambda]^2*Log[\[Mu]/T] + 360*gsq^2*T^4*Y^4*Log[\[Mu]^2] - 
     720*gsq*T^4*Y^2*\[Lambda]*Log[\[Mu]^2] + 480*T^4*\[Lambda]^2*
      Log[\[Mu]^2] + 132000*gsq^2*T^4*Y^4*Derivative[1][Zeta][-3] + 
     86400*T^4*\[Lambda]^2*Derivative[1][Zeta][-3])/Pi^2, 
 "Parameters3d" -> {gsq3d, \[Lambda]3d, \[Mu]sq3d}, 
 "VeffLO" -> (\[Mu]sq3d*\[Phi]^2)/2 + (\[Lambda]3d*\[Phi]^4)/4, 
 "VeffNLO" -> -1/6*(gsq3d*Y^2*\[Phi]^2)^(3/2)/Pi - 
   (\[Mu]sq3d + \[Lambda]3d*\[Phi]^2)^(3/2)/(12*Pi) - 
   (\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2)^(3/2)/(12*Pi), 
 "VeffNNLO" -> (gsq3d*Y^2*Sqrt[gsq3d*Y^2*\[Phi]^2]*
     Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]^2])/(16*Pi^2) + 
   (3*\[Lambda]3d*(\[Mu]sq3d + \[Lambda]3d*\[Phi]^2))/(64*Pi^2) + 
   (gsq3d*Y^2*Sqrt[gsq3d*Y^2*\[Phi]^2]*
     Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2])/(16*Pi^2) + 
   (\[Lambda]3d*Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]^2]*
     Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2])/(32*Pi^2) + 
   (3*\[Lambda]3d*(\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2))/(64*Pi^2) - 
   (3*\[Lambda]3d^2*\[Phi]^2*
     (1/2 + Log[\[Mu]3US/(3*Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2])]))/
    (16*Pi^2) + ((gsq3d^2*Y^4*\[Phi]^4)/(8*Pi^2) - 
     (gsq3d*Y^2*\[Phi]^2*(-\[Mu]sq3d + 2*gsq3d*Y^2*\[Phi]^2 - 
        3*\[Lambda]3d*\[Phi]^2))/(16*Pi^2) + 
     (gsq3d*Y^2*\[Phi]^2*Sqrt[gsq3d*Y^2*\[Phi]^2]*
       Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2])/(8*Pi^2) - 
     ((\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2)^2*
       (1/2 + Log[\[Mu]3US/Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2]]))/
      (16*Pi^2) + ((-\[Mu]sq3d + gsq3d*Y^2*\[Phi]^2 - 3*\[Lambda]3d*\[Phi]^2)^
        2*(1/2 + Log[\[Mu]3US/(Sqrt[gsq3d*Y^2*\[Phi]^2] + 
           Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2])]))/(8*Pi^2) - 
     ((7*gsq3d^2*Y^4*\[Phi]^4 + (-\[Mu]sq3d + gsq3d*Y^2*\[Phi]^2 - 
          3*\[Lambda]3d*\[Phi]^2)^2 - 2*gsq3d*Y^2*\[Phi]^2*
         (\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2))*
       (1/2 + Log[\[Mu]3US/(2*Sqrt[gsq3d*Y^2*\[Phi]^2] + 
           Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2])]))/(16*Pi^2))/
    (4*\[Phi]^2) + 
   (-1/16*(Sqrt[gsq3d*Y^2*\[Phi]^2]*(gsq3d*Y^2*\[Phi]^2 - 
         2*\[Lambda]3d*\[Phi]^2)*Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]^2])/
       Pi^2 + (Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2]*
       ((gsq3d*Y^2*\[Phi]^2*Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]^2])/(4*Pi) - 
        (Sqrt[gsq3d*Y^2*\[Phi]^2]*(gsq3d*Y^2*\[Phi]^2 + 2*\[Lambda]3d*
            \[Phi]^2))/(4*Pi)))/(4*Pi) + 
     (\[Lambda]3d^2*\[Phi]^4*(1/2 + 
        Log[\[Mu]3US/(Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]^2] + 
           Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2])]))/(4*Pi^2) - 
     ((gsq3d^2*Y^4*\[Phi]^4 + 4*\[Lambda]3d^2*\[Phi]^4 - 
        2*gsq3d*Y^2*\[Phi]^2*(2*\[Mu]sq3d + 4*\[Lambda]3d*\[Phi]^2))*
       (1/2 + Log[\[Mu]3US/(Sqrt[gsq3d*Y^2*\[Phi]^2] + 
           Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]^2] + 
           Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2])]))/(16*Pi^2))/
    (4*\[Phi]^2) + 
   (-1/16*(Sqrt[gsq3d*Y^2*\[Phi]^2]*(gsq3d*Y^2*\[Phi]^2 + 
         2*\[Lambda]3d*\[Phi]^2)*Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2])/
       Pi^2 + (Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]^2]*
       (-1/4*(Sqrt[gsq3d*Y^2*\[Phi]^2]*(gsq3d*Y^2*\[Phi]^2 - 
            2*\[Lambda]3d*\[Phi]^2))/Pi + (gsq3d*Y^2*\[Phi]^2*
          Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2])/(4*Pi)))/(4*Pi) + 
     (\[Lambda]3d^2*\[Phi]^4*(1/2 + 
        Log[\[Mu]3US/(Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]^2] + 
           Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2])]))/(4*Pi^2) - 
     ((gsq3d^2*Y^4*\[Phi]^4 + 4*\[Lambda]3d^2*\[Phi]^4 - 
        2*gsq3d*Y^2*\[Phi]^2*(2*\[Mu]sq3d + 4*\[Lambda]3d*\[Phi]^2))*
       (1/2 + Log[\[Mu]3US/(Sqrt[gsq3d*Y^2*\[Phi]^2] + 
           Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]^2] + 
           Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2])]))/(16*Pi^2))/
    (4*\[Phi]^2) - (\[Lambda]3d^2*\[Phi]^2*
     (1/2 + Log[\[Mu]3US/(2*Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]^2] + 
         Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2])]))/(16*Pi^2), 
 "VEV" -> {\[Phi]}, "SquareMassMatrixScalar" -> 
  {{\[Mu]sq3d + \[Lambda]3d*\[Phi]^2, 0}, 
   {0, \[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2}}, "SquareMassMatrixVector" -> 
  {{gsq3d*Y^2*\[Phi]^2}}, "VTotLO" -> (\[Mu]sq3d*\[Phi]^2)/2 + 
   (\[Lambda]3d*\[Phi]^4)/4, "VTotNLO" -> 
  -1/12*(-6*Pi*\[Mu]sq3d*\[Phi]^2 - 3*Pi*\[Lambda]3d*\[Phi]^4 + 
     2*(gsq3d*Y^2*\[Phi]^2)^(3/2) + (\[Mu]sq3d + \[Lambda]3d*\[Phi]^2)^
      (3/2) + (\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2)^(3/2))/Pi, 
 "VTotNNLO" -> (6*gsq3d^2*Y^4*\[Phi]^4 + 96*Pi^2*\[Mu]sq3d*\[Phi]^4 + 
    48*Pi^2*\[Lambda]3d*\[Phi]^6 - 32*Pi*\[Phi]^2*(gsq3d*Y^2*\[Phi]^2)^
      (3/2) - 3*(gsq3d*Y^2 - 2*\[Lambda]3d)*\[Phi]^2*Sqrt[gsq3d*Y^2*\[Phi]^2]*
     Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]^2] + 12*(gsq3d*Y^2*\[Phi]^2)^(3/2)*
     Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]^2] + 9*\[Lambda]3d*\[Phi]^2*
     (\[Mu]sq3d + \[Lambda]3d*\[Phi]^2) - 16*Pi*\[Phi]^2*
     (\[Mu]sq3d + \[Lambda]3d*\[Phi]^2)^(3/2) - 3*(gsq3d*Y^2 + 2*\[Lambda]3d)*
     \[Phi]^2*Sqrt[gsq3d*Y^2*\[Phi]^2]*
     Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2] + 18*(gsq3d*Y^2*\[Phi]^2)^(3/2)*
     Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2] + 6*\[Lambda]3d*\[Phi]^2*
     Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]^2]*
     Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2] + 9*\[Lambda]3d*\[Phi]^2*
     (\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2) - 16*Pi*\[Phi]^2*
     (\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2)^(3/2) + 
    3*gsq3d*Y^2*\[Phi]^2*(\[Mu]sq3d + (-2*gsq3d*Y^2 + 3*\[Lambda]3d)*
       \[Phi]^2) + 3*\[Phi]^2*Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2]*
     (-((gsq3d*Y^2 + 2*\[Lambda]3d)*Sqrt[gsq3d*Y^2*\[Phi]^2]) + 
      gsq3d*Y^2*Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]^2]) - 
    3*\[Phi]^2*Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]^2]*
     ((gsq3d*Y^2 - 2*\[Lambda]3d)*Sqrt[gsq3d*Y^2*\[Phi]^2] - 
      gsq3d*Y^2*Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2]) - 
    18*\[Lambda]3d^2*\[Phi]^4*
     (1 + 2*Log[\[Mu]3US/(3*Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2])]) - 
    3*(\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2)^2*
     (1/2 + Log[\[Mu]3US/Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2]]) + 
    3*(\[Mu]sq3d + (-(gsq3d*Y^2) + 3*\[Lambda]3d)*\[Phi]^2)^2*
     (1 + 2*Log[\[Mu]3US/(Sqrt[gsq3d*Y^2*\[Phi]^2] + 
          Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2])]) - 
    (3*(\[Mu]sq3d^2 + 2*(-2*gsq3d*Y^2 + 3*\[Lambda]3d)*\[Mu]sq3d*\[Phi]^2 + 
       (8*gsq3d^2*Y^4 - 12*gsq3d*Y^2*\[Lambda]3d + 9*\[Lambda]3d^2)*\[Phi]^4)*
      (1 + 2*Log[\[Mu]3US/(2*Sqrt[gsq3d*Y^2*\[Phi]^2] + 
           Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2])]))/2 + 
    12*\[Lambda]3d^2*\[Phi]^4*
     (1 + 2*Log[\[Mu]3US/(Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]^2] + 
          Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2])]) - 
    6*(gsq3d^2*Y^4*\[Phi]^4 + 4*\[Lambda]3d^2*\[Phi]^4 - 
      4*gsq3d*Y^2*\[Phi]^2*(\[Mu]sq3d + 2*\[Lambda]3d*\[Phi]^2))*
     (1/2 + Log[\[Mu]3US/(Sqrt[gsq3d*Y^2*\[Phi]^2] + 
         Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]^2] + 
         Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2])]) - 
    6*\[Lambda]3d^2*\[Phi]^4*
     (1 + 2*Log[\[Mu]3US/(2*Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]^2] + 
          Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]^2])]))/(192*Pi^2*\[Phi]^2), 
 "VTot" -> Function[{\[Phi]$, order$}, Switch[order$, "LO", 
    (\[Mu]sq3d*\[Phi]$^2)/2 + (\[Lambda]3d*\[Phi]$^4)/4, "NLO", 
    -1/12*(-6*Pi*\[Mu]sq3d*\[Phi]$^2 - 3*Pi*\[Lambda]3d*\[Phi]$^4 + 
       2*(gsq3d*Y^2*\[Phi]$^2)^(3/2) + (\[Mu]sq3d + \[Lambda]3d*\[Phi]$^2)^
        (3/2) + (\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]$^2)^(3/2))/Pi, "NNLO", 
    (6*gsq3d^2*Y^4*\[Phi]$^4 + 96*Pi^2*\[Mu]sq3d*\[Phi]$^4 + 
      48*Pi^2*\[Lambda]3d*\[Phi]$^6 - 32*Pi*\[Phi]$^2*(gsq3d*Y^2*\[Phi]$^2)^
        (3/2) - 3*(gsq3d*Y^2 - 2*\[Lambda]3d)*\[Phi]$^2*
       Sqrt[gsq3d*Y^2*\[Phi]$^2]*Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]$^2] + 
      12*(gsq3d*Y^2*\[Phi]$^2)^(3/2)*Sqrt[\[Mu]sq3d + 
         \[Lambda]3d*\[Phi]$^2] + 9*\[Lambda]3d*\[Phi]$^2*
       (\[Mu]sq3d + \[Lambda]3d*\[Phi]$^2) - 16*Pi*\[Phi]$^2*
       (\[Mu]sq3d + \[Lambda]3d*\[Phi]$^2)^(3/2) - 
      3*(gsq3d*Y^2 + 2*\[Lambda]3d)*\[Phi]$^2*Sqrt[gsq3d*Y^2*\[Phi]$^2]*
       Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]$^2] + 
      18*(gsq3d*Y^2*\[Phi]$^2)^(3/2)*Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*
          \[Phi]$^2] + 6*\[Lambda]3d*\[Phi]$^2*
       Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]$^2]*
       Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]$^2] + 9*\[Lambda]3d*\[Phi]$^2*
       (\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]$^2) - 16*Pi*\[Phi]$^2*
       (\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]$^2)^(3/2) + 
      3*gsq3d*Y^2*\[Phi]$^2*(\[Mu]sq3d + (-2*gsq3d*Y^2 + 3*\[Lambda]3d)*
         \[Phi]$^2) + 3*\[Phi]$^2*Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]$^2]*
       (-((gsq3d*Y^2 + 2*\[Lambda]3d)*Sqrt[gsq3d*Y^2*\[Phi]$^2]) + 
        gsq3d*Y^2*Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]$^2]) - 
      3*\[Phi]$^2*Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]$^2]*
       ((gsq3d*Y^2 - 2*\[Lambda]3d)*Sqrt[gsq3d*Y^2*\[Phi]$^2] - 
        gsq3d*Y^2*Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]$^2]) - 
      18*\[Lambda]3d^2*\[Phi]$^4*
       (1 + 2*Log[\[Mu]3US/(3*Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]$^2])]) - 
      3*(\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]$^2)^2*
       (1/2 + Log[\[Mu]3US/Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]$^2]]) + 
      3*(\[Mu]sq3d + (-(gsq3d*Y^2) + 3*\[Lambda]3d)*\[Phi]$^2)^2*
       (1 + 2*Log[\[Mu]3US/(Sqrt[gsq3d*Y^2*\[Phi]$^2] + 
            Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]$^2])]) - 
      (3*(\[Mu]sq3d^2 + 2*(-2*gsq3d*Y^2 + 3*\[Lambda]3d)*\[Mu]sq3d*
          \[Phi]$^2 + (8*gsq3d^2*Y^4 - 12*gsq3d*Y^2*\[Lambda]3d + 
           9*\[Lambda]3d^2)*\[Phi]$^4)*
        (1 + 2*Log[\[Mu]3US/(2*Sqrt[gsq3d*Y^2*\[Phi]$^2] + 
             Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]$^2])]))/2 + 
      12*\[Lambda]3d^2*\[Phi]$^4*
       (1 + 2*Log[\[Mu]3US/(Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]$^2] + 
            Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]$^2])]) - 
      6*(gsq3d^2*Y^4*\[Phi]$^4 + 4*\[Lambda]3d^2*\[Phi]$^4 - 
        4*gsq3d*Y^2*\[Phi]$^2*(\[Mu]sq3d + 2*\[Lambda]3d*\[Phi]$^2))*
       (1/2 + Log[\[Mu]3US/(Sqrt[gsq3d*Y^2*\[Phi]$^2] + 
           Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]$^2] + 
           Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]$^2])]) - 
      6*\[Lambda]3d^2*\[Phi]$^4*
       (1 + 2*Log[\[Mu]3US/(2*Sqrt[\[Mu]sq3d + \[Lambda]3d*\[Phi]$^2] + 
            Sqrt[\[Mu]sq3d + 3*\[Lambda]3d*\[Phi]$^2])]))/
     (192*Pi^2*\[Phi]$^2)]]|>
