<|"BetaFunctions" -> {gsq -> (gsq^2*Y\[Phi]^2)/(24*Pi^2), 
   \[Lambda] -> (3*gsq^2*Y\[Phi]^4 - 6*gsq*Y\[Phi]^2*\[Lambda] + 
      10*\[Lambda]^2)/(8*Pi^2), 
   msq -> -1/8*(msq*(3*gsq*Y\[Phi]^2 - 4*\[Lambda]))/Pi^2}, 
 "BetaFunctions3d" -> 
  {msq -> (8*gsq3d^2*Y\[Phi]^4 - 16*gsq3d*Y\[Phi]^2*\[Lambda]3d + 
      16*\[Lambda]3d^2 + \[Lambda]VL[1]^2)/(32*Pi^2)}, 
 "Constants3d" -> {Lb -> 2*EulerGamma - 2*Log[4*Pi] + Log[\[Mu]^2/T^2], 
   Lf -> 2*EulerGamma + 4*Log[2] - 2*Log[4*Pi] + Log[\[Mu]^2/T^2]}, 
 "Couplings3d" -> {gsq3d -> gsq*T - (gsq^2*Lb*T*Y\[Phi]^2)/(48*Pi^2), 
   \[Lambda]3d -> (T*(gsq^2*(2 - 3*Lb)*Y\[Phi]^4 + 6*gsq*Lb*Y\[Phi]^2*
        \[Lambda] + 2*\[Lambda]*(8*Pi^2 - 5*Lb*\[Lambda])))/(16*Pi^2)}, 
 "TempScalar3d" -> {\[Lambda]VLL[1] -> (gsq^2*T*Y\[Phi]^4)/Pi^2, 
   \[Lambda]VL[1] -> (gsq*T*Y\[Phi]^2*(48*Pi^2 - gsq*(-4 + Lb)*Y\[Phi]^2 + 
       24*\[Lambda]))/(24*Pi^2)}, "DebyeMass3dLO" -> 
  {\[Mu]sqU1 -> (gsq*T^2*Y\[Phi]^2)/3}, "DebyeMass3dNLO" -> 
  {\[Mu]sqU1 -> (gsq*Y\[Phi]^2*(36*msq + 7*gsq*T^2*Y\[Phi]^2 - 
       2*EulerGamma*gsq*T^2*Y\[Phi]^2 + 12*T^2*\[Lambda] + 
       2*gsq*T^2*Y\[Phi]^2*Log[4*Pi*T] - 2*gsq*T^2*Y\[Phi]^2*Log[\[Mu]]))/
     (144*Pi^2)}, "ScalarMass3dLO" -> 
  {msq3d -> msq + (T^2*(3*gsq*Y\[Phi]^2 + 4*\[Lambda]))/12}, 
 "ScalarMass3dNLO" -> 
  {msq3d -> (12*gsq*Y\[Phi]^2*(Lb*(9*msq - 6*T^2*\[Lambda]) + 
        2*T^2*\[Lambda]*(1 + 6*EulerGamma - 72*Log[Glaisher])) + 
      24*\[Lambda]*(Lb*(-6*msq + T^2*\[Lambda]) - 6*T^2*\[Lambda]*
         (EulerGamma - 12*Log[Glaisher])) + gsq^2*T^2*Y\[Phi]^4*
       (-8 - 108*EulerGamma + 69*Lb + 1296*Log[Glaisher]) + 
      18*Log[\[Mu]3/\[Mu]]*(8*gsq3d^2*Y\[Phi]^4 - 16*gsq3d*Y\[Phi]^2*
         \[Lambda]3d + 16*\[Lambda]3d^2 + \[Lambda]VL[1]^2))/(576*Pi^2)}, 
 "Pressure3dLO" -> (2*Pi^2*T^4)/45, "Pressure3dNLO" -> 
  (-24*msq*T^2 - T^4*(5*gsq*Y\[Phi]^2 + 4*\[Lambda]))/288, 
 "Pressure3dNNLO" -> -1/69120*(2160*Lb*msq^2 + 720*gsq*msq*T^2*Y\[Phi]^2 + 
     4320*EulerGamma*gsq*msq*T^2*Y\[Phi]^2 - 3240*gsq*Lb*msq*T^2*Y\[Phi]^2 + 
     2000*gsq^2*T^4*Y\[Phi]^4 - 40*EulerGamma*gsq^2*T^4*Y\[Phi]^4 - 
     175*gsq^2*Lb*T^4*Y\[Phi]^4 - 1440*Lb*msq*T^2*\[Lambda] + 
     240*gsq*T^4*Y\[Phi]^2*\[Lambda] + 1440*EulerGamma*gsq*T^4*Y\[Phi]^2*
      \[Lambda] - 1080*gsq*Lb*T^4*Y\[Phi]^2*\[Lambda] + 696*T^4*\[Lambda]^2 - 
     480*EulerGamma*T^4*\[Lambda]^2 + 8040*gsq^2*T^4*Y\[Phi]^4*Log[2] + 
     7200*T^4*\[Lambda]^2*Log[2] - 51840*gsq*msq*T^2*Y\[Phi]^2*
      Log[Glaisher] - 52320*gsq^2*T^4*Y\[Phi]^4*Log[Glaisher] - 
     34560*gsq*T^4*Y\[Phi]^2*\[Lambda]*Log[Glaisher] - 
     17280*T^4*\[Lambda]^2*Log[Glaisher] + 4020*gsq^2*T^4*Y\[Phi]^4*Log[Pi] + 
     2160*T^4*\[Lambda]^2*Log[Pi] + 1440*T^4*\[Lambda]^2*Log[Pi*T] - 
     720*gsq^2*T^4*Y\[Phi]^4*Log[4*Pi*T] + 1440*gsq*T^4*Y\[Phi]^2*\[Lambda]*
      Log[4*Pi*T] - 2400*T^4*\[Lambda]^2*Log[4*Pi*T] - 
     4020*gsq^2*T^4*Y\[Phi]^4*Log[\[Mu]/T] - 2160*T^4*\[Lambda]^2*
      Log[\[Mu]/T] + 360*gsq^2*T^4*Y\[Phi]^4*Log[\[Mu]^2] - 
     720*gsq*T^4*Y\[Phi]^2*\[Lambda]*Log[\[Mu]^2] + 
     480*T^4*\[Lambda]^2*Log[\[Mu]^2] + 132000*gsq^2*T^4*Y\[Phi]^4*
      Derivative[1][Zeta][-3] + 86400*T^4*\[Lambda]^2*
      Derivative[1][Zeta][-3])/Pi^2, "Parameters3d" -> 
  {gsq3d, \[Lambda]3d, msq3d}, "VeffLO" -> (msq3d*\[Phi]^2)/2 + 
   (\[Lambda]3d*\[Phi]^4)/4, "VeffNLO" -> 
  -1/6*(gsq3d*Y\[Phi]^2*\[Phi]^2)^(3/2)/Pi - 
   (msq3d + \[Lambda]3d*\[Phi]^2)^(3/2)/(12*Pi) - 
   (msq3d + 3*\[Lambda]3d*\[Phi]^2)^(3/2)/(12*Pi), 
 "VeffNNLO" -> (gsq3d*Y\[Phi]^2*Sqrt[gsq3d*Y\[Phi]^2*\[Phi]^2]*
     Sqrt[msq3d + \[Lambda]3d*\[Phi]^2])/(16*Pi^2) + 
   (3*\[Lambda]3d*(msq3d + \[Lambda]3d*\[Phi]^2))/(64*Pi^2) + 
   (gsq3d*Y\[Phi]^2*Sqrt[gsq3d*Y\[Phi]^2*\[Phi]^2]*
     Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2])/(16*Pi^2) + 
   (\[Lambda]3d*Sqrt[msq3d + \[Lambda]3d*\[Phi]^2]*
     Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2])/(32*Pi^2) + 
   (3*\[Lambda]3d*(msq3d + 3*\[Lambda]3d*\[Phi]^2))/(64*Pi^2) - 
   (3*\[Lambda]3d^2*\[Phi]^2*
     (1/2 + Log[\[Mu]3US/(3*Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2])]))/
    (16*Pi^2) + ((gsq3d^2*Y\[Phi]^4*\[Phi]^4)/(8*Pi^2) - 
     (gsq3d*Y\[Phi]^2*\[Phi]^2*(-msq3d + 2*gsq3d*Y\[Phi]^2*\[Phi]^2 - 
        3*\[Lambda]3d*\[Phi]^2))/(16*Pi^2) + 
     (gsq3d*Y\[Phi]^2*\[Phi]^2*Sqrt[gsq3d*Y\[Phi]^2*\[Phi]^2]*
       Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2])/(8*Pi^2) - 
     ((msq3d + 3*\[Lambda]3d*\[Phi]^2)^2*
       (1/2 + Log[\[Mu]3US/Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2]]))/
      (16*Pi^2) + ((-msq3d + gsq3d*Y\[Phi]^2*\[Phi]^2 - 
         3*\[Lambda]3d*\[Phi]^2)^2*
       (1/2 + Log[\[Mu]3US/(Sqrt[gsq3d*Y\[Phi]^2*\[Phi]^2] + 
           Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2])]))/(8*Pi^2) - 
     ((7*gsq3d^2*Y\[Phi]^4*\[Phi]^4 + (-msq3d + gsq3d*Y\[Phi]^2*\[Phi]^2 - 
          3*\[Lambda]3d*\[Phi]^2)^2 - 2*gsq3d*Y\[Phi]^2*\[Phi]^2*
         (msq3d + 3*\[Lambda]3d*\[Phi]^2))*
       (1/2 + Log[\[Mu]3US/(2*Sqrt[gsq3d*Y\[Phi]^2*\[Phi]^2] + 
           Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2])]))/(16*Pi^2))/(4*\[Phi]^2) + 
   (-1/16*(Sqrt[gsq3d*Y\[Phi]^2*\[Phi]^2]*(gsq3d*Y\[Phi]^2*\[Phi]^2 - 
         2*\[Lambda]3d*\[Phi]^2)*Sqrt[msq3d + \[Lambda]3d*\[Phi]^2])/Pi^2 + 
     (Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2]*
       ((gsq3d*Y\[Phi]^2*\[Phi]^2*Sqrt[msq3d + \[Lambda]3d*\[Phi]^2])/
         (4*Pi) - (Sqrt[gsq3d*Y\[Phi]^2*\[Phi]^2]*(gsq3d*Y\[Phi]^2*\[Phi]^2 + 
           2*\[Lambda]3d*\[Phi]^2))/(4*Pi)))/(4*Pi) + 
     (\[Lambda]3d^2*\[Phi]^4*(1/2 + 
        Log[\[Mu]3US/(Sqrt[msq3d + \[Lambda]3d*\[Phi]^2] + 
           Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2])]))/(4*Pi^2) - 
     ((gsq3d^2*Y\[Phi]^4*\[Phi]^4 + 4*\[Lambda]3d^2*\[Phi]^4 - 
        2*gsq3d*Y\[Phi]^2*\[Phi]^2*(2*msq3d + 4*\[Lambda]3d*\[Phi]^2))*
       (1/2 + Log[\[Mu]3US/(Sqrt[gsq3d*Y\[Phi]^2*\[Phi]^2] + 
           Sqrt[msq3d + \[Lambda]3d*\[Phi]^2] + 
           Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2])]))/(16*Pi^2))/(4*\[Phi]^2) + 
   (-1/16*(Sqrt[gsq3d*Y\[Phi]^2*\[Phi]^2]*(gsq3d*Y\[Phi]^2*\[Phi]^2 + 
         2*\[Lambda]3d*\[Phi]^2)*Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2])/Pi^2 + 
     (Sqrt[msq3d + \[Lambda]3d*\[Phi]^2]*
       (-1/4*(Sqrt[gsq3d*Y\[Phi]^2*\[Phi]^2]*(gsq3d*Y\[Phi]^2*\[Phi]^2 - 
            2*\[Lambda]3d*\[Phi]^2))/Pi + (gsq3d*Y\[Phi]^2*\[Phi]^2*
          Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2])/(4*Pi)))/(4*Pi) + 
     (\[Lambda]3d^2*\[Phi]^4*(1/2 + 
        Log[\[Mu]3US/(Sqrt[msq3d + \[Lambda]3d*\[Phi]^2] + 
           Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2])]))/(4*Pi^2) - 
     ((gsq3d^2*Y\[Phi]^4*\[Phi]^4 + 4*\[Lambda]3d^2*\[Phi]^4 - 
        2*gsq3d*Y\[Phi]^2*\[Phi]^2*(2*msq3d + 4*\[Lambda]3d*\[Phi]^2))*
       (1/2 + Log[\[Mu]3US/(Sqrt[gsq3d*Y\[Phi]^2*\[Phi]^2] + 
           Sqrt[msq3d + \[Lambda]3d*\[Phi]^2] + 
           Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2])]))/(16*Pi^2))/(4*\[Phi]^2) - 
   (\[Lambda]3d^2*\[Phi]^2*
     (1/2 + Log[\[Mu]3US/(2*Sqrt[msq3d + \[Lambda]3d*\[Phi]^2] + 
         Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2])]))/(16*Pi^2), 
 "\[Phi]Vev" -> {\[Phi]}, "VTotLO" -> (msq3d*\[Phi]^2)/2 + 
   (\[Lambda]3d*\[Phi]^4)/4, "VTotNLO" -> 
  -1/12*(-6*msq3d*Pi*\[Phi]^2 - 3*Pi*\[Lambda]3d*\[Phi]^4 + 
     2*(gsq3d*Y\[Phi]^2*\[Phi]^2)^(3/2) + (msq3d + \[Lambda]3d*\[Phi]^2)^
      (3/2) + (msq3d + 3*\[Lambda]3d*\[Phi]^2)^(3/2))/Pi, 
 "VTotNNLO" -> (96*msq3d*Pi^2*\[Phi]^4 + 6*gsq3d^2*Y\[Phi]^4*\[Phi]^4 + 
    48*Pi^2*\[Lambda]3d*\[Phi]^6 - 32*Pi*\[Phi]^2*(gsq3d*Y\[Phi]^2*\[Phi]^2)^
      (3/2) - 3*(gsq3d*Y\[Phi]^2 - 2*\[Lambda]3d)*\[Phi]^2*
     Sqrt[gsq3d*Y\[Phi]^2*\[Phi]^2]*Sqrt[msq3d + \[Lambda]3d*\[Phi]^2] + 
    12*(gsq3d*Y\[Phi]^2*\[Phi]^2)^(3/2)*Sqrt[msq3d + \[Lambda]3d*\[Phi]^2] + 
    9*\[Lambda]3d*\[Phi]^2*(msq3d + \[Lambda]3d*\[Phi]^2) - 
    16*Pi*\[Phi]^2*(msq3d + \[Lambda]3d*\[Phi]^2)^(3/2) - 
    3*(gsq3d*Y\[Phi]^2 + 2*\[Lambda]3d)*\[Phi]^2*
     Sqrt[gsq3d*Y\[Phi]^2*\[Phi]^2]*Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2] + 
    18*(gsq3d*Y\[Phi]^2*\[Phi]^2)^(3/2)*
     Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2] + 6*\[Lambda]3d*\[Phi]^2*
     Sqrt[msq3d + \[Lambda]3d*\[Phi]^2]*
     Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2] + 9*\[Lambda]3d*\[Phi]^2*
     (msq3d + 3*\[Lambda]3d*\[Phi]^2) - 16*Pi*\[Phi]^2*
     (msq3d + 3*\[Lambda]3d*\[Phi]^2)^(3/2) + 3*gsq3d*Y\[Phi]^2*\[Phi]^2*
     (msq3d + (-2*gsq3d*Y\[Phi]^2 + 3*\[Lambda]3d)*\[Phi]^2) + 
    3*\[Phi]^2*Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2]*
     (-((gsq3d*Y\[Phi]^2 + 2*\[Lambda]3d)*Sqrt[gsq3d*Y\[Phi]^2*\[Phi]^2]) + 
      gsq3d*Y\[Phi]^2*Sqrt[msq3d + \[Lambda]3d*\[Phi]^2]) - 
    3*\[Phi]^2*Sqrt[msq3d + \[Lambda]3d*\[Phi]^2]*
     ((gsq3d*Y\[Phi]^2 - 2*\[Lambda]3d)*Sqrt[gsq3d*Y\[Phi]^2*\[Phi]^2] - 
      gsq3d*Y\[Phi]^2*Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2]) - 
    18*\[Lambda]3d^2*\[Phi]^4*
     (1 + 2*Log[\[Mu]3US/(3*Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2])]) - 
    3*(msq3d + 3*\[Lambda]3d*\[Phi]^2)^2*
     (1/2 + Log[\[Mu]3US/Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2]]) + 
    3*(msq3d + (-(gsq3d*Y\[Phi]^2) + 3*\[Lambda]3d)*\[Phi]^2)^2*
     (1 + 2*Log[\[Mu]3US/(Sqrt[gsq3d*Y\[Phi]^2*\[Phi]^2] + 
          Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2])]) - 
    (3*(msq3d^2 + 2*msq3d*(-2*gsq3d*Y\[Phi]^2 + 3*\[Lambda]3d)*\[Phi]^2 + 
       (8*gsq3d^2*Y\[Phi]^4 - 12*gsq3d*Y\[Phi]^2*\[Lambda]3d + 
         9*\[Lambda]3d^2)*\[Phi]^4)*
      (1 + 2*Log[\[Mu]3US/(2*Sqrt[gsq3d*Y\[Phi]^2*\[Phi]^2] + 
           Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2])]))/2 + 
    12*\[Lambda]3d^2*\[Phi]^4*
     (1 + 2*Log[\[Mu]3US/(Sqrt[msq3d + \[Lambda]3d*\[Phi]^2] + 
          Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2])]) - 
    6*(gsq3d^2*Y\[Phi]^4*\[Phi]^4 + 4*\[Lambda]3d^2*\[Phi]^4 - 
      4*gsq3d*Y\[Phi]^2*\[Phi]^2*(msq3d + 2*\[Lambda]3d*\[Phi]^2))*
     (1/2 + Log[\[Mu]3US/(Sqrt[gsq3d*Y\[Phi]^2*\[Phi]^2] + 
         Sqrt[msq3d + \[Lambda]3d*\[Phi]^2] + 
         Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2])]) - 
    6*\[Lambda]3d^2*\[Phi]^4*
     (1 + 2*Log[\[Mu]3US/(2*Sqrt[msq3d + \[Lambda]3d*\[Phi]^2] + 
          Sqrt[msq3d + 3*\[Lambda]3d*\[Phi]^2])]))/(192*Pi^2*\[Phi]^2), 
 "VTot" -> Function[{\[Phi]$, order$}, Switch[order$, "LO", 
    DRExpressions["VTotLO"] /. vev$690428 -> \[Phi]$, "NLO", 
    DRExpressions["VTotNLO"] /. vev$690428 -> \[Phi]$, "NNLO", 
    DRExpressions["VTotNNLO"] /. vev$690428 -> \[Phi]$]]|>
