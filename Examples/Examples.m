(* ::Package:: *)

(* ::Title:: *)
(*Examples Package*)


BeginPackage["Examples`"]


(* ::Subsection:: *)
(*Available public functions*)


(* Models *)
CFFModel;
AHModel;


(* analytical action *)
ActionFromPolynomialCoefficients;
ActionPolynomial;


(* analytic nucleation *)
NucleationFromCoefficientsCFF;
NucleationCFF;


Begin["Private`"];


(* ::Section:: *)
(*Code*)


(* ::Subsection:: *)
(*Utilities*)


(* ::Subsubsection:: *)
(*Autocomplete arguments*)


(* 
https://resources.wolframcloud.com/FunctionRepository/resources/AddCodeCompletion
https://mathematica.stackexchange.com/questions/56984
*)
addCodeCompletion[function_String][args___]:=Module[
	{processed},
	processed={args}/.{
		None -> 0,
		"AbsoluteFileName" -> 2,
		"RelativeFileName" -> 3,
		"Color" -> 4,
		"PackageName" -> 7,
		"DirectoryName" -> 8,
		"InterpreterType" -> 9
	};
	(FE`Evaluate[FEPrivate`AddSpecialArgCompletion[#1]]&)[function -> processed]
];


SetAttributes[AutoComplete,HoldFirst];
AutoComplete[sym_]:=Module[{name,keys,keySeq,min,max},
	Switch[sym,
		_Symbol,
			name=ToString@sym;
			keys=Keys@Options@sym;
			(* count min/max number of arguments *)
			{min,max}=MinMax[Length/@List@@ReleaseHold[DownValues[sym][[All,1]]]] - 1/.{\[Infinity],-\[Infinity]}->{0,0};
			keySeq=Sequence@@Join[Table[0,min],ConstantArray[keys,Length@keys+(max-min)]],
		_Association,
			name=ToString@HoldForm@sym;
			keySeq=Keys@sym,
		_Transition|_ActionFunction,
			name=ToString@HoldForm@sym;
			keySeq=Keys@sym[Association]
		];
	addCodeCompletion[name][keySeq]
	]
(* autocomplete options from multiple functions *)
AutoComplete[sym_Symbol,functions_List]:=Module[
	{name,keys,keySeq},
	name=ToString@sym;
	keys=Keys[Union@@Options/@functions];
	keySeq=Sequence@@ConstantArray[keys,Length@keys];
	addCodeCompletion[name][keySeq]
	]


SetAttributes[AutoCompleteModel,HoldFirst];
AutoCompleteModel[model_]:=Module[{},addCodeCompletion["CFFModel"][0,{"Benchmark","Transition"}];]


(* ::Subsubsection::Closed:: *)
(*Messages*)


msg["badPolynomial"]="To derive the analytic bounce action, the potential must be of the form V[\[Phi],T]:=\!\(\*SubscriptBox[\(c\), \(2\)]\)[T]\!\(\*SuperscriptBox[\(\[Phi]\), \(2\)]\)+\!\(\*SubscriptBox[\(c\), \(3\)]\)[T]\!\(\*SuperscriptBox[\(\[Phi]\), \(3\)]\)+\!\(\*SubscriptBox[\(c\), \(4\)]\)[T]\!\(\*SuperscriptBox[\(\[Phi]\), \(4\)]\).\nBad field coefficients:\n";


msg["badPolynomialTn"]="To derive the nucleation temperature, the potential must be of the form V[\[Phi],T]:=\!\(\*SubscriptBox[\(a\), \(2\)]\)(\!\(\*SuperscriptBox[\(T\), \(2\)]\)-\!\(\*SubsuperscriptBox[\(T\), \(0\), \(2\)]\))\!\(\*SuperscriptBox[\(\[Phi]\), \(2\)]\) + \!\(\*SubscriptBox[\(a\), \(3\)]\)(T+\!\(\*SubscriptBox[\(T\), \(1\)]\))\!\(\*SuperscriptBox[\(\[Phi]\), \(3\)]\) + \!\(\*SubscriptBox[\(a\), \(4\)]\)\!\(\*SuperscriptBox[\(\[Phi]\), \(4\)]\).\nBad field coefficients:\n";
msg["badNucleationOption"]=StringTemplate["Bad nucleation expansion option \!\(\*
StyleBox[\"``\",\nStripOnInput->False,\nLineColor->RGBColor[1, 0, 0],\nFrontFaceColor->RGBColor[1, 0, 0],\nBackFaceColor->RGBColor[1, 0, 0],\nGraphicsColor->RGBColor[1, 0, 0],\nFontColor->RGBColor[1, 0, 0]]\). Should match \"Inflection\", \"Critical\" or Automatic."];


(* ::Subsection:: *)
(*Coupled Fluid-Field Model*)


(* ::Text:: *)
(*A minimal model featuring first-order phase transition, see arXiv 1504.03291.*)


(* coupled fluid-field model *)
CFFModel::usage="CFFModel[] gives the coupled fluid-field model, see \!\(\*TemplateBox[{\"\\\"1504.03291\\\"\", \"https://arxiv.org/abs/1504.03291\"},\n\"HyperlinkURL\"]\).
CFFModel[n] gives the \!\(\*SuperscriptBox[\(n\), \(th\)]\) benchmark for the CFF model.
CFFModel[n,\\[OpenCurlyDoubleQuote]Transition\\[CloseCurlyDoubleQuote]] gives the Transition corresponding to the \!\(\*SuperscriptBox[\(n\), \(th\)]\) benchmark.
CFFModel[n,\\[OpenCurlyDoubleQuote]Benchmark\\[CloseCurlyDoubleQuote]] gives the \!\(\*SuperscriptBox[\(n\), \(th\)]\) benchmark parameters.
";
CFFModel[]:=Function[{Global`\[Phi],Global`T},"\[Gamma]"/2(Global`T^2-"T0"^2)Global`\[Phi]^2 - "A"/3 Global`T Global`\[Phi]^3 + "\[Lambda]"/4 Global`\[Phi]^4]
CFFBenchmarks=With[{values={
		{140,1/18,Sqrt[10]/72,10/648,34.25,"Benchmark 1 from Hindmarsh et al., 1504.0329",.9,None},
		{140,2/9,Sqrt[10]/9,160/648,34.25,"Benchmark 2 from Hindmarsh et al., 1504.0329",.9,None},
		{140,1/9,Sqrt[10]/144,1.25/648,34.25,"Strong transition",.9,None},(* MM benchmark *)
		{100/Sqrt[2],1/18,Sqrt[10]/72,10/648,106.75,"Benchmark 1 from Krajewski et al., 2303.18216",.9,None},(* from 2303.18216 *)
		{100/Sqrt[2],1/9,Sqrt[10]/72,5/648,106.75,"Benchmark 2 from Krajewski et al., 2303.18216",.9,None},
		{140,1/9,Sqrt[10]/144,1.25/648,34.25,"Strong transition, with contribution from bubble wall collisions",.9,<|"GaugeCouplings"->{0.3},"MassFunctions"->{"Bosons"->{Function[{Global`\[Phi],Global`T},Sqrt[0.1111 (-19600.`+Global`T^2)-0.04392 Global`T Global`\[Phi]+0.005787 Global`\[Phi]^2]]}},"T0"->"Tn","T"->"Tp"|>}
		}},
	Dataset[AssociationThread[{"T0","\[Gamma]","A","\[Lambda]","RelativisticDOF","Description","WallVelocity","CollisionData"}->#]&/@values]
	];
CFFModel[i_Integer]:=Function[{Global`\[Phi],Global`T},CFFModel[][Global`\[Phi],Global`T]/.
	(CFFBenchmarks[i]//Normal//N)//Evaluate
	]
CFFModel[i_,"Benchmark"]:=CFFBenchmarks[i]


CFFTransitionsCompressed="1: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";
CFFTransitions=Uncompress[CFFTransitionsCompressed];
CFFModel[i_,"Transition"]:=CFFTransitions[[i]]


AutoCompleteModel[CFFModel]


(* ::Subsubsection:: *)
(*Analytic action*)


ActionFromPolynomialCoefficients::usage="ActionFromPolynomialCoefficients[c] gives the bounce action for the transition in a polynomial potential of order 4, as a function of its coefficients c[2],c[3],c[4].";
ActionPolynomial::usage="ActionPolynomial[V] gives the bounce action for the transition in a polynomial potential of order 4, extracting its coefficients \!\(\*SubscriptBox[\(c\), \(2\)]\),\!\(\*SubscriptBox[\(c\), \(3\)]\) and \!\(\*SubscriptBox[\(c\), \(4\)]\).";
ActionFromPolynomialCoefficients[c_]:=Function[Global`T,(16 Sqrt[2] \[Pi] c[2][T]^(3/2) (19/2-(34 c[2][T] c[4][T])/c[3][T]^2+1/16 (494-36 \[Pi]^2) (1-(4 c[2][T] c[4][T])/c[3][T]^2)^2))/(81 T (c[3][T]-(4 c[2][T] c[4][T])/c[3][T])^2)/.T->Global`T//Evaluate]
ActionPolynomial[V_]:=Module[{coeffRules,badCoeffs,\[Phi],T,Global`c},
	coeffRules=Reverse[CoefficientRules[V[\[Phi],T],\[Phi]]]/.{x_Integer}:>x;
	If[(badCoeffs=Select[coeffRules,FreeQ[{2,3,4},#[[1]]]&])=!={},
		Print[msg["badPolynomial"],badCoeffs/.(l_->r_):>(\[Phi]^l->r)/.{\[Phi]->"\[Phi]",T->"T"}//TableForm];
		Return[]
		];
	Global`c[n_]:=Function[temp,n/.coeffRules/.T->temp];
	ActionFromPolynomialCoefficients[Global`c]
	]


(* ::Subsubsection:: *)
(*Analytic nucleation*)


CriticalCFF[a_,T0_,T1_]:=(4 T1 a[3]^2+Sqrt[64 (-T0^2+T1^2) a[2] a[3]^2 a[4]+256 T0^2 a[2]^2 a[4]^2])/(-4 a[3]^2+16 a[2] a[4]);
NucleationExpansionCFF["T0"][a_,T0_,T1_,k_]:=T0+(9 k^(2/3) (3/(323 \[Pi]-18 \[Pi]^3))^(2/3) ((T0+T1)^4 a[2]^6 a[3]^4)^(1/3))/(4 T0^(1/3) a[2]^3);
NucleationExpansionCFF["Tc"][a_,T0_,T1_,Tc_,k_]:=Evaluate@Uncompress["1:eJztWT1s00AUdmwnTRBDVZYipkrsKAsSAxsSE1JUdWVIo0Y6kRKJgBBbRhYmRiQGViRGZhY2GLqzs9GpqoRoQmwnTu+9z/eXc5GRF7eW79537917370vt3c43h/GQRBMmovHgTg+mgzD1Wtv/PLoWfaaDOmNXkwmydtBN/szELP5fE7HN5LX5NEX0eIJx0SXbRJ8sb34z2YRIRhPFxHnY3LUpsrrS0bbi8d+/7kYP+2PRGIy/ZaC9wTxJJI96TBU8VWKRjYs5g4szSPflt5nn0joQg54b8YBb6sB5fiSycT8K3vz0B/tNuo2hE51T2qDhYi5QeJr7MSynQKv5CwVr8Pl/CdsdwwdIysYtmSAab4gbARviSYIFLOMQowlm8qcbXjK2W7qIil6gvXwN8caOWGtK3NAy54HVXwBPt5S4y6BjMoTufrJGxVE6sSqqWBjKkgjTMuSZFXQ0Dtv4hg/lCjpACvGdd+UDCsTtAPO3h2HBO3CcIEi9AcYrTzmxX+d474BuB0nXEWRhiAGbb6W9/ZrURMR4FzU93z2FIMBas9qNvLKRmmE4QrkUx0IhQI06nKLJQjhIFf6sfVS1gtKurrhrU1BpQpK5nG5UuFdLRUqU5FpvRhIBUdVgAqelOdWUbqWWqNRXilX3VLopcT9C4511wlrXbmsmwC08K0UKUFO8B2Oe84Lp9YOFeGGRDvoj+bQwD/jGPDM/Uccsg54LUsk3FqW1LKk8kRHUusPcIxsL6ORgqXDfmhtVfbMojNWuXhNHfN0X9rqMem+XHCxRnZiSqNgkVwWoumjN25DRAJI7QwAgh5EIZqiosk5xinAaDk4ZXZXllpuy2jBdtP+6FTVnZ6ptgLD5gBdMFaazYznJ19okzPl8wzPgoILH39HjIMKJCQC2Nbxgkh/Up9461b00u4cSKyrkHZvgY/epR3A/emtC9pU7hHd8mE1VeYts9KoNOkU9yFU2s3sa9B+h0zc2UQaKveqjMun79I6s1D+cPiFw1jl+QNUqLxdjnsH4Lr8lOND5T0Ca/mlXou9ygOxv+kpBpurPFK8DxqavsyWpf5TzlOM/Qt74kzx"];
NucleationExpansionCFF["\[Epsilon]"][c_,T_]:=1-4c[4][T]c[2][T]/(c[3][T]^2)


NucleationFromCoefficientsCFF::usage="NucleationFromCoefficientsCFF[c,k] gives the estimate for the nucleation temperature given the CFF coefficients c[n][T] and target action value \!\(\*FractionBox[SubscriptBox[\(S\), \(3\)], \(T\)]\)\[Congruent]k.";
Options[NucleationFromCoefficientsCFF]={"Expansion"->Automatic};
NucleationFromCoefficientsCFF[c_,k_,OptionsPattern[]]:=Module[{a,Tc,\[Epsilon],acc,T0,T1,Tns,badCoeffs={},expansion=OptionValue["Expansion"]},
	a[2]=c[2][T]/.{x_(T^2+T0sq_):>(T0=Abs[T0sq]^(1/2);x),x_ T^2 + y_:>(T0=Abs[y/x]^(1/2);x),x_ T^2:>(T0=0;x),x_:>(AppendTo[badCoeffs,{2->x}];x)};
	a[3]=c[3][T]/.{x_(T+\[CapitalTau]1_):>(T1=\[CapitalTau]1;x),x_ T + y_:>(T1=y/x;x),x_ T:>(T1=0;x),x_:>(AppendTo[badCoeffs,{3->x}];x)};
	a[4]=c[4][T]/.{x_/;MemberQ[Variables[x],T]:>(AppendTo[badCoeffs,{4->x}];x)};
	If[badCoeffs=!={},Print[msg["badPolynomialTn"],badCoeffs/.(l_->r_):>(\[Phi]^l->r)/.{\[Phi]->"\[Phi]",T->"T"}//TableForm]];
	
	(* compute nucleation temperature(s) *)
	Tc=CriticalCFF[a,T0,T1];
	\[Epsilon]=NucleationExpansionCFF["\[Epsilon]"][c,T];
	Tns["T0"]=NucleationExpansionCFF["T0"][a,T0,T1,k];
	Tns["Tc"]=NucleationExpansionCFF["Tc"][a,T0,T1,Tc,k];
	expansion/.{
		"Inflection":>Tns["T0"],
		"Critical":>Tns["Tc"],
		Automatic:>If[AllTrue[Tns/@{"T0","Tc"},NumericQ],
			acc={\[Epsilon]/.T->Tns["T0"],1-\[Epsilon]/.T->Tns["Tc"]}//
				Echo[#,"Expansion accuracies:",Column[Row/@{{"\!\(\*SubscriptBox[\(T\), \(0\)]\) \[Rule] ",PercentForm@#[[1]]},{"\!\(\*SubscriptBox[\(T\), \(c\)]\) \[Rule] ",PercentForm@#[[2]]}}]&]&;
			Tns[If[acc[[1]]>acc[[2]],
				Print["Selecting expansion around T0"];"T0",
				Print["Selecting expansion around Tc"];"Tc"]],
			<|"ExpansionAroundT0"->Tns["T0"],"ExpansionAroundTc"->Tns["Tc"]|>
			],
		x_:>(Print@msg["badNucleationOption"][expansion];None)
		}
	]
AutoComplete[NucleationFromCoefficientsCFF];


NucleationCFF::usage="NucleationCFF[V,k] gives the estimate for the nucleation temperature given the CFF potential V and target action value \!\(\*FractionBox[SubscriptBox[\(S\), \(3\)], \(T\)]\)\[Congruent]k.";
NucleationCFF[V_,k_,opt:OptionsPattern[]]:=Module[{coeffRules,badCoeffs,\[Phi],T,c},
	coeffRules=Reverse[CoefficientRules[V[\[Phi],T],\[Phi]]]/.{x_Integer}:>x//Simplify;
	If[(badCoeffs=Select[coeffRules,FreeQ[{2,3,4},#[[1]]]&])=!={},
		Print[msg["badPolynomialTn"],badCoeffs/.(l_->r_):>(\[Phi]^l->r)/.{\[Phi]->"\[Phi]",T->"T"}//TableForm];
		Return[]
		];
	c[n_]:=Function[temp,n/.coeffRules/.T->temp];
	NucleationFromCoefficientsCFF[c,k,opt]
	]
AutoComplete[NucleationCFF,{NucleationFromCoefficientsCFF}];


(* ::Subsection::Closed:: *)
(*Abelian Higgs Model*)


AHModel::usage="AHModel[n,\\[OpenCurlyDoubleQuote]Transition\\[CloseCurlyDoubleQuote]] gives the \!\(\*SuperscriptBox[\(n\), \(th\)]\) transition of the abelian-Higgs (aka, the dark photon) model, computed in the dimensional reduction approach."
AHTansitions={
(* GeV transitions *)
PT2GW`Transition[<|"Phases" -> {InterpolatingFunction[{{9.6, 32.}}, {5, 6, 0, {29}, {4}, 0, 0, 0, 0, Indeterminate& , {}, {}, False}, {{9.6, 10.4, 11.2, 12., 12.8, 13.6, 14.4, 15.2, 16., 16.799999999999997`, 17.6, 18.4, 19.2, 20., 20.8, 21.6, 22.4, 23.2, 24., 24.799999999999997`, 25.599999999999998`, 26.4, 27.200000000000003`, 28., 28.8, 29.6, 30.4, 31.2, 32.}}, {Developer`PackedArrayForm, {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29}, {0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.}}, {Automatic}], InterpolatingFunction[{{8., 21.6}}, {5, 6, 0, {18}, {4}, 0, 0, 0, 0, Indeterminate& , {}, {}, False}, {{8., 8.8, 9.6, 10.4, 11.2, 12., 12.8, 13.6, 14.4, 15.2, 16., 16.799999999999997`, 17.6, 18.4, 19.2, 20., 20.8, 21.6}}, {Developer`PackedArrayForm, {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18}, {813.532120575199, 363.08205730295913`, 254.86214546079745`, 205.95990233859843`, 177.92330561513555`, 159.60220137345274`, 146.5587389228224, 136.66919727569913`, 128.78248506792173`, 122.21291523784836`, 116.51752422620189`, 111.38563328654487`, 106.5763889246206, 101.87498173012905`, 97.0468354973561, 91.7517069806359, 85.25086738628286, 72.48632899214518}}, {Automatic}]}, "WallVelocity" -> 0.95, "Tc" -> 19.917279056001824`, "TnEstimate" -> 11.541731565501804`, "ActionFunction" -> PT2GW`ActionFunction[<|"Function" -> Function[PT2GW`Private`T$, Piecewise[{{-1246.6191379621428` + 1166.8894189034377` (19.917279056001824` - PT2GW`Private`T$)^(-2) + 5979.657141087893/(19.917279056001824` - PT2GW`Private`T$) + 77.75826914258772 (19.917279056001824` - PT2GW`Private`T$), Inequality[18.24216955790182, LessEqual, PT2GW`Private`T$, Less, 19.917279056001824`]}, {InterpolatingFunction[{{10.993899944339669`, 18.24216955790182}}, {5, 39, 0, {27}, {4}, 0, 0, 0, 0, Automatic, {}, {}, False}, {{10.993899944339669`, 11.27267954486129, 11.55145914538291, 11.830238745904532`, 12.109018346426154`, 12.387797946947774`, 12.666577547469396`, 12.945357147991018`, 13.224136748512638`, 13.502916349034262`, 13.781695949555882`, 14.060475550077502`, 14.339255150599122`, 14.618034751120746`, 14.896814351642366`, 15.175593952163988`, 15.45437355268561, 15.73315315320723, 16.01193275372885, 16.290712354250473`, 16.569491954772094`, 16.848271555293717`, 17.127051155815337`, 17.405830756336957`, 17.684610356858578`, 17.9633899573802, 18.24216955790182}}, {BSplineFunction[1, {{10.993899944339669`, 18.24216955790182}}, {3}, {False}, {{114.48770517838321`, 133.03826562684182`, 149.59178409966384`, 185.64671273034503`, 207.54257537479484`, 235.22099837451458`, 264.2450531912022, 290.00528074361256`, 324.464528679114, 348.97575146806565`, 385.27222968759173`, 425.0493315788355, 463.376410415137, 507.3567714520862, 562.347549725569, 617.0986063327573, 678.3198107903937, 750.2386403440144, 830.9400553303498, 926.3163952834118, 1039.2981597084759`, 1178.3695190971696`, 1349.0367104342029`, 1567.1613985067604`, 1951.3041456110077`, 2395.2649844544003`, 2869.134651907842}, {}}, {{10.993899944339669`, 10.993899944339669`, 10.993899944339669`, 10.993899944339669`, 11.55145914538291, 11.830238745904532`, 12.109018346426154`, 12.387797946947774`, 12.666577547469394`, 12.945357147991015`, 13.224136748512638`, 13.50291634903426, 13.78169594955588, 14.0604755500775, 14.339255150599122`, 14.618034751120744`, 14.896814351642366`, 15.175593952163986`, 15.454373552685606`, 15.73315315320723, 16.01193275372885, 16.290712354250473`, 16.569491954772094`, 16.848271555293714`, 17.127051155815337`, 17.405830756336957`, 17.684610356858578`, 18.24216955790182, 18.24216955790182, 18.24216955790182, 18.24216955790182}}, {0}, MachinePrecision, "Unevaluated"], {}}, {Automatic}][PT2GW`Private`T$], Inequality[10.993899944339669`, LessEqual, PT2GW`Private`T$, Less, 18.24216955790182]}}, Indeterminate]], "ActionMethod" -> "PWLaurent", "Domain" -> {10.993899944339669`, 19.917279056001824`}, "Tc" -> 19.917279056001824`, "StandardDeviation" -> 0.012959914404693728`, "NPoints" -> 27, "Unit" -> "GeV", "Data" -> CompressedData["
1:eJwBwQE+/iFib1JlAgAAABsAAAACAAAAbUb70v49MkANSRjxRGqmQCg+aLmg
9jFAzer521jMoUDiNdWfQq8xQK5cMplXQp1AnS1ChuRnMUCVHybuTKuYQFgl
r2yGIDFAtjwXAckzlUATHRxTKNkwQBQJLb6KfpJAzRSJOcqRMEBXXZPwlU6Q
QIgM9h9sSjBA2PC2SwEKjUBCBGMGDgMwQAs8PUYWC4pA+vef2V93L0CCPUuG
nn2HQHDneaaj6C5AYT20bdJAhUDl1lNz51kuQJdVQ2xqUYNAWsYtQCvLLUDt
ofP0dZKBQNC1Bw1vPC1AAAncxxHTf0BEpeHZsq0sQBqrmRUZBX1AupS7pvYe
LEBciAMu7Ix6QDCElXM6kCtA4WeXKKMdeECmc29AfgErQL3ePBQK73VAGmNJ
DcJyKkCstziG5yx0QJBSI9oF5ClAfbNCKUg3ckAFQv2mSVUpQEcgvaE3e3BA
ejHXc43GKEC+rJenP25tQPAgsUDRNyhAqDte5jMQakBlEIsNFaknQA7UX8ZA
GWdA2v9k2lgaJ0AzuLqBqvhjQFDvPqeciyZAWyTsbmFGYUDF3hh04PwlQLrP
x482n1xABa25nw==
"]|>], "Tn" -> 11.45625020918956, "NucleationAction" -> 151.42533909974534`, "NucleationGammaOverH4" -> 1698.2472357407905`, "NucleationIntegralGammaOverH4" -> 0.9999288904515917, "Tp" -> 11.248863584553376`, "PercolationAction" -> 136.38640569662294`, "PercolationIntegralValue" -> 0.3399999999999992, "\[Alpha]" -> 0.9075071221089192, "\[Kappa]" -> 0.5997509583853278, "K" -> 0.17120091241114324`, "Kstr" -> 0.47575556158636473`, "\[Beta]/H" -> 860.9040953643802, "HR" -> 0.003232328201655374, "TpCondition" -> -5.5299419742387625`*^-14, "fPeak" -> 0.0006776991150915228, "h2OmegaPeak" -> 9.262586779054056*^-13, "Domain" -> {9.600001941731563, 19.917279056001824`}, "Unit" -> "GeV"|>],
PT2GW`Transition[<|"Phases" -> {InterpolatingFunction[{{230., 400.}}, {5, 6, 0, {18}, {4}, 0, 0, 0, 0, Indeterminate& , {}, {}, False}, {{230., 240., 250., 260., 270., 280., 290., 299.99999999999994`, 310., 320., 329.99999999999994`, 340., 350.00000000000006`, 360., 370., 380., 390., 400.}}, {Developer`PackedArrayForm, {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18}, {0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.}}, {Automatic}], InterpolatingFunction[{{100., 299.99999999999994`}}, {5, 6, 0, {21}, {4}, 0, 0, 0, 0, Indeterminate& , {}, {}, False}, {{100., 110., 120., 130., 140., 150., 160., 170., 180., 190., 200., 210., 220., 230., 240., 250., 260., 270., 280., 290., 299.99999999999994`}}, {Developer`PackedArrayForm, {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21}, {1394.62655282722, 1319.2175009068962`, 1263.2134311004463`, 1219.3085443021466`, 1183.3369439647338`, 1152.7228062808244`, 1125.7674483954647`, 1101.286192653041, 1078.407267493419, 1056.4543961445274`, 1034.8711016890616`, 1013.1682171443432`, 990.8822940944505, 967.5366903823127, 942.5949690927557, 915.3969212295766, 885.0460639776508, 850.1854280736567, 808.4551908619219, 754.7632734016602, 671.3474718405851}}, {Automatic}]}, "WallVelocity" -> 0.95, "Tc" -> 294.09775932102826`, "TnEstimate" -> 261.5500720241963, "ActionFunction" -> PT2GW`ActionFunction[<|"Function" -> Function[PT2GW`Private`T$, Piecewise[{{-1720.3302233220513` + 2484.932389339779 (294.09775932102826` - PT2GW`Private`T$)^(-2) + 25202.195313292177`/(294.09775932102826` - PT2GW`Private`T$) + 24.136326171562235` (294.09775932102826` - PT2GW`Private`T$), Inequality[287.58822186166185`, LessEqual, PT2GW`Private`T$, Less, 294.09775932102826`]}, {InterpolatingFunction[{{240.55156409075633`, 287.58822186166185`}}, {5, 39, 0, {29}, {4}, 0, 0, 0, 0, Automatic, {}, {}, False}, {{240.55156409075633`, 242.23144472543152`, 243.91132536010673`, 245.59120599478192`, 247.2710866294571, 248.95096726413232`, 250.6308478988075, 252.3107285334827, 253.9906091681579, 255.6704898028331, 257.3503704375083, 259.0302510721835, 260.7101317068587, 262.3900123415339, 264.0698929762091, 265.7497736108843, 267.4296542455595, 269.10953488023466`, 270.78941551490993`, 272.4692961495851, 274.14917678426025`, 275.82905741893546`, 277.5089380536107, 279.1888186882859, 280.86869932296105`, 282.5485799576362, 284.2284605923115, 285.90834122698664`, 287.58822186166185`}}, {BSplineFunction[1, {{240.55156409075633`, 287.58822186166185`}}, {3}, {False}, {{22.303296390842853`, 25.838958554723064`, 32.10370433950772, 40.89249629613657, 47.06544155918582, 56.277058498373094`, 64.97496764119516, 74.69170268080185, 83.84845212887798, 96.17565223701885, 108.75152089163002`, 122.49355661638067`, 137.75128207878623`, 156.83444750129533`, 176.37588500404422`, 198.38280416437945`, 224.14185942894355`, 253.2329518176644, 287.23720523432974`, 328.03626530957655`, 376.40782277075533`, 435.89386814207506`, 510.20477567475217`, 606.4007122861825, 733.9175867476542, 909.228003994789, 1258.050908976241, 1707.7832146091876`, 2367.008886837991}, {}}, {{240.55156409075633`, 240.55156409075633`, 240.55156409075633`, 240.55156409075633`, 243.9113253601067, 245.5912059947819, 247.2710866294571, 248.9509672641323, 250.63084789880747`, 252.3107285334827, 253.9906091681579, 255.6704898028331, 257.3503704375082, 259.03025107218343`, 260.71013170685865`, 262.39001234153386`, 264.0698929762091, 265.7497736108843, 267.4296542455595, 269.10953488023466`, 270.7894155149099, 272.46929614958503`, 274.14917678426025`, 275.82905741893546`, 277.5089380536107, 279.18881868828583`, 280.86869932296105`, 282.5485799576362, 284.2284605923114, 287.58822186166185`, 287.58822186166185`, 287.58822186166185`, 287.58822186166185`}}, {0}, MachinePrecision, "Unevaluated"], {}}, {Automatic}][PT2GW`Private`T$], Inequality[240.55156409075633`, LessEqual, PT2GW`Private`T$, Less, 287.58822186166185`]}}, Indeterminate]], "ActionMethod" -> "PWLaurent", "Domain" -> {240.55156409075633`, 294.09775932102826`}, "Tc" -> 294.09775932102826`, "StandardDeviation" -> 5.856313508256537*^-14, "NPoints" -> 29, "Unit" -> "GeV", "Data" -> CompressedData["
1:eJwB4QEe/iFib1JlAgAAAB0AAAACAAAAFKpTW2n5cUASzdCMBH6iQEJ4z5CI
3nFAO4tNxv40mUBxRkvGp8NxQEvcyMn8npJAnhTH+8aocUBcK6dT5tyMQM3i
QjHmjXFAmM42vxAvh0D8sL5mBXNxQMwk9In3HINAKn86nCRYcUD8LHN40Q6A
QFhNttFDPXFABA8gudVle0CGGzIHYyJxQJMpt+4ppHdAtemtPIIHcUAqDsUI
xpR0QOS3KXKh7HBA9TY/LuoFckARhqWnwNFwQNbHnXGowW9AQFQh3d+2cEDW
7yt0TxZsQG4inRL/m3BA/jck2ULgaECc8BhIHoFwQCtESXQtGWZAy76UfT1m
cEC1pfJ8JZ1jQPmMELNcS3BA0yvkgHFMYUAnW4zoezBwQAtxaUbBr15AVSkI
HpsVcEDTHfNUiTxbQAfvB6d09W9AlCTL6uQNWEBki/8Rs79vQCk2znseGFVA
wCf3fPGJb0CVEgW5S6ZSQB3E7ucvVG9A94WZ8UNJUEB6YOZSbh5vQIq0lB+B
GExA1vzdvazobkCbRoKWM8lHQDOZ1Sjrsm5AJQVC8E5pRECQNc2TKX1uQFyk
RPwpK0FA7NHE/mdHbkBSZasvhQY8QEluvGmmEW5ANqoP1aRNNkC6JNSi
"]|>], "Tn" -> 260.76130676498695`, "NucleationAction" -> 138.89397497443164`, "NucleationGammaOverH4" -> 2743.113627608072, "NucleationIntegralGammaOverH4" -> 0.9999459887499216, "Tp" -> 258.7065668182044, "PercolationAction" -> 119.9800122357764, "PercolationIntegralValue" -> 0.34000000000001596`, "\[Alpha]" -> 0.007558887710189151, "\[Kappa]" -> 0.011557135424555507`, "K" -> 0.00005202222321170689, "Kstr" -> 0.007502179577183547, "\[Beta]/H" -> 2189.0473763957025`, "HR" -> 0.0012712034542389345`, "TpCondition" -> -7.131919993682343*^-11, "fPeak" -> 0.03378603110303998, "h2OmegaPeak" -> 7.588496978407011*^-19, "Domain" -> {235.51192218673074`, 294.09775932102826`}, "Unit" -> "GeV"|>],
(* keV transitions *)
PT2GW`Transition[<|"Phases" -> {InterpolatingFunction[{{9.6, 38.4}}, {5, 6, 0, {31}, {4}, 0, 0, 0, 0, Indeterminate& , {}, {}, False}, {{9.6, 10.559999999999999`, 11.52, 12.48, 13.440000000000001`, 14.399999999999999`, 15.36, 16.32, 17.28, 18.24, 19.2, 20.159999999999997`, 21.119999999999997`, 22.08, 23.04, 24., 24.96, 25.919999999999998`, 26.88, 27.839999999999996`, 28.799999999999997`, 29.759999999999998`, 30.72, 31.68, 32.64, 33.6, 34.56, 35.52, 36.48, 37.44, 38.4}}, {Developer`PackedArrayForm, {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31}, {0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0., 0.}}, {Automatic}], InterpolatingFunction[{{9.6, 22.08}}, {5, 6, 0, {14}, {4}, 0, 0, 0, 0, Indeterminate& , {}, {}, False}, {{9.6, 10.559999999999999`, 11.52, 12.48, 13.440000000000001`, 14.399999999999999`, 15.36, 16.32, 17.28, 18.24, 19.2, 20.159999999999997`, 21.119999999999997`, 22.08}}, {Developer`PackedArrayForm, {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14}, {334.5910860993953, 245.19485210802824`, 202.52621730834363`, 177.29052655308791`, 160.3919207945771, 148.06675881065902`, 138.4602846782542, 130.5353089390367, 123.64433998424693`, 117.33030619548074`, 111.21013782828612`, 104.86432180731964`, 97.60493475201368, 87.13781093279212}}, {Automatic}]}, "WallVelocity" -> 0.95, "Tc" -> 20.681696511896888`, "TnEstimate" -> 12.336267287529227`, "ActionFunction" -> PT2GW`ActionFunction[<|"Function" -> Function[PT2GW`Private`T$, Piecewise[{{-1921.1106499114996` + 236.22826125374925` (20.681696511896888` - PT2GW`Private`T$)^(-2) + 8502.415006339817/(20.681696511896888` - PT2GW`Private`T$) + 124.79392842889078` (20.681696511896888` - PT2GW`Private`T$), Inequality[19.012610667023356`, LessEqual, PT2GW`Private`T$, Less, 20.681696511896888`]}, {InterpolatingFunction[{{11.421797819639428`, 19.012610667023356`}}, {5, 39, 0, {26}, {4}, 0, 0, 0, 0, Automatic, {}, {}, False}, {{11.421797819639428`, 11.725430333534785`, 12.029062847430142`, 12.332695361325499`, 12.636327875220857`, 12.939960389116214`, 13.24359290301157, 13.54722541690693, 13.850857930802285`, 14.154490444697643`, 14.458122958592998`, 14.761755472488357`, 15.065387986383715`, 15.36902050027907, 15.67265301417443, 15.976285528069786`, 16.279918041965143`, 16.583550555860498`, 16.887183069755856`, 17.19081558365121, 17.49444809754657, 17.798080611441925`, 18.101713125337284`, 18.405345639232642`, 18.708978153128, 19.012610667023356`}}, {BSplineFunction[1, {{11.421797819639428`, 19.012610667023356`}}, {3}, {False}, {{128.7134489339619, 151.01746695808515`, 171.50052121826593`, 215.0851600535815, 241.92000170899783`, 275.60208364038766`, 311.0724127461675, 342.80536490718913`, 385.3519260846471, 415.308411846946, 463.15256297135085`, 510.3040248700859, 559.1885735050703, 617.4004581771593, 678.9351190071195, 757.9121108808015, 839.3294259007338, 934.2510885113286, 1045.3355784914727`, 1180.6668732710089`, 1343.727466744123, 1548.9738577750866`, 1813.7691620172136`, 2287.297465602261, 2852.9409193370143`, 3466.0328394804833`}, {}}, {{11.421797819639428`, 11.421797819639428`, 11.421797819639428`, 11.421797819639428`, 12.02906284743014, 12.332695361325497`, 12.636327875220855`, 12.939960389116212`, 13.24359290301157, 13.547225416906928`, 13.850857930802285`, 14.154490444697641`, 14.458122958592998`, 14.761755472488355`, 15.065387986383712`, 15.36902050027907, 15.672653014174427`, 15.976285528069784`, 16.27991804196514, 16.583550555860498`, 16.887183069755853`, 17.19081558365121, 17.494448097546567`, 17.798080611441925`, 18.101713125337284`, 18.405345639232642`, 19.012610667023356`, 19.012610667023356`, 19.012610667023356`, 19.012610667023356`}}, {0}, MachinePrecision, "Unevaluated"], {}}, {Automatic}][PT2GW`Private`T$], Inequality[11.421797819639428`, LessEqual, PT2GW`Private`T$, Less, 19.012610667023356`]}}, Indeterminate]], "ActionMethod" -> "PWLaurent", "Domain" -> {11.421797819639428`, 20.681696511896888`}, "Tc" -> 20.681696511896888`, "StandardDeviation" -> 0.00018243248471649495`, "NPoints" -> 26, "Unit" -> "keV", "Data" -> CompressedData["
1:eJwBsQFO/iFib1JlAgAAABoAAAACAAAAMXLiczoDM0BjHVbQEBSrQGRDnZd/
tTJA7Ti8gSwjpUCWFFi7xGcyQP1eAaKbIKFAyOUS3wkaMkA7hEmDT5OcQPq2
zQJPzDFA/6ewPphbmEAtiIgmlH4xQIhNqaIIG5VAX1lDStkwMUDddF9YJ4WS
QJIq/m0e4zBAaEz0wIFlkEDE+7iRY5UwQEVzGSSPR41A98xztahHMEDT5fEl
pEyKQFM8XbLb8y9AY9F694yyh0C43tL5ZVgvQPgp0cW8ToVAHIFIQfC8LkDW
HwdQok+DQIIjvoh6IS5AQpzi7fGFgUDmxTPQBIYtQD+xCmh86X9ASmipF4/q
LEBnbKcFmPB8QLAKH18ZTyxAayVckaIkekAUrZSmo7MrQPPRF6oO9HdAek8K
7i0YK0Af/3XduIl1QN7xfzW4fCpAKHhOODFnc0BDlPV8QuEpQDbXTOlmPnFA
qDZrxMxFKUCrKF1r9WFuQAzZ4AtXqihA0Af9doHDakBxe1ZT4Q4oQEBcVEIX
+GZA1h3MmmtzJ0B/nVkXJq5jQDvAQeL11yZAcNfbktQWYEBuHLtO
"]|>], "Tn" -> 12.264429692820357`, "NucleationAction" -> 207.18032456894844`, "NucleationGammaOverH4" -> 1379.770653402662, "NucleationIntegralGammaOverH4" -> 1.0000305939106244`, "Tp" -> 12.098298103656667`, "PercolationAction" -> 190.46275737379304`, "PercolationIntegralValue" -> 0.3400000000000329, "\[Alpha]" -> 0.7028995403110981, "\[Kappa]" -> 0.5321169725172801, "K" -> 0.13178397193264843`, "Kstr" -> 0.41276629869938625`, "\[Beta]/H" -> 1199.8067011328978`, "HR" -> 0.0023193107554236457`, "TpCondition" -> -8.316113129287067*^-20, "fPeak" -> 9.87392410109701*^-10, "h2OmegaPeak" -> 3.2207279929362*^-13, "Domain" -> {9.600002736267285, 20.681696511896888`}, "Unit" -> "keV"|>]
};
AHTransition[i_,"Transition"]:=AHTransitions[i]


AutoCompleteModel[AHModel]


(* ::Section:: *)
(*End package*)


End[]; (* end "Private`" *) 


(* ReadProtected attribute on public symbols prevents rendering of huge box with all 
definitions (DownValues) when they are called in Information or with shortcut ?FindBounce. *)
SetAttributes[Evaluate@Names["`*"],{ReadProtected}]; (* from FindBounce.m *) 


EndPackage[];


Print["\[Checkmark] Imported \!\(\*StyleBox[\"Examples`\",\nFontWeight->\"Bold\"]\)"]
