(* ::Package:: *)

(* ::Title:: *)
(*Export DRalgo*)


(* ::Section:: *)
(*Begin package*)


BeginPackage["DRTools`"]


(* ::Subsection:: *)
(*Public Symbols*)


(* utilities *)
SubRulesAppend;
(* store expressions *)
StoreDRExpressions;
LoadDRExpressions;
DRExpressions;
(* RG evolution *)
BetaToEquations;
RGSolve;
RGSolutions;
(* dimensional reduction *)
DRStep;
DRScalarSquareMasses;
DRVectorSquareMasses;
HighTRatio;
(* effective potential *)
DefineDRPotential;
DRPotentialN;
ComputeDRPotential;
(* plot *)
PlotRG;
PlotDR;
PlotHighTRatio;


(* ::Section:: *)
(*Code*)


Begin["Private`"]


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


(* ::Subsubsection:: *)
(*Messages*)


msg["DRstored"]=StringTemplate["DR expressions saved to ``."];
msg["load"]=StringTemplate["Failed to load \!\(\*
StyleBox[\"``\",\nStripOnInput->False,\nLineColor->RGBColor[1, 0, 0],\nFrontFaceColor->RGBColor[1, 0, 0],\nBackFaceColor->RGBColor[1, 0, 0],\nGraphicsColor->RGBColor[1, 0, 0],\nFontColor->RGBColor[1, 0, 0]]\). Please provide a valid DR file."];
msg["loadAssociation"]=StringTemplate["Content of \!\(\*
StyleBox[\"``\",\nStripOnInput->False,\nLineColor->RGBColor[1, 0, 0],\nFrontFaceColor->RGBColor[1, 0, 0],\nBackFaceColor->RGBColor[1, 0, 0],\nGraphicsColor->RGBColor[1, 0, 0],\nFontColor->RGBColor[1, 0, 0]]\) is not a valid Association."];
msg["missingDR"]=StringTemplate["Missing elements \!\(\*
StyleBox[\"``\",\nStripOnInput->False,\nLineColor->RGBColor[1, 0, 0],\nFrontFaceColor->RGBColor[1, 0, 0],\nBackFaceColor->RGBColor[1, 0, 0],\nGraphicsColor->RGBColor[1, 0, 0],\nFontColor->RGBColor[1, 0, 0]]\) in file ``."];


msg["computeVeff"]="Make sure DRExpressions is either
	- created (\"SubRules\"\[Rule]subRules must be provided)
	- loaded from file (\"LoadDRFrom\"\[Rule]file.m)
	- or already contains all DR quantities and the effective potential."


(* ::Subsection:: *)
(*Substitution rules*)


SubRulesAppend::usage="SubRulesAppend[list] appends the substitution rules for \"3d\" and \"3dUS\" variables to a list of substitution rules.
SubRulesAppend[list,\"Suffix\"->{suffix3d,suffixUS}] defines substitution rules with custom suffixes.
";
Options[SubRulesAppend]={"Inplace"->True,"Suffix"->{"3d","3dUS"}};
AutoComplete[SubRulesAppend];
SetAttributes[SubRulesAppend,HoldFirst];
SubRulesAppend[subRules_,OptionsPattern[]]:=Module[{sub3dRules,subUSRules,subRes,suff=OptionValue["Suffix"]},
	append3dRule = #->Symbol[ToString[#]<>suff[[1]]]&;
	append3dUSRule = #->Symbol[ToString[#]<>suff[[2]]]&;
	sub3dRules=append3dRule/@(subRules[[All,1]]~Join~Variables[subRules[[All,2]]]);
	subUSRules=append3dUSRule/@(subRules[[All,1]]~Join~Variables[subRules[[All,2]]]);
	subRes=Append[subRules,{subRules/.sub3dRules,subRules/.subUSRules}]//Flatten;
	If[OptionValue["Inplace"],Check[subRules=subRes,subRes]//Quiet,subRes]
	]


(* ::Subsection:: *)
(*Store expressions*)


(* ::Subsubsection:: *)
(*Save expressions*)


(* ::Text:: *)
(*2do:*)
(*Add anomalous dimensions*)


sepString=StringJoin[Table["\[LongDash]",50]];


DRExpressions::usage="DRExpressions is a container for all relevant quantities in the dimensionally-reduced theory.
DRExpressions can be defined, stored (loaded) with StoreDRExpressions (LoadDRExpressions).
DRExpressions[key] returns the value associated with key, if defined.
";


StoreDRExpressions::usage="StoreDRExpressions[] stores \[Beta] functions, 3d soft and ultrasoft expressions, and the effective potential
computed by DRalgo in the symbol DRExpressions.";
Options[StoreDRExpressions]={"US"->True,"PrintDR"->False,"SubRules"->{},"File"->None};
AutoComplete[StoreDRExpressions];
StoreDRExpressions[OptionsPattern[]]:=Module[{
	subRules=OptionValue["SubRules"],file=OptionValue["File"],US=OptionValue["US"],USrules
	},
	(* initialize DRExpressions Association if not defined *)
	If[!MatchQ[DRExpressions,_Association],DRExpressions=<||>];
	(* 4d \[Beta]-functions *)
	DRExpressions["BetaFunctions"]=DRalgo`BetaFunctions4D[]/.subRules;
	(* 3d soft-scale expressions *)
	DRExpressions["BetaFunctions3d"]=DRalgo`BetaFunctions3DS[]/.subRules;
	DRExpressions["Constants3d"]=DRalgo`PrintConstants[]/.subRules;
	DRExpressions["Couplings3d"]=DRalgo`PrintCouplings[]/.subRules;
	DRExpressions["TempScalar3d"]=DRalgo`PrintTemporalScalarCouplings[]/.subRules;
	DRExpressions["DebyeMass3dLO"]=DRalgo`PrintDebyeMass["LO"]/.subRules;
	DRExpressions["DebyeMass3dNLO"]=DRalgo`PrintDebyeMass["NLO"]/.subRules;
	DRExpressions["ScalarMass3dLO"]=DRalgo`PrintScalarMass["LO"]/.subRules;
	DRExpressions["ScalarMass3dNLO"]=DRalgo`PrintScalarMass["NLO"]/.subRules;
	DRExpressions["Pressure3dLO"]=DRalgo`PrintPressure["LO"]/.subRules;
	DRExpressions["Pressure3dNLO"]=DRalgo`PrintPressure["NLO"]/.subRules;
	DRExpressions["Pressure3dNNLO"]=DRalgo`PrintPressure["NNLO"]/.subRules;
	(* parameter names *)
	DRExpressions["Parameters3d"]=DRExpressions["Couplings3d"][[All,1]]~Join~DRExpressions["ScalarMass3dLO"][[All,1]];
	(* printing (optional) *)
	If[OptionValue["PrintDR"],
		(* print 4d \[Beta]-functions *)
		Echo["","4d \[Beta]-functions"];
		Print[DRExpressions["BetaFunctions"]//TableForm];
		(* print 3d soft-scale expressions *)
		Print[sepString];
		Echo["","3d soft-scale quantities"];
		Echo["\[Beta]-functions"];
		Print[DRExpressions["BetaFunctions3d"]//TableForm];
		Echo["Constants"];
		Print[DRExpressions["Constants3d"]//TableForm];
		Echo["Couplings"];
		Print[DRExpressions["Couplings3d"]//TableForm];
		Echo["Temporal scalar couplings"];
		Print[DRExpressions["TempScalar3d"]//TableForm];
		Echo["Debye masses"];
		Print[DRExpressions["DebyeMass3dLO"]//TableForm];
		Print[DRExpressions["DebyeMass3dNLO"]//TableForm];
		Echo["Scalar masses"];
		Print[DRExpressions["ScalarMass3dLO"]//TableForm];
		Print[DRExpressions["ScalarMass3dNLO"]//TableForm];		
		Echo["Pressure"];
		Print[DRExpressions["Pressure3dLO"]//TableForm];
		Print[DRExpressions["Pressure3dNLO"]//TableForm];
	];
	(* 3d ultrasoft-scale expressions *)
	If[US,
		DRExpressions["BetaFunctionsUS"]=DRalgo`BetaFunctions3DUS[]/.subRules;
		DRExpressions["CouplingsUS"]=DRalgo`PrintCouplingsUS[]/.subRules;
		DRExpressions["ScalarMassUSLO"]=DRalgo`PrintScalarMassUS["LO"]/.subRules;
		DRExpressions["ScalarMassUSNLO"]=DRalgo`PrintScalarMassUS["NLO"]/.subRules;
		DRExpressions["PressureUSLO"]=DRalgo`PrintPressureUS["LO"]/.subRules;
		DRExpressions["PressureUSNLO"]=DRalgo`PrintPressureUS["NLO"]/.subRules;
		Check[DRExpressions["pressureUSNNLO"]=DRalgo`PrintPressureUS["NNLO"]/.subRules,Print["Skipping US pressure @ NNLO."]];
		DRExpressions["BetaFunctionsUS"]=DRalgo`BetaFunctions3DUS[]/.subRules;
		(* printint (optional) *)
		If[OptionValue["PrintDR"],
			Print[sepString];
			Echo["","3d ultrasoft-scale quantities"];
			Echo["\[Beta]-functions"];
			Print[DRExpressions["BetaFunctionsUS"]//TableForm];
			Echo["Couplings"];
			Print[DRExpressions["CouplingsUS"]//TableForm];
			Echo["Scalar masses"];
			Print[DRExpressions["ScalarMassUSLO"]//TableForm];
			Print[DRExpressions["ScalarMassUSNLO"]//TableForm];
			Echo["Pessure"];
			Print[DRExpressions["PressureUSLO"]//TableForm];
			Print[DRExpressions["PressureUSNLO"]//TableForm];
		];
	];
	(* effective potential *)
	USrules=If[US,append3dUSRule,append3dRule]/@(Symbol[StringDrop[ToString[#],-2]]&/@DRExpressions["Parameters3d"]);
	DRExpressions["VeffLO"]=DRalgo`PrintEffectivePotential["LO"]/.subRules/.USrules;
	DRExpressions["VeffNLO"]=DRalgo`PrintEffectivePotential["NLO"]/.subRules/.USrules;
	DRExpressions["VeffNNLO"]=DRalgo`PrintEffectivePotential["NNLO"]/.subRules/.USrules;
	DRExpressions["VEV"]=DRalgo`Private`\[Phi]Vev["ExplicitValues"];
	DRExpressions["SquareMassMatrixScalar"]=Normal[DRalgo`PrintTensorsVEV[1]]/.subRules/.USrules;
	DRExpressions["SquareMassMatrixVector"]=Normal[DRalgo`PrintTensorsVEV[2]]/.subRules/.USrules;
	(* print (optional) *)
	If[OptionValue["PrintDR"],
		Print[sepString];
		Echo["","Effective potential"];
		Print[DRExpressions["VeffLO"]];
		Print[VeffNLO];
		];
	If[file=!=None,
		Put[DRExpressions,file];
		Print@msg["DRstored"][file]
		];
	(* autocomplete Key arguments *)
	AutoComplete[DRExpressions];
]


(* ::Subsubsection:: *)
(*Load expressions*)


LoadDRExpressions::usage="LoadDRExpressions[file] loads DRalgo expression (stored with StoreDRExpressions[]) into the 'DRExpressions' association.";
DRrequirements={"BetaFunctions","Constants3d","Couplings3d","TempScalar3d","DebyeMass3dLO","ScalarMass3dLO"};
LoadDRExpressions[file_]:=Module[{missing},
	(* initialize DRExpressions Association if not defined *)
	If[!MatchQ[DRExpressions,_Association],DRExpressions=<||>];
	(* load *)
	DRExpressions=Check[Get[file],Print@msg["load"][file];Abort[]];
	Which[
		!MatchQ[DRExpressions,_Association],Print@msg["loadAssociation"][file];Return[DRExpressions],
		(missing=Complement[DRrequirements,Keys[DRExpressions]])!={},Print@msg["missingDR"][missing,file];Return[DRExpressions]
		];
	AutoComplete[DRExpressions];
	DRExpressions
	]


(* ::Subsection:: *)
(*RG evolution*)


BetaToEquations::usage="BetaToEquations[\[Beta]Rules,\[Mu]] turns a list of rules for \[Beta]-functions to a list of RG equations {\[Mu]\!\(\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\)g\[Equal]\[Beta][g]}.";
BetaToEquations[\[Beta]Rules_,\[Mu]_]:=Module[{vars,\[Beta]Functions},
	vars=\[Beta]Rules[[All,1]]; (* extract parameters *)
	\[Beta]Functions=\[Beta]Rules/.Thread[vars->Through[vars[\[Mu]]]]; (* make parameters into functions *)
	\[Mu] \!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Mu]\)]\(#[\([1]\)]\)\)==#[[2]]&/@\[Beta]Functions (* define explicit RG equations *)
]


RGSolve::usage="RGSolve[\[Beta]Functions,boundaryValues,{\[Mu]0,\[Mu]min,\[Mu]max}] solves the RG equations with the given boundary conditions.
RGSolve[\[Beta]Rules,boundaryValues,{\[Mu]0,\[Mu]min,\[Mu]max}] automatically turns the list of rules to RG equations.";
Options[RGSolve]={Return->Rule,"NumericRules"->{}};
AutoComplete[RGSolve];
RGSolve[\[Beta]Functions_List,boundaryValues_,{\[Mu]0_,\[Mu]min_,\[Mu]max_},OptionsPattern[]]:=Module[{out,vars,RGequations,boundaryConditions},
	vars=\[Beta]Functions[[All,1]];
	RGequations=If[Head@\[Beta]Functions[[1]]===Equal,
		\[Beta]Functions,
		BetaToEquations[\[Beta]Functions,\[Mu]]
		]/.OptionValue["NumericRules"];
	boundaryConditions=Thread[Through[vars[\[Mu]0]]==boundaryValues];
	out=NDSolveValue[
		RGequations~Join~boundaryConditions,
		vars,
		{\[Mu],\[Mu]min,\[Mu]max}
	];
	If[OptionValue[Return]===Rule,
		Thread[vars->out],
		out
		]
]


PlotRG::usage="PlotRG[RGSolutions,parameters,energyRange] plots the RG evolutions for parameters.
PlotRG[parameters,energyRange] assumes the symbol 'RGSolutions' from DRTools` is defined.";
Options[PlotRG]={"EnergyScaleSymbol"->"\[CapitalLambda]"};
PlotRG[RGSolutions_List,params_,energyRange_List,opt:OptionsPattern[{PlotRG,Plot}]]:=Module[{allParams,pars,badParams,expr,optPlot},
	optPlot=Evaluate@FilterRules[{opt},Options[Plot]];
	allParams=RGSolutions[[All,1]];
	pars=Switch[params,
		All,allParams,
		_Symbol,If[!params\[Element]allParams,Print@StringTemplate["Parameter `` not found in 'RGSolutions'."][params]];
			{params},
		_List,badParams=Complement[params,allParams];
			If[badParams=!={},Print@StringTemplate["Parameters `` not found in 'RGSolutions'."][badParams]];
			DeleteElements[params,badParams]
		 ];
	expr=pars/.RGSolutions;
	Plot[(#/.RGSolutions)[\[CapitalLambda]]//Evaluate,{\[CapitalLambda],Sequence@@energyRange},
		optPlot//Evaluate,
		(*PlotLabels->pars,*)
		AxesLabel->{OptionValue["EnergyScaleSymbol"],#}
		]&/@pars
	]
PlotRG[params_,energyRange_List,opt:OptionsPattern[]]:=PlotRG[RGSolutions,params,energyRange,opt]
AutoComplete[PlotRG,{PlotRG,Plot}];


(* ::Subsection:: *)
(*DR step*)


DRStep::usage="DRStep[T,\[Mu]4DPrefactor,parameters4DRules] returns the couplings, masses and other parameters in the 3d effective theory.";
Options[DRStep]={"OrderDR"->"NLO","US"->True,"CheckDebyeMassSq"->False,"NumericRules"->{}};
AutoComplete[DRStep];O
DRStep[Temp_,\[Mu]4DPrefactor_,parameters4DRules_,OptionsPattern[]]:=Module[{\[Mu]4d,TempRules,
		p4dRules,
		ks,c3d,m3d,mDb,\[Lambda]s,
		p3dRules,cUS,mUS,
		order=OptionValue["OrderDR"]
	},
	(* legend: \[Mu]->energy scale, Temp->temperature, p->parameters, k->constants, c->couplings,
	m->scalar masses, mDb->Deby masses,  \[Lambda]->temporal scalars *)
	\[Mu]4d=\[Mu]4DPrefactor \[Pi] Temp;
	TempRules={Global`T->Temp,Global`\[Mu]->\[Mu]4d}; (* temporary fix: turning expressions with `T` into functions of `Temp`*)
	ks=DRExpressions["Constants3d"]/.TempRules;
	p4dRules=Flatten[{parameters4DRules,ks,TempRules}];
	(* 3d quantities *)
	c3d=DRExpressions["Couplings3d"]/.p4dRules;
	\[Lambda]s=DRExpressions["TempScalar3d"]/.p4dRules;
	mDb=DRExpressions["DebyeMass3dLO"];
	m3d=DRExpressions["ScalarMass3dLO"];
	If[order=="NLO",
		mDb[[All,2]]+=DRExpressions["DebyeMass3dNLO"][[All,2]];
		m3d[[All,2]]+=DRExpressions["ScalarMass3dNLO"][[All,2]];
		];
	mDb=mDb/.p4dRules;
	AppendTo[p4dRules,Global`\[Mu]3->Sqrt[Min[DeleteCases[mDb[[All,2]],0]]]]; (* setting \[Mu]3 energy scale to minimum Debye mass *)
	If[OptionValue["CheckDebyeMassSq"]\[And](Min[mDb[[All,2]]]<0),Print["Negative square Debye mass!"]];
	m3d=m3d/.c3d/.\[Lambda]s/.p4dRules;
	(*Column@{mDb,p4dRules[[-1]],m3d}*)
	(* US quantities (optional) *)
	If[OptionValue["US"],
		AppendTo[p4dRules,Global`\[Mu]3US->Sqrt[Min[DeleteCases[mDb[[All,2]],0]]]]; (* setting \[Mu]3US energy scale to minimum Debye mass *)
		p3dRules=Flatten[{c3d,\[Lambda]s,mDb,m3d}];
		cUS=DRExpressions["CouplingsUS"]/.p4dRules/.p3dRules;
		mUS=DRExpressions["ScalarMassUSLO"];
		If[order=="NLO",
			mUS[[All,2]]+=DRExpressions["ScalarMassUSNLO"][[All,2]]
			];
		mUS=mUS/.p4dRules/.p3dRules;
		Return[{cUS,mUS,p4dRules[[-2;;]]}//Flatten]
	];
	Return[{c3d,m3d,p4dRules[[-1]]}/.OptionValue["NumericRules"]//Flatten]
	]
AutoComplete[DRStep];


(* ::Subsubsection:: *)
(*Effective masses & high-T perturbativity (m/\[CapitalLambda])*)


DRScalarSquareMasses::usage="DRScalarSquareMasses[T,\[Mu]4DPrefactor,RGSolutions] gives the eigenvalues of the scalar square-mass matrix, in the 3D effective theory.";
DRScalarSquareMasses[T_,s_,RGSolutions_,opt:OptionsPattern[{DRStep}]]:=Module[{pars,drstep,params3drules,msq3d,optDR},
	optDR=Evaluate@FilterRules[{opt}~Join~Options[DRStep],Options[DRStep]]//DeleteDuplicatesBy[#,First]&;
	pars=RGSolutions[[All,1]];
	drstep=DRStep[T,s,RGSolutions/.((l_->r_):>(l->r[s \[Pi] T])),optDR]~Join~OptionValue["NumericRules"];
	params3drules=Thread[pars->drstep[[;;Length@RGSolutions,1]]];
	msq3d=Normal[DRExpressions["SquareMassMatrixScalar"]]/.params3drules;
	Eigenvalues[msq3d/.drstep]
	]
AutoComplete[DRScalarSquareMasses,{DRScalarSquareMasses,DRStep}];


"DRScalarSquareMasses[T,\[Mu]4DPrefactor,RGSolutions] gives the eigenvalues of the vector square-mass matrix, in the 3D effective theory."
DRVectorSquareMasses[T_,s_,RGSolutions_,opt:OptionsPattern[{DRStep}]]:=Module[{pars,drstep,params3drules,Msq3d,optDR},
	optDR=Evaluate@FilterRules[{opt}~Join~Options[DRStep],Options[DRStep]]//DeleteDuplicatesBy[#,First]&;
	pars=RGSolutions[[All,1]];
	drstep=DRStep[T,s,RGSolutions/.((l_->r_):>(l->r[s \[Pi] T])),optDR]~Join~OptionValue["NumericRules"];
	params3drules=Thread[pars->drstep[[;;Length@RGSolutions,1]]];
	Msq3d=Normal[DRExpressions["SquareMassMatrixVector"]]/.params3drules;
	Eigenvalues[Msq3d/.drstep]
	]
AutoComplete[DRVectorSquareMasses,{DRVectorSquareMasses,DRStep}];


HighTRatio::usage="HighTRatio[T,\[Mu]4DPrefactor,RGSolutions,\[Phi]] gives the mass-over-energy scale ratio at the field value \[Phi]: \!\(\*SubsuperscriptBox[\(m\), \(eff\), \(max\)]\)/\[CapitalLambda].
HighTRatio[T,\[Mu]4DPrefactor,RGSolutions,\[Phi][T]] extracts the field value from a phase \[Phi][T].
HighTRatio[T,\[Mu]4DPrefactor,RGSolutions,{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\)[T],\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)[T]}] gives the ratios for a list of field values or phases. 
";
HighTRatio[T_,s_,RGSolutions_,phases_List,opt:OptionsPattern[{DRStep}]]:=Table[HighTRatio[T,s,RGSolutions,phase,opt],{phase,phases}]
HighTRatio[T_,s_,RGSolutions_,phase_,opt:OptionsPattern[{DRStep}]]:=Module[{msq3d,minimum},
	minimum=If[NumericQ[phase],phase,phase[T]];
	msq3d=DRVectorSquareMasses[T,s,RGSolutions,opt]/.DRExpressions["VEV"][[1]]->minimum;
	Sqrt[Max[msq3d]]/(s \[Pi] T)
	]


(* ::Subsubsection:: *)
(*Plot DR Quantities*)


PlotDR::usage="PlotDR[RGSolutions,param3dUS,energyRange,scaleFactor] plots an ultrasoft parameter in the given energy range.
PlotDR[param3dUS,energyRange,scaleFactor] assumes the RGSolutions symbol from DRTools` is defined.
PlotDR[{p1_3dUS,..},energyRange,scaleFactor] plots multiple ultrasoft parameters.
PlotDR[All,energyRange,scaleFactor] plots all ultrasoft parameters.";
Options[PlotDR]={"US"->True,"EnergyScaleSymbol"->"\[CapitalLambda]"};
PlotDR[RGSolutions_List,params_,energyRange_List,s_?NumericQ,opt:OptionsPattern[{PlotDR,DRStep,Plot}]]:=Module[{
	pars,optDR,optPlot,allParams
	},
	optDR=Evaluate@FilterRules[{opt}~Join~Options[PlotDR],Options[DRStep]];
	optPlot=Evaluate@FilterRules[{opt},Options[Plot]];
	allParams==(Symbol[ToString[#]<>"3d"<>If[OptionValue["US"],"US",""]]&/@RGSolutions[[All,1]]);
	pars=Switch[params,
		All,(Symbol[ToString[#]<>"3d"<>If[OptionValue["US"],"US",""]]&/@RGSolutions[[All,1]]),
		_Symbol,{params},
		_List,params];
	Plot[#/.DRStep[\[Mu]/(s \[Pi]),s,RGSolutions/.((l_->r_)->(l->r[\[Mu]])),optDR],
		{\[Mu],Sequence@@energyRange},
		Evaluate[Sequence@@optPlot],
		AxesLabel->{OptionValue["EnergyScaleSymbol"],#}
		]&/@pars
	]
PlotDR[params_,energyRange_,s_?NumericQ,opt:OptionsPattern[]]:=Module[{},
	If[!ValueQ[RGSolutions],Print["Symbol RGSolutions must be defined or provided as first argument!"];Abort[]];
	PlotDR[RGSolutions,params,energyRange,s,opt]
	]
AutoComplete[PlotDR,{PlotDR,Plot}];


(* check m/\[Mu] ratio *)
PlotHighTRatio::usage="PlotHighTRatio[{\!\(\*SubscriptBox[\(T\), \(min\)]\),\!\(\*SubscriptBox[\(T\), \(max\)]\)},\[Mu]4DPrefactor,RGSolutions,\[Phi]] plots the mass-over-energy scale ratio for the given phase: \!\(\*SubsuperscriptBox[\(m\), \(eff\), \(max\)]\)(T)/\[CapitalLambda].
PlotHighTRatio[{\!\(\*SubscriptBox[\(T\), \(min\)]\),\!\(\*SubscriptBox[\(T\), \(max\)]\)},\[Mu]4DPrefactor,RGSolutions,{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)}] plots the ratios for the given list of phases.
PlotHighTRatio[transition,\[Mu]4DPrefactor,RGSolutions,{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)}] plots the ratios for the phases of a Transition object.
";
Options[PlotHighTRatio]={"EnergyScaleSymbol"->"\[CapitalLambda]"};
PlotHighTRatio[{Tmin_,Tmax_},s_,RGSolutions_,phase_,opt:OptionsPattern[{PlotHighTRatio,DRStep,Plot}]]:=Module[{optDR,optPlot},
	optDR=Evaluate@FilterRules[{opt}~Join~Options[DRStep],Options[DRStep]]//DeleteDuplicatesBy[#,First]&;
	optPlot=Evaluate@FilterRules[{opt}~Join~Options[Plot],Options[Plot]]//DeleteDuplicatesBy[#,First]&;
	Plot[HighTRatio[T,s,RGSolutions,phase,optDR]//Evaluate,
		{T,Tmin,Tmax},
		optPlot//Evaluate,
		AxesLabel->{"T","\!\(\*SubscriptBox[\(m\), \(3  D\)]\)/"<>OptionValue["EnergyScaleSymbol"]}
		]
	]
PlotHighTRatio[tr_PT2GW`Transition,s_,RGSolutions_,opt:OptionsPattern[{PlotHighTRatio,DRStep,Plot}]]:=PlotHighTRatio[tr["Domain"],s,RGSolutions,tr["Phases"],opt]
AutoComplete[PlotHighTRatio,{PlotHighTRatio,PlotDR,Plot}];


(* ::Subsection:: *)
(*Effective potential*)


DefineDRPotential::usage="DefineDRPotential[] constructs the analytic effective potential VTot[\[Phi],order] and defines the \"VTotLO\", \"VTotNLO\", \"VTotNNLO\" entries in DRExpressions.";
DefineDRPotential[]:=Module[{vev},
	DRExpressions["VTotLO"]=DRExpressions["VeffLO"];
	DRExpressions["VTotNLO"]=Total[{"VeffLO","VeffNLO"}/.DRExpressions]//Simplify;
	DRExpressions["VTotNNLO"]=Total[{"VeffLO","VeffNLO","VeffNNLO"}/.DRExpressions]//Simplify;
	vev=DRExpressions["VEV"]//First; (* extract vev symbol *)
	DRExpressions["VTot"]=Function[{\[Phi],order},Switch[order,
		"LO",DRExpressions["VTotLO"]/.vev->\[Phi]//Evaluate,
		"NLO",DRExpressions["VTotNLO"]/.vev->\[Phi]//Evaluate,
		"NNLO",DRExpressions["VTotNNLO"]/.vev->\[Phi]//Evaluate]//Evaluate];
	AutoComplete[DRExpressions];
	]


DRPotentialN::usage="DRPotentialN[\[Phi],T,RGSolutions,\[Mu]4DPrefactor] gives the (numerical) effective potential extracted from DRalgo.";
Options[DRPotentialN]={"OrderVeff"->"NNLO","US"->True,"RescaleTo4D"->True,"NumericRules"->{}};
AutoComplete[DRPotentialN,{DRPotentialN,DRStep}];
DRPotentialN[\[CurlyPhi]_,T_,RGsol_,\[Mu]4DPrefactor_,opt:OptionsPattern[{DRPotentialN,DRStep}]]:=Module[
	{\[Mu]4d,p4d,p3d,optDR,
		oVeff=OptionValue["OrderVeff"],numRules=OptionValue["NumericRules"]
	},
	optDR=Evaluate@FilterRules[{opt}~Join~Options[DRPotentialN]~Join~Options[DRStep],Options[DRStep]];
	(* define analytic potential if missing *)
	If[!KeyExistsQ[DRExpressions,"VTot"],DefineDRPotential[]];
	\[Mu]4d=\[Mu]4DPrefactor \[Pi] T;
	p4d=RGsol/.((l_->r_):>(l->r[\[Mu]4d]));
	p3d=(DRStep[T,\[Mu]4DPrefactor,p4d,optDR]/.numRules)~Join~numRules;
	If[OptionValue["RescaleTo4D"],
		T DRExpressions["VTot"][\[CurlyPhi]/Sqrt[T],oVeff]/.p3d,
		DRExpressions["VTot"][\[CurlyPhi],oVeff]/.p3d
		]
]


(* ::Subsubsection:: *)
(*Wrapping function*)


ComputeDRPotential::usage="ComputeDRPotential[benchmarkParameters,{\[Mu],\[Mu]min,\[Mu]max}] returns the effective potential V[\[Phi],T].
ComputeDRPotential[benchmarkParameters,{\[Mu],\[Mu]min,\[Mu]max},\"SubRules\"\[Rule]subRules] passes substitution rules to StoreDRExpressions. 
ComputeDRPotential[benchmarkParameters,{\[Mu],\[Mu]min,\[Mu]max},\"LoadDRFrom\"\[Rule]file] loads DR expressions from file.";
Options[ComputeDRPotential]={"SubRules"->None,"OrderDR"->"NLO","OrderVeff"->"NNLO","PlotRG"->False,"LoadDRFrom"->None};
functionList={ComputeDRPotential,StoreDRExpressions,DRPotentialN};
AutoComplete[ComputeDRPotential,functionList];
ComputeDRPotential[bchmParams_,energyRange_,opt:OptionsPattern[functionList]]:=Module[{
	optStore,optVeff,optRG,
	load=OptionValue["LoadDRFrom"],subRules=OptionValue["SubRules"]},
	optStore=Evaluate@FilterRules[{opt}~Join~Options[StoreDRExpressions],Options[StoreDRExpressions]]//DeleteDuplicatesBy[#,First]&;
	optRG=Evaluate@FilterRules[{opt}~Join~Options[RGSolve],Options[RGSolve]]//DeleteDuplicatesBy[#,First]&;
	optVeff=Evaluate@FilterRules[{opt}~Join~Options[DRPotentialN],Options[DRPotentialN]]//DeleteDuplicatesBy[#,First]&;
	(* store DRalgo quantities *)
	Which[load=!=None,LoadDRExpressions[load],
		subRules=!=None,StoreDRExpressions["SubRules"->subRules,optStore],
		True,Print@msg["computeVeff"]
		];
	(* solve RG equations *)
	RGSolutions=RGSolve[DRExpressions["BetaFunctions"],bchmParams,energyRange,optRG];
	If[OptionValue["PlotRG"],Echo[PlotRG[All,Rest@energyRange,ScalingFunctions->"SignedLog",ImageSize->Small]//TableForm,"RG solutions \[Rule]"]];
	(* effective potential *)
	DefineDRPotential[];
	Function[{\[Phi],T},Check[DRPotentialN[\[Phi],T,RGSolutions,1,optVeff],
		Limit[DRPotentialN[\[CurlyPhi],T,RGSolutions,1,optVeff],\[CurlyPhi]->\[Phi]]
		]//Quiet]
	]


(* ::Section:: *)
(*End package*)


End[]; (* Private` *)


(* ReadProtected attribute on public symbols prevents rendering of huge box with all 
definitions (DownValues) when they are called in Information or with shortcut ?FindBounce. *)
SetAttributes[Evaluate@Names["`*"],{ReadProtected}]; (* from FindBounce.m *) 


EndPackage[] (* DRTools` *)


Print["\[Checkmark] Imported \!\(\*StyleBox[\"DRTools`\",\nFontWeight->\"Bold\"]\)"];
