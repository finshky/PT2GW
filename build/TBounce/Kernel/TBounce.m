(* ::Package:: *)

(* ::Section::Closed:: *)
(*Header*)


(* :Title: TBounce *)
(* :Context: TBounce` *)
(* :Author: M&M *)
(* :Summary: Computes phase transition parameters for thermal potentials. *)
(* :Keywords: tunneling, first order phase transitions, bubble nucleation, gravitational waves, cosmology *)


(*  Copyright (C) 2019

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License aM published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)


(* ::Section::Closed:: *)
(*Begin Package*)


BeginPackage["TBounce`",{"FindBounce`","NumericalCalculus`","GW`","Examples`"}]


(* ::Subsection::Closed:: *)
(*Available public Symbols*)


(* utilities *)
DefineUnits;
TranslatedSCubic;
NewMessageGroup;
Bisection;
MassFunction;
AutoComplete;


(* cosmology *)
H;
\[CapitalGamma]OverH4;
Int\[CapitalGamma]OverH4;
IFV;


(* phase transition *)
Overlap;
FindTcrit;
FindTnuc;
FindTperc;
PercolationCheck;
\[Alpha]Fun;
\[Beta]HFun;


(* action functions *)
Action;
ActionFit;


(* plot functions *)
PlotPotential;
PlotAction;
PlotPhases;
PlotTransition;
PlotGW;
PlotGWSensitivities;


(* high-level paclet functions *)
TracePhases;
SearchPhases;
SearchPotential;


(* objects *)
Transition;
ActionFunction;


(* symbols *)
gstar;
Unit; (* energy units *)
MPl;  (* Planck mass *)
MPlN; (* numerical value of Planck mass *)
(*T; (* temporary fix *)
\[Phi];*)


(* Clear definitions from package symbols in public and private context. *)
ClearAll["`*","`*`*"];


(* ::Section:: *)
(*Code*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Utilities*)


(* ::Subsubsection::Closed:: *)
(*Version compatibility*)


(* Manual implementation of MinMax function, because it has been only added in Mathematica 10.1
It matches the original in all examples from documentation. *)
(*minMax[list_List]:={Min[list],Max[list]};

minMax[list_List,d_]:={Min[list]-d,Max[list]+d};

minMax[list_List,Scaled[s_]]:=With[
	{d=s*(Max[list]-Min[list])},
	{Min[list]-d,Max[list]+d}
];

minMax[list_List,{dMin_,dMax_}]:={Min[list]-dMin,Max[list]+dMax};*)


(*subdivide[x1_,x2_,n_Integer]:=Table[x1+(x2-x1)*k/n,{k,0,n}];*)


(*(* Use user-defined function only in earlier versions. *)
If[
	$VersionNumber<10.1,
	MinMax=minMax;
	Subdivide=subdivide
];*)


(* ::Subsubsection::Closed:: *)
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


(* ::Subsubsection::Closed:: *)
(*Filter Options*)


FilterOptions[opt___,sym_Symbol]:=Evaluate@FilterRules[{opt}~Join~Options[sym],Options[sym]]//DeleteDuplicatesBy[#,First]&
FilterOptions[opt___,syms_List]:=Evaluate@FilterRules[{opt}~Join~Options[First@syms],Join@@Options/@syms]//DeleteDuplicatesBy[#,First]&


(* ::Subsubsection::Closed:: *)
(*Bisection algorithm*)


Bisection::usage="Bisection[f, range, xTol, yTol] is a root solver based on the bisection method.";
Bisection::noasc="The function failed to evaluate at supremum `1`.
Either restrict the interval or provide \"Ascending\" option (True or False).";
Options[Bisection]={MaxIterations->\[Infinity],Direction->Up,Return->"x","PrintIterations"->False,"Ascending"->Automatic};
Bisection[f_,xrange_,xtol_,ytol_,OptionsPattern[]]:=Module[
	{x, y, xm=1+xtol, ym, res,
		print=OptionValue["PrintIterations"],
		maxIter=OptionValue[MaxIterations],ascending=OptionValue["Ascending"]
	},
	(* initialization *)
	x=Last@xrange;
	If[maxIter==Automatic,maxIter=Ceiling[-Log2[xtol]]];
	y=Which[
		ascending===Automatic, Check[f[x],Message[Bisection::noasc,Last@xrange];Return[Null]],
		ascending, 1+ytol,
		True, -(1+ytol)
		];
	(* Bisection algorithm*)
	If[print,Print["  x \[Function] f(x)"]];
	res=NestWhile[(
		x=xm;
		xm=Mean@#;
		ym=f[xm];
		If[print,Echo[ym,ToString[Round[xm,0.01]]<>" \[Function]"]];
		Switch[ym y,
			_?Positive|\[Infinity], y=ym; {First@#, xm},
			_?Negative|-\[Infinity], {xm, Last@#},
			0|0., y=ym; {xm,xm},
			_?(Not@*NumericQ),
			Switch[OptionValue[Direction], (* ad hoc method (not generic): if failing, push to higher/lower x values *)
				Up, {xm, Last@#},
				Down, {First@#, xm}]
		]) &,
     xrange,
     (Abs[xm-x]>xtol \[Or] Abs[ym-y]>ytol)(* \[And] (Abs[ym-y]=!=Indeterminate)*) &, {2,1}, maxIter];
     (* return *)
     res=If[NumericQ[ym y],Mean@res,None];
     If[print,If[NumericQ[ym y],Echo[f[res],ToString[Round[res,0.01]]<>" \[Function]"],Echo[Indeterminate,"Result \[Rule] "]]]; (* print last iteration *)
     OptionValue[Return]/.{"x":>res,"y":>f[res]}
  ]
AutoComplete[Bisection];


(* ::Subsubsection::Closed:: *)
(*Stable points*)


(* find and classify stable points of a multi-variate function *)
FindStable::usage="Return minima, maxima and saddle points of a (multivariate) function.
Defaults to \!\(\*TemplateBox[{},\n\"Reals\"]\) domain.";
Options[FindStable]={Return->"min","Domain"->Reals};
FindStable[f_,x_,OptionsPattern[]]:=Block[{stable,d2f,eig,min,max,saddle,opts,out},
	Switch[Head[x],
		Symbol,(* 1d function: x is a Symbol *)(
		stable=NSolve[\!\(
\*SubscriptBox[\(\[PartialD]\), \(x\)]\(f[x]\)\)==0//Simplify,x,OptionValue["Domain"]];
		d2f=\!\(
\*SubscriptBox[\(\[PartialD]\), \({x, 2}\)]\(f[x]\)\)//Simplify;
		min=Select[stable,Positive[d2f/.#]&];
		max=Select[stable,Negative[d2f/.#]&];),
		List,(* D dimensional function: x is a List *)(
		stable=NSolve[Thread[\!\(
\*SubscriptBox[\(\[PartialD]\), \({x}\)]\((f @@ x)\)\)==0],x,Reals];
		d2f=\!\(
\*SubscriptBox[\(\[PartialD]\), \({x, 2}\)]\((f @@ x)\)\);
		eig=Eigenvalues[d2f];
		min=Select[stable,AllTrue[(eig/.#),Positive]&];
		max=Select[stable,AllTrue[(eig/.#),Negative]&];)
		];
	saddle=Complement[stable,min,max];
	opts={"min","max","saddle"};
	OptionValue[Return]/.{"min"->min,"max"->max,"saddle"->saddle}
]


(* ::Subsubsection::Closed:: *)
(*Mass Function*)


MassFunction::usage="MassFunction[V] gives the square root of the second derivative \!\(\*SubsuperscriptBox[\(\[PartialD]\), \(\[Phi]\), \(2\)]\)V[\[Phi],T]\[Congruent]m[\[Phi],T].
MassFunction[V,\[Phi][T]] gives the mass function at a specific phase \[Phi][T].
MassFunction[V,{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\)[T],\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)[T],...}] gives a list of mass functions corresponding to phases \!\(\*SubscriptBox[\(\[Phi]\), \(i\)]\)[T].";
MassFunction[V_]:=Module[{\[Phi]},Function[{\[Phi],T},Sqrt[\!\(
\*SubscriptBox[\(\[PartialD]\), \({\[Phi], 2}\)]\(V[\[Phi], T]\)\)]//Evaluate]]
MassFunction[V_,phase_]:=Module[{},Function[T,StupidMassFunction[V][phase[T],T]//Evaluate]]
MassFunction[V_,phases_List]:=Table[StupidMassFunction[V,phase],{phase,phases}]


(* ::Subsubsection::Closed:: *)
(*Mathematical Functions*)


(* redefined Log *)
LogR[0.]:=-\[Infinity]
LogR[x_]:=Log[x]


(* ::Subsubsection:: *)
(*Plot Potential*)


PlotPotential::usage=="PlotPotential[V,phases,T] makes an interactive plot of V vs \[Phi].";
Options[PlotPotential]={"Manipulate"->True,
	"LogVRange"->1.,"Log\[Phi]Range"->1.,"LogTRange"->1.,"Appearance"->"Labeled",
	"Real"->False,
	ControlPlacement->Left,ControlType->VerticalSlider};
PlotPotential[V_,\[Phi]Range_,T_,opt:OptionsPattern[{PlotPotential,Plot,Manipulate}]]:=Module[{
	optPlot,optManipulate,
	real=OptionValue["Real"],
	LogVRange=OptionValue["LogVRange"],
	Log\[Phi]Range=OptionValue["Log\[Phi]Range"],
	LogTRange=OptionValue["LogTRange"]
	},
	SetOptions[Manipulator,"Appearance"->OptionValue["Appearance"]];
	optPlot=Evaluate@FilterRules[{opt}~Join~Options[PlotPotential],Options[Plot]];
	optManipulate=Evaluate@FilterRules[{opt}~Join~Options[PlotPotential],Options[Manipulate]];
	If[OptionValue["Manipulate"],
		Manipulate[Plot[If[real,Re,Identity][V[\[Phi],temp]],{\[Phi],Sequence@@(10.^log\[Phi] \[Phi]Range)},
			(*PlotRange->10.^LogVRange{-1,1},*)
			AxesLabel->{"\[Phi]","V"}],
			{{temp,T,"T"},Max@{0.,T-10^LogTRange},T+10^LogTRange,Appearance->"Labeled"},
			{{log\[Phi],0.,"log(\[Phi])"},-Log\[Phi]Range,Log\[Phi]Range,Appearance->"Labeled"},
			(*{{LogVRange,0,"log(V)"},-LogVRange,LogVRange},*)
			Evaluate[Sequence@@optManipulate],Method->"ControlAreaDisplayFunction"->(Row[(Column[#,Alignment->Center]&/@#[[1]]),Spacer[20]]&)
			],
		Plot[If[real,Re,Identity][V[\[Phi],T]],{\[Phi],Sequence@@\[Phi]Range},AxesLabel->{"\[Phi]","V"}]
		]
	]
AutoComplete[PlotPotential,{PlotPotential,Plot,Manipulate}];


(* ::Subsubsection::Closed:: *)
(*Messages*)


(* ::Text:: *)
(*Message groups*)


NewMessageGroup::usage="NewMessageGroup[name\[RuleDelayed]messages] appends the message group messages to $MessageGroups.
NewMessageGroup[] appends a default group of messages named \"SearchPotential\".";
Options[NewMessageGroup]={Print->False};
NewMessageGroup[name_:>msgs_,OptionsPattern[]]:=Module[{},
	(* drop existing message list, if any *)
	If[KeyExistsQ[$MessageGroups,name],$MessageGroups=KeyDrop[$MessageGroups,name]//Normal];
	AppendTo[$MessageGroups,name:>msgs];
	If[OptionValue[Print],Print["Added message group: ",name:>msgs]]
	]
NewMessageGroup[opt:OptionsPattern[]]:=Module[{
	msgGroup="TBounce":>{(*FindRoot::reged,*)FindRoot::jsing,FindMinimum::lstol,FindBounce::cvmit,FindBounce::errbc,NSolve::ratnz},
	optMsg=Evaluate@FilterRules[{opt}~Join~Options[NewMessageGroup],Options[NewMessageGroup]]
	},
	NewMessageGroup[msgGroup,optMsg];
	]
NewMessageGroup[];
Off["TBounce"]; (* turn off messages by default. To turn on, use On["SearchPotential"] *)


(* ::Text:: *)
(*Print messages*)


findRootLink="\!\(\*TemplateBox[{Cell[TextData[\"FindRoot\"]], \"paclet:ref/FindRoot\"},\n\"RefLink\",\nBaseStyle->{\"InlineFormula\"}]\)";


msg["tracing"]="Tracing the phases..";
msg["optTrace"]="Unknown option `1` for \!\(\*
StyleBox[\"TracePhases\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\".\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"Valid\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"options\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"are\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"Solve\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\",\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"NSolve\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)or \"Numeric\".";
msg["noTRange"]="Bad temperature range ``.\nWith \"TracingMethod\"\[Rule]\"Numeric\", a temperature range in the form \"TRange\"\[Rule]{\!\(\*SubscriptBox[\(T\), \(min\)]\),\!\(\*SubscriptBox[\(T\), \(max\)]\)} must be provided.";
msg["Vcomplex"]=StringTemplate["Complex potential detected at `` phase!"];
msg["UnboundedV"]=StringTemplate["Solution did not converge for T\[Element]``. Potential might be unbounded from below!"];


msg["Tn"][a_Association]:=Row[If[KeyExistsQ[a,#],
	Style[Row[{#/.{"Tn"->"","STn"->Style["\!\(\*SubscriptBox[\(S\), \(3\)]\)/T \[Rule] ",Gray],"GammaH4"->"\[CapitalGamma]/\!\(\*SuperscriptBox[\(H\), \(4\)]\) \[Rule] ","IntGammaH4"->"\!\(\*SubsuperscriptBox[\(\[Integral]\), SubscriptBox[\(T\), \(n\)], SubscriptBox[\(T\), \(c\)]]\)\!\(\*FractionBox[\(\[DifferentialD]T\), \(T\)]\)\!\(\*FractionBox[\(\[CapitalGamma]\), SuperscriptBox[\(H\), \(4\)]]\) \[Rule] "},
	#/.a}],If[#=="Tn",Black,Gray]]]&/@{"Tn","STn","GammaH4","IntGammaH4"},Spacer[20]
	]
msg["NoTnuc"]="Nucleation not reached!";
msg["noTnCriterion"]=StringTemplate["Bad \"NucleationCriterion\"\[Rule]\"\!\(\*
StyleBox[\"`1`\",\nStripOnInput->False,\nLineColor->RGBColor[1, 0, 0],\nFrontFaceColor->RGBColor[1, 0, 0],\nBackFaceColor->RGBColor[1, 0, 0],\nGraphicsColor->RGBColor[1, 0, 0],\nFontColor->RGBColor[1, 0, 0]]\)\" in FindTnuc.\nMust match any of `2`."];
msg["missingIntGammaMethod"]=StringTemplate["With \"NucleationCriterion\"\[Rule]\"IntegralGammaOverH4\", \"NucleationMethod\" must include ``."];


msg["NoTperc"]="Percolation not reached!";
msg["searchingTp"]=StringTemplate["Solving \!\(\*SubscriptBox[\(`simple`I\), \(\[ScriptCapitalF]\)]\)(\!\(\*SubscriptBox[\(T\), \(p\)]\)) = `target` for \!\(\*SubscriptBox[\(T\), \(p\)]\) with `method` method..."];


msg["SinglePhase"]="Only 1 phase detected.";
msg["loopPhases"]="Looping over pairs of phases";
msg["MetaKeys"]=StringTemplate["Metadata Keys `` will be converted to String!"];
msg["vw"]=StringTemplate["Bubble wall velocity \!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\)=`` must be bewteen 0 and 1."];
msg["computations"]=StringTemplate["Form of `1`\[Rule]\!\(\*FormBox[
StyleBox[\"`2`\",\nStripOnInput->False,\nLineColor->RGBColor[1, 0, 0],\nFrontFaceColor->RGBColor[1, 0, 0],\nBackFaceColor->RGBColor[1, 0, 0],\nGraphicsColor->RGBColor[1, 0, 0],\nFontColor->RGBColor[1, 0, 0]],
TraditionalForm]\) doesn't match `3`. Returning most recent result."];
msg["aborted"]=Style["Aborted. Returning most recent result.",Red];
msg["NoTran"]="No transition detected.";


(* ::Subsection::Closed:: *)
(*Physical quantities*)


(* ::Subsubsection::Closed:: *)
(*Constants*)


(* physical constants *)
(* Subscript[g, *]: number of relativistic degrees of freedom *)
gstar=106.75;
(* reduced Planck mass in GeV (1.22 10^19) *)
MPl=Quantity["ReducedPlanckMass"];
MPlN=MPl Quantity["SpeedOfLight"]^2//UnitConvert[#,"GeV"]&//QuantityMagnitude; (* defaults to GeV units. Use DefineUnits to switch *)


(* ::Subsubsection::Closed:: *)
(*Units*)


Unit::usage="Unit is the energy unit symbol (defaults to \"GeV\").";
MPl::usage="MPl is the reduced Planck mass.";
MPlN::usage="MPlN is Planck mass numerical value, in the energy unit set by DefineUnits[string].";
DefineUnits::usage="DefineUnits[string] sets the energy unit symbol Unit to string (defaults to \"GeV\").
DefineUnits[] resets Unit to \"GeV\".";
DefineUnits[u_:"GeV"]:=Module[{},
	Unit=Echo[u,"Energy units set to"];
	MPlN=Echo[MPl Quantity["SpeedOfLight"]^2//UnitConvert[#,u]&,Row@{MPl," \[Rule]"}]//QuantityMagnitude;
	]
DefineUnits[] (* initialize to GeV units *)


(* ::Subsubsection::Closed:: *)
(*Cosmological definitions*)


(* vacuum energy radiation density *)
Subscript[\[Rho], \[Gamma]][T_]:=\[Pi]^2/30 gstar T^4


(* Hubble parameter *)
H::usage="H[T] gives the Hubble parameter at temperature T.
H[T,V,{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)}] includes the vacuum contribution to the total energy density.
";
H[T_?NumericQ]:= Sqrt[Subscript[\[Rho], \[Gamma]][T]/3]/MPlN
H[T_?NumericQ,V_,phases_]:=Module[{xm1,xm2},{xm1,xm2}=phases;
	\[Sqrt]((Subscript[\[Rho], \[Gamma]][T]+Abs[V[xm1[T],T]-V[xm2[T],T]])/3)/MPlN
	]
Hc=H[1];


(* nucleation rates *)
\[CapitalGamma]::usage="\[CapitalGamma][T,ActionFunction] gives the vacuum decay rate.
\[CapitalGamma][T,ActionValue] gives the vacuum decay rate.
\[CapitalGamma][T,V,{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)}] includes the vacuum energy contribution to the Hubble parameter.
";
\[CapitalGamma][T_?NumericQ,action_Function|action_Symbol,opt:OptionsPattern[]]:=With[{s=action[T,opt]},
	If[s<-Log@$MinMachineNumber,T^4 ((1/(2\[Pi]))s)^(3/2) E^-s,0.]]
\[CapitalGamma][T_?NumericQ,action_?NumericQ]:=If[action<-Log@$MinMachineNumber,T^4 (action/(2\[Pi]))^(3/2) E^-action,0]
\[CapitalGamma][T_?NumericQ,V_,phases_,opt:OptionsPattern[]]:=With[{s=Action[T,V,phases,opt]},
	If[(s<-Log@$MinMachineNumber),T^4 (s/(2\[Pi]))^(3/2) E^-s,0]]


(* nucleation criterion: \[CapitalGamma]/H^4 *)
\[CapitalGamma]OverH4::usage="\[CapitalGamma]OverH4[T,V,{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)}] gives the vacuum decay rate per Hubble volume and Hubble time: \[CapitalGamma]/\!\(\*SuperscriptBox[\(H\), \(4\)]\).
\[CapitalGamma]OverH4[T,ActionFunction,V,{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)}] gives uses the input ActionFunction instead of computing it with FindBounce."
\[CapitalGamma]OverH4[T_,V_,phases_,opt:OptionsPattern[]]:=\[CapitalGamma][T,V,phases,opt]/H[T,V,phases]^4
\[CapitalGamma]OverH4[T_?NumericQ,action_Function|action_Symbol,V_,phases_,opt:OptionsPattern[]]:=\[CapitalGamma][T,action,opt]/H[T,V,phases]^4


(* nucleation criterion: \[Integral]dT/T \[CapitalGamma]/H^4 *)
Int\[CapitalGamma]OverH4::usage="Int\[CapitalGamma]OverH4[T,Tc,ActionFunction,V,phases] gives the integral \[Integral]dT/T \[CapitalGamma]/\!\(\*SuperscriptBox[\(H\), \(4\)]\). Nucleation is achieved when Int\[CapitalGamma]OverH4=1.";
Int\[CapitalGamma]OverH4[T_?NumericQ,Tc_,action_Function|action_Symbol,V_,phases_,opt:OptionsPattern[]]:=NIntegrate[\[CapitalGamma]OverH4[\[ScriptCapitalT],action,V,phases,opt]/\[ScriptCapitalT],{\[ScriptCapitalT],T,Tc}]


Int\[CapitalGamma]OverH4[x,Tc,STfun,V,phases,optST]


(* -Log(false vacuum fractional volume): Pf=\[ExponentialE]^-IFV *)
IFV0[T_?NumericQ,T2_?NumericQ]:=NIntegrate[1/H[T3],{T3,T,T2}]
IFV::usage="IFV[action,T,Tc,\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\)] gives the -log(\!\(\*SubscriptBox[\(P\), \(\[ScriptCapitalF]\)]\)), where \!\(\*SubscriptBox[\(P\), \(\[ScriptCapitalF]\)]\) is the false vacuum fractional volume.
IFV[action,T,Tc,\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\),V,{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)}] includes the vacuum contribution \[CapitalDelta]V in the Hubble parameter."
IFV[action_,T_?NumericQ,Tc_,vw_]:=4\[Pi]/3 vw^3 NIntegrate[(1/T2^4)(\[CapitalGamma][T2,action]/H[T2])IFV0[T,T2]^3,{T2,T,Tc}]
IFV[action_,T_?NumericQ,Tc_,vw_,V_,phases_]:=4\[Pi]/3 vw^3 NIntegrate[(1/T2^4)(\[CapitalGamma][T2,action]/H[T,V,phases])IFV0[T,T2]^3,{T2,T,Tc}]
IFV1[action_,T_?NumericQ,Tc_,vw_]:=4\[Pi]/3 (vw/T)^3 NIntegrate[((T2-T)^3/T2^9)(\[CapitalGamma][T2,action]/Hc^4),{T2,T,Tc}](*IFV1[action_,T_,Tc_,vw_]:= ((4\[Pi] vw^3)/(3T^3))\!\(
\*SubsuperscriptBox[\(\[Integral]\), \(T\), \(Tc\)]\(\((
\*SuperscriptBox[\((T2 - T)\), \(3\)]/
\*SuperscriptBox[\(T2\), \(9\)])\)\[CapitalGamma][T2, action]\[DifferentialD]T2\)\)*)


(* ::Subsection:: *)
(*Action*)


(* ::Text:: *)
(*Requires FindBounce!*)


(* ::Subsubsection::Closed:: *)
(*Euclidean Bounce Action*)


(* ::Text:: *)
(*Where FindBounce is called*)


(* Return action over temperature (S/T) *)
Action::usage="Action[T,V,{Subscript[\[Phi], 1],Subscript[\[Phi], 2]}] computes the 3D Euclidean action Subscript[S, 3](T)/T (calls FindBounce).";
Options[Action]={
	"FieldPoints"->51,"Dimension"->3,"Gradient"->None,
	"ReBounce"->{True,"ShiftToExitPoint"->True,"LowerActionTolerance"->True},
	"CheckProfile"->{True,"ShiftToExitPoint"->True,"LowerActionTolerance"->True},
	"PrintBounceInfo"->False,"PrintAction"->False,"PrintShiftInfo"->False,"PlotBounce"->False,"Action/T"->True
	};
Action[T_?NumericQ,V_,phases_,opt:OptionsPattern[{Action,FindBounce,ShiftToExitPoint,Plot}]]:=Module[{
	\[Phi],minima,max,fail,bf,bf0,\[Phi]0,\[Phi]1,xm1,xm2,\[Epsilon],fp,
	\[CapitalDelta]bounce,\[CapitalDelta]bounceMax=10.^-3,
	optFB,optPlot,optShift,
	reBounce=OptionValue["ReBounce"],
	checkProfile=OptionValue["CheckProfile"],
	print=OptionValue["PrintBounceInfo"]
	},
	optFB=Evaluate@FilterRules[{opt}~Join~Options[Action]~Join~Options[FindBounce],Options[FindBounce]]//DeleteDuplicatesBy[#,First]&; (* extract FindBounce options *)
	optShift=Evaluate@FilterRules[{opt}~Join~Options[Action]~Join~Options[FindBounce]~Join~Options[ShiftToExitPoint],Options[ShiftToExitPoint]]//DeleteDuplicatesBy[#,First]&; (* extract ShiftToExitPoint options *)
	optPlot=Evaluate@FilterRules[{opt},Options[Plot]]//DeleteDuplicatesBy[#,First]&;
	(* maximum as MidFieldPoint *)
	minima=Through[phases[T]];
	(*max=\[Phi]/.NSolve[{\!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Phi]\)]\(V[\[Phi], T]\)\)==0,\!\(
\*SubscriptBox[\(\[PartialD]\), \({\[Phi], 2}\)]\(V[\[Phi], T]\)\)<0//Simplify,\[Phi]~Between~minima},\[Phi]]//Mean;*)
	max=\[Phi]/.NMaximize[{V[\[Phi],T],\[Phi]~Between~minima},\[Phi]][[2]];
	(* call FindBounce (check for error messages) *)
	Check[fail=False;
		bf=FindBounce[V[\[Phi],T],\[Phi],minima,"MidFieldPoint"->max,optFB],
		If[reBounce//First,fail=True,fail=False],
		{(*FindBounce::cvmit,*)\!\(TraditionalForm\`FindBounce::errbc\),FindBounce::nosol}];
	(* optionally recompute bounce in case of failure or violated conditions *)
	(* 1st attempt: shift exit point *)
	If[(fail\[And]OptionValue[reBounce//Rest,"ShiftToExitPoint"]) \[Or]
		(First[checkProfile]\[And]With[{},\[CapitalDelta]bounce=BounceError[bf];\[CapitalDelta]bounce>\[CapitalDelta]bounceMax]\[And]OptionValue[checkProfile//Rest,"ShiftToExitPoint"]),
		If[print,
			If[Not@fail,
				Echo["Bounce error "<>ToString[Round[\[CapitalDelta]bounce,\[CapitalDelta]bounceMax]]<>" exceeds threshold."]];
			Echo["Recomputing with true vacuum shifted towards exit point."]];
		Check[fail=False;
			bf=ShiftToExitPoint[T,V,minima,max,optShift],
			If[reBounce//First,fail=True,fail=False],
			{(*FindBounce::cvmit,*)\!\(TraditionalForm\`FindBounce::errbc\),FindBounce::nosol}];
		If[print\[And]Not[fail]\[And]OptionValue[checkProfile//Rest,"LowerActionTolerance"]\[And](\[CapitalDelta]bounce=BounceError[bf]),
			Echo[\[CapitalDelta]bounce,"Bounce error:"]];
		];
	(* 2nd attempt: lower action tolerance *)
	If[(fail\[And]OptionValue[reBounce//Rest,"LowerActionTolerance"]) \[Or]
		(First[checkProfile]\[And]With[{},\[CapitalDelta]bounce=BounceError[bf];\[CapitalDelta]bounce>\[CapitalDelta]bounceMax]\[And]OptionValue[checkProfile//Rest,"LowerActionTolerance"]),
		If[print,
			If[Not@fail,
				Echo["Bounce error "<>ToString[Round[\[CapitalDelta]bounce,\[CapitalDelta]bounceMax]]<>" exceeds threshold."]];
			Echo["Recomputing with lower ActionTolerance."]];
		Check[fail=False;
			bf=FindBounce[V[\[Phi],T],\[Phi],minima,"MidFieldPoint"->max,"FieldPoints"->3 OptionValue["FieldPoints"],"ActionTolerance"->10.^-16,optFB],
			If[reBounce//First,fail=True,fail=False],
			{(*FindBounce::cvmit,*)\!\(TraditionalForm\`FindBounce::errbc\),FindBounce::nosol}];
		If[print\[And]Not[fail],Echo[BounceError[bf],"Bounce error:"]];
		];
	(* plot bounce (optional) *)
	If[OptionValue["PlotBounce"],Print@BouncePlot[bf,optPlot]];
	(* return action *)
	bf["Action"]If[OptionValue["Action/T"],1/T,1]//
		If[OptionValue["PrintAction"],Echo[#,"\!\(\*FractionBox[SubscriptBox[\(S\), \(3\)], \(T\)]\)="]&,Identity]
	]
Action[TL_List,V_,phases_,opt:OptionsPattern[]]:=Table[Action[T,V,phases,opt],{T,TL}]
AutoComplete[Action];


ShiftToExitPoint::usage="Shift true vacuum towards exit point, estimated by \!\(\*SubscriptBox[\(\[Phi]\), \(eq\)]\):V(\!\(\*SubscriptBox[\(\[Phi]\), \(eq\)]\))=V(\!\(\*SubscriptBox[\(\[Phi]\), \(FV\)]\)).
The \"ShiftToExitPoint\" option controls the amount of shift: \!\(\*SubscriptBox[\(\[Phi]\), \(TV\)]\) \[Rule] \!\(\*SubscriptBox[\(\[Phi]\), \(eq\)]\)+\[Epsilon](\!\(\*SubscriptBox[\(\[Phi]\), \(TV\)]\)-\!\(\*SubscriptBox[\(\[Phi]\), \(eq\)]\)).
\[Epsilon] defaults to 0.5.";
Options[ShiftToExitPoint]={"Shift"->0.5,"PrintShiftInfo"->True};
ShiftToExitPoint[T_,V_,minima_,max_,opt:OptionsPattern[{ShiftToExitPoint,Action,FindBounce,Plot}]]:=Module[
	{\[Phi],\[Phi]T,\[Phi]F,\[Phi]eq,\[Phi]0,optFB,shift=OptionValue["Shift"]},
	optFB=Evaluate@FilterRules[{opt}~Join~Options[Action]~Join~Options[FindBounce],Options[FindBounce]]//DeleteDuplicatesBy[#,First]&;
	(* extract numerical value from "ShiftToExitPoint" option, else default to 0.5 *)
	(* determine false and true vacua *)
	{\[Phi]T,\[Phi]F}=SortBy[minima,V[#,T]&];
	(* determine equivalence point *)
	\[Phi]eq=\[Phi]/.FindRoot[V[\[Phi],T]==V[\[Phi]F,T],{\[Phi],max}~Join~Sort@{\[Phi]T,max}];
	\[Phi]0=\[Phi]eq+shift(\[Phi]T-\[Phi]eq);
	If[OptionValue["PrintShiftInfo"],Echo[\[Phi]T->\[Phi]0,"Shifting \!\(\*SubscriptBox[\(\[Phi]\), \(TV\)]\)\[Rule]\!\(\*SubscriptBox[\(\[Phi]\), \(eq\)]\)+"<>ToString[shift]<>"(\!\(\*SubscriptBox[\(\[Phi]\), \(TV\)]\)-\!\(\*SubscriptBox[\(\[Phi]\), \(eq\)]\)):"]];
	(* recompute bounce *)
	FindBounce[V[\[Phi],T],\[Phi],{\[Phi]0,\[Phi]F},"MidFieldPoint"->max(*,"ActionTolerance"->10^-16*),optFB]
]


(* ::Subsubsection::Closed:: *)
(*Filter & Refine*)


BounceError[bounce_]:=Module[{radii,\[Phi]Funs,\[Phi]Extrema,\[CapitalDelta]\[Phi]},
	radii=bounce["Radii"];
	(*\[Phi]Values=bounce["Bounce"][[1]][#]&/@radii;*)
	\[Phi]Funs=Table[bounce["Bounce"][[1]][r][[1,n,1]],{n,Length[radii]-1}];
	\[CapitalDelta]\[Phi]=Table[\[Phi]Funs[[{i-1,i}]]/.r->radii[[i]],{i,2,Length[radii]-1}];
	(*\[Phi]Extrema=Table[{#/.r->radii[[n]],#/.r->radii[[n+1]]}&@\[Phi]Funs[[n]],{n,Length[radii]-1}];*)
	Sqrt[((#[[2]]-#[[1]])/(#[[2]]+#[[1]]))^2&/@\[CapitalDelta]\[Phi]//Mean]
]


STfilter::usage="Filter out points from a table of action values {{\!\(\*SubscriptBox[\(T\), \(1\)]\),(S/T\!\(\*SubscriptBox[\()\), \(1\)]\)},..} which violate \[CapitalDelta]^2(ln(S/T))/\[CapitalDelta]T^2<n.\[Sigma],
where the maximum number of standard deviation allowed is defined in the option 'stdMax'. Defaults to 1.";
Options[STfilter]={"stdMax"->1.};
STfilter[TvsST_,OptionsPattern[]]:=Module[{Tlist,STlist,dT,dT2,dST,d2ST,\[Sigma],stdMax=OptionValue["stdMax"],pos,nFiltered},
{Tlist,STlist}=TvsST\[Transpose];
dT=Differences[Tlist];
dT2=MovingAverage[dT,2];
dST=Differences[Log@STlist]/dT;
d2ST=Differences[Log@dST]/dT2;
\[Sigma]=Sqrt[Variance[d2ST]];
pos=Position[d2ST,x_/;Abs[x-Mean[d2ST]]<stdMax \[Sigma]]//Flatten;
nFiltered=Length[STlist]-Length[pos]-2;
Print[nFiltered," filtered action points (",nFiltered/Length[STlist]//N[#,2]&//PercentForm,")"];
TvsST[[{1,2}~Join~(pos+2)]]
]

STimprove::usage="Refine points filtered out by \!\(\*
StyleBox[\"STfilter\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"by\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)recomputing the action with a lower 'ActionTolerance'.
Defaults to \!\(\*SuperscriptBox[\(10\), \(-15\)]\).";
Options[STimprove]={"ActionTolerance"->10.^-15};
STimprove[V_,TvsST_,phases_,opt:OptionsPattern[]]:=Module[{optST,badData,newData,TvsSTfil,Tlist,STlist},
	optST=Evaluate@FilterRules[{opt}~Join~Options[STimprove]~Join~Options[Action],Options[Action]~Join~Options[FindBounce]]//DeleteDuplicatesBy[#,First]&;
	{Tlist,STlist}=TvsST\[Transpose];
	TvsSTfil=STfilter[TvsST];
	badData=Complement[TvsST,TvsSTfil][[All,1]];
	newData={badData,Action[V,badData,phases,optST]}\[Transpose];
	SortBy[TvsSTfil~Join~newData,First]
]


(* ::Subsubsection::Closed:: *)
(*Fit Functions*)


(* ::Text:: *)
(*Laurent polynomial \!\( *)
(*\*SubscriptBox[\(\[Sum]\), \(n\)]*)
(*\*SuperscriptBox[\((T - *)
(*\*SubscriptBox[\(T\), \(c\)])\), \(n\)]\)*)


Options[laurentFitFun]={"Orders"->Range[-2,0],Print->False};
laurentFitFun[data_,Tc_,opt:OptionsPattern[]]:=Module[{
	orders=OptionValue["Orders"],cs,optFit,printOutput=OptionValue[Print],laur},
	(* extract fitting options *)
	optFit=Evaluate@FilterRules[{opt}~Join~Options[laurentFitFun]~Join~Options[NonlinearModelFit],
		Options[NonlinearModelFit]]//DeleteDuplicatesBy[#,First]&;
	(* contruct fit function *)
	cs=(Subscript[c, #]&/@orders);
	laur=cs . ("\!\(\*SubscriptBox[\(T\), \(c\)]\)"-T)^orders;
	If[printOutput,Echo[laur,"Fitting to laurent polynomial"]];
	(* perform fit *)
	fit=NonlinearModelFit[data,{laur/."\!\(\*SubscriptBox[\(T\), \(c\)]\)"->Tc,Subscript[c, Min[orders]]>0},cs,T,optFit];
	(* return output *)
	If[printOutput,Echo[fit["EstimatedVariance"],"Estimated variance: "];];
	fit
]


(* ::Text:: *)
(*Piecewise function, merging*)
(*- interpolation far from Subscript[T, c]*)
(*- Laurent fit close to Subscript[T, c] *)


Options[pwSplineFun]={Weights->(1/#2^2&)};
pwSplineFun[data_,Tc_,opt:OptionsPattern[]]:=Module[{optInt,optFit,\[ScriptCapitalD],STint,orders,cs,laur,cons,dataFit,laurFit},
	optFit=Evaluate@FilterRules[{opt}~Join~Options[pwSplineFun],Options[NonlinearModelFit]]//DeleteDuplicatesBy[#,First]&;
	\[ScriptCapitalD]=MinMax[data[[All,1]]];
	(* low T: spline interpolation *)
	STint=Interpolation[data,Method->"Spline"];	
	(* high T: laurent polynomial *)
	orders=Range[-2,1];
	cs=(Subscript[c, #]&/@orders);
	laur=Function[T,cs . ("\!\(\*SubscriptBox[\(T\), \(c\)]\)"-T)^orders//Evaluate];
	(* continuity and smoothness conditions *)
	cons={laur[T]==STint[T],laur'[T]==STint'[T],laur''[T]==STint''[T]}/.T->\[ScriptCapitalD][[2]]/."\!\(\*SubscriptBox[\(T\), \(c\)]\)"->Tc;
	dataFit=data[[-3;;]];
	laurFit=Check[
		NonlinearModelFit[dataFit,{laur[T]/."\!\(\*SubscriptBox[\(T\), \(c\)]\)"->Tc,Subscript[c, Min[orders]]>0}~Join~cons,cs,T,optFit],
		(* in case fitting fails, brute-force the solution by imposing a constraint on Subscript[c, min] *)
		laur/.NSolve[cons~Join~{Subscript[c, Min[orders]]==1},cs][[1]]/."\!\(\*SubscriptBox[\(T\), \(c\)]\)"->Tc
	];
	(* piecewise function *)
	Function[T,\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"laurFit", "[", "T", "]"}], 
RowBox[{
RowBox[{"\[ScriptCapitalD]", "[", 
RowBox[{"[", "2", "]"}], "]"}], "<=", "T", "<", "Tc"}]},
{
RowBox[{"STint", "[", "T", "]"}], 
RowBox[{
RowBox[{"\[ScriptCapitalD]", "[", 
RowBox[{"[", "1", "]"}], "]"}], "<=", "T", "<", 
RowBox[{"\[ScriptCapitalD]", "[", 
RowBox[{"[", "2", "]"}], "]"}]}]},
{"Indeterminate", "True"}
},
AllowedDimensions->{2, Automatic},
Editable->True,
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.84]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}},
Selectable->True]}
},
GridBoxAlignment->{"Columns" -> {{Left}}, "ColumnsIndexed" -> {}, "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
GridBoxItemSize->{"Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, "Rows" -> {{1.}}, "RowsIndexed" -> {}},
GridBoxSpacings->{"Columns" -> {Offset[0.27999999999999997`], {Offset[0.35]}, Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {Offset[0.2], {Offset[0.4]}, Offset[0.2]}, "RowsIndexed" -> {}}],
"Piecewise",
DeleteWithContents->True,
Editable->False,
SelectWithContents->True,
Selectable->False,
StripWrapperBoxes->True]\)//Evaluate]
]


(* ::Subsubsection:: *)
(*Fit Module*)


ActionFit::usage="ActionFit[V,{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)},{\!\(\*SubscriptBox[\(T\), \(min\)]\),\!\(\*SubscriptBox[\(T\), \(max\)]\)}] fits and/or interpolate the 3D Euclidean action Subscript[S, 3]/T.";
ActionFit::usage="ActionFit[V,{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)},{\!\(\*SubscriptBox[\(T\), \(min\)]\),\!\(\*SubscriptBox[\(T\), \(max\)]\)},\!\(\*SubscriptBox[\(T\), \(c\)]\)] enables polynomial and Laurent fitting about \!\(\*SubscriptBox[\(T\), \(c\)]\).";
Options[ActionFit]={
	"Data"->None,
	Refine->False,
	"NActionPoints"->31,
	"StopAtFailure"->False,
	"ActionMethod"->"PWLaurent","Orders"->Range[-3,2],
	(*Function->None,Variables->None,*)
	"PlotAction"->False,ProgressIndicator->True
	};
(*SetOptions[ActionFit,{"ReBounce"->True}];*)
ActionFit[V_,phases_,{Tmin_,Tmax_},Tc_:0,opt:OptionsPattern[{ActionFit,Action,Interpolation,laurentFitFun,pwSplineFun}]]:=Module[{
	(* internal variables *)
	Tlist,STlist,TvsST,s,STfun,cs,printProg,\[Sigma],
	(* options *)
	optST,optLaur,optInt,optPW,
	data=OptionValue["Data"],
	method=OptionValue["ActionMethod"],
	(*fitFunction=OptionValue[Function],vars=OptionValue[Variables],*)
	npoints=OptionValue["NActionPoints"]
	},
	optST=Evaluate@FilterRules[{opt}~Join~Options[Action],Options[Action]]//DeleteDuplicatesBy[#,First]&;
	optLaur=Evaluate@FilterRules[{opt}~Join~Options[laurentFitFun],Options[laurentFitFun]]//DeleteDuplicatesBy[#,First]&;
	optPW=Evaluate@FilterRules[{opt}~Join~Options[pwSplineFun],Options[pwSplineFun]]//DeleteDuplicatesBy[#,First]&;
	optInt=Evaluate@FilterRules[{opt}~Join~Options[Interpolation],Options[Interpolation]]//DeleteDuplicatesBy[#,First]&;
	(* compute euclidean action values *)
	If[data===None,
	(* solve bounce action over T range *)
	(*Print["Computing euclidean bounce action Subscript[S, 3]/T over range of "<>ToString[npoints]<>" T values"];*)
	(*(* adjust fit extrema *)
	If[OptionValue["AdjustExtrema"],
	While[STfail==True,
	STmin=Action[Tmin,V,phases,optST];
	STfail=Not@NumericQ[STmin]];
	If[STfail,Tmin+=]
	*)
		Tlist=Subdivide[Tmax,Tmin,npoints];
		If[OptionValue[ProgressIndicator],
			printProg=PrintTemporary[
				ProgressIndicator[Dynamic@\[CapitalTau], {Tmax,Tmin}],
				(*Dynamic[StringTemplate["  Evaluating action at T = `1` `2`"][Dynamic@\[CapitalTau],Unit]]*) (* String are not dynamic *)
				"  Evaluating action at T = ",Dynamic[\[CapitalTau]]," ",Unit
				]
			];
		(* call Action function on T list *)
		Quiet[
		If[OptionValue["StopAtFailure"],
			(* stop at first FindBounce failure *)
			TvsST={};
			TvsST=Catch[Do[
					s=Action[\[CapitalTau],V,phases,optST];
					If[NumericQ[s],AppendTo[TvsST,{\[CapitalTau],s}],Throw[TvsST]],
				{\[CapitalTau],Tlist}];
				Throw[TvsST]],
			(* don't stop at FindBounce failure *)
			STlist=Table[Action[\[CapitalTau],V,phases,optST],{\[CapitalTau],Tlist}];
			TvsST=Cases[{Tlist,STlist}\[Transpose],{_,_?NumericQ}]
			],
			FindRoot::reged],
		(* predefined bounce action data *)
		TvsST=Cases[data,{_,_?NumericQ}]
	];

	(* apply filter on data *)
	If[OptionValue[Refine],TvsST=STimprove[V,TvsST,phases,"ShiftToExitPoint"->False]];
	
	(* select method *)
	STfun=Switch[method,
		Interpolation,Interpolation[TvsST,Method->"Spline",optInt],
		"Laurent",laurentFitFun[TvsST,Tc,optLaur],
		"PWLaurent",pwSplineFun[TvsST,Tc,optPW]
		];
	(* fit error deviation *)
	STlist=Table[STfun[T],{T,TvsST[[All,1]]}];
	\[Sigma]=Sqrt[Mean[(STlist-TvsST[[All,2]])^2]];
	(* plot (optional) *)
	If[OptionValue["PlotAction"],
		Print@PlotAction[STfun,{Tmin,Tc},"Data"->TvsST,
			"Tlines"-><|"Tc"->{Dashed,Red,InfiniteLine[{{Tc,0},{Tc,1}}],
				Text[Style["\!\(\*SubscriptBox[\(T\), \(c\)]\)",Medium],{Tc-2,1},Scaled@{.5,-.5}]}
				|>
		]];
	NotebookDelete[printProg];
	(* output *)
	(*OptionValue[Return]/.{ActionFunction->af,"Function"->STfun,"Data"->TvsST}*)
	ActionFunction[<|
		"Function"->STfun,
		"ActionMethod"->method,
		"Domain"->{Min@TvsST[[All,1]],Tc},
		If[Tc=!=0,"Tc"->Tc,Nothing],
		"StandardDeviation"->\[Sigma],
		"NActionPoints"->Length[TvsST],
		"Unit"->Unit,
		"Data"->TvsST|>]
	]
AutoComplete[ActionFit,{ActionFit,Action,Interpolation,laurentFitFun,pwSplineFun}];


(* ::Subsubsection::Closed:: *)
(*Plot action*)


PlotAction::usage="PlotAction[ActionFunction,{\!\(\*SubscriptBox[\(T\), \(min\)]\),\!\(\*SubscriptBox[\(T\), \(max\)]\)}] plot the Euclidean action Subscript[S, 3]/T in the given temperature range.
PlotAction[ActionFunction] plots the action \!\(\*SubscriptBox[\(S\), \(3\)]\)(T)/T for the given ActionFunction object.
PlotAction[Transition] \!\(\*SubscriptBox[\(S\), \(3\)]\)(T)/T for the given Transition object.
PlotAction[Associaton] \!\(\*SubscriptBox[\(S\), \(3\)]\)(T)/T for the given Association.";
Options[PlotAction]={"Data"->{},"Tlines"-><||>};
PlotAction[STfun_Function|STfun_Symbol,{Tmin_,Tmax_},opt:OptionsPattern[{PlotAction,LogPlot,ListLogPlot}]]:=Module[{
	Tm,TM,optLogPlot,optListPlot,TvsST=OptionValue["Data"],Tlines=OptionValue["Tlines"]
	},
	{Tm,TM}=MinMax[{Tmin,Tmax},Scaled[.1]];
	optLogPlot=Evaluate[FilterRules[{opt}~Join~Options[PlotAction],Options[LogPlot]]//DeleteDuplicatesBy[#,First]&];
	optListPlot=Evaluate@FilterRules[{opt}~Join~Options[PlotAction],Options[ListLogPlot]](*//DeleteDuplicatesBy[#,First]&*);
	With[{optPlot=optLogPlot},
	Show[
		LogPlot[STfun[T],{T,Tm,TM},
			optPlot,
			GridLines->Automatic,
			PlotLegends->{"Fit function"},
			AxesLabel->{"T / "<>Unit,"\!\(\*FractionBox[SubscriptBox[\(S\), \(3\)], \(T\)]\)(T)"},
			Epilog->If[Length[Tlines]>0,Lookup[Tlines,{"Tc","Tn","Tp"},Nothing],{}]
			],
		ListLogPlot[TvsST,optListPlot,PlotStyle->Black,PlotLegends->{"FindBounce"}]
		]]
	]


PlotAction[asc_Association,opt:OptionsPattern[{}]]:=Module[{STfun,Tmin,Tc,Tn,Tp,TvsST,Tlines,TlinesRule},
	TlinesRule=(l_->T_):>(l->{Dashed,l/.{"Tc"->Red,"Tn"->Blend[{Orange,Red}],"Tp"->Orange},
							InfiniteLine[{{T,0},{T,1}}],
							Text[Style[l/.{"Tc"->"\!\(\*SubscriptBox[\(T\), \(c\)]\)","Tn"->"\!\(\*SubscriptBox[\(T\), \(n\)]\)","Tp"->"\!\(\*SubscriptBox[\(T\), \(p\)]\)"},Medium],{T,1},Scaled@{.5,-.5}]
							});
	STfun=asc["Function"];
	TvsST=asc["Data"];
	Tmin=TvsST[[All,1]]//Min;
	Tlines=<|Normal[KeyTake[asc,{"Tc","Tn","Tp"}]]/.TlinesRule|>;
	PlotAction[STfun,asc["Domain"],opt,"Data"->TvsST,"Tlines"->Tlines]
	]


PlotAction[af_ActionFunction|af_Association,opt:OptionsPattern[{}]]:=Module[{STfun,Tmin,Tc,Tn,Tp,TvsST,Tlines,TlinesRule},
	TlinesRule=(l_->T_):>(l->{Dashed,l/.{"Tc"->Red,"Tn"->Blend[{Orange,Red}],"Tp"->Orange},
							InfiniteLine[{{T,0},{T,1}}],
							Text[Style[l/.{"Tc"->"\!\(\*SubscriptBox[\(T\), \(c\)]\)","Tn"->"\!\(\*SubscriptBox[\(T\), \(n\)]\)","Tp"->"\!\(\*SubscriptBox[\(T\), \(p\)]\)"},Medium],{T,1},Scaled@{.5,-.5}]
							});
	STfun=af["Function"];
	TvsST=af["Data"];
	Tmin=TvsST[[All,1]]//Min;
	Tlines=<|Normal[KeyTake[af[Association],{"Tc","Tn","Tp"}]]/.TlinesRule|>;
	PlotAction[STfun,af["Domain"],opt,"Data"->TvsST,"Tlines"->Tlines]
	]


PlotAction[tr_Transition,opt:OptionsPattern[{}]]:=Module[{af,Tlines,TlinesRule},
	TlinesRule=(l_->T_):>(l->{Dashed,l/.{"Tc"->Red,"Tn"->Blend[{Orange,Red}],"Tp"->Orange},
							InfiniteLine[{{T,0},{T,1}}],
							Text[Style[l/.{"Tc"->"\!\(\*SubscriptBox[\(T\), \(c\)]\)","Tn"->"\!\(\*SubscriptBox[\(T\), \(n\)]\)","Tp"->"\!\(\*SubscriptBox[\(T\), \(p\)]\)"},Medium],{T,1},Scaled@{.5,-.5}]
							});
	Tlines=<|Normal[KeyTake[tr[Association],{"Tc","Tn","Tp"}]]/.TlinesRule|>;
	af=tr["ActionFunction"];
	PlotAction[af,opt,"Tlines"->Tlines]
	]


(* ::Subsubsection::Closed:: *)
(*Action Object*)


ActionGraphics[af_ActionFunction|af_Transition]:=Module[{Tc,Tlines,TlinesRule},
	Tc=Lookup[af[Association],"Tc",af["Domain"][[2]]];
	TlinesRule=(l_->T_):>(l->{Dashing[.07],l/.{"Tn"->Blend[{Orange,Red}],"Tp"->Orange},
							InfiniteLine[{{T,0},{T,1}}]
							});
	Tlines=<|"Tc"->{Dashing[.07],Red,InfiniteLine[{{Tc,0},{Tc,1}}]}|>;
	AppendTo[Tlines,<|Normal[KeyTake[af[Association],{"Tn","Tp"}]]/.TlinesRule|>];
	PlotAction[af,
		PerformanceGoal->"Speed",
		Background->White,
		Frame->True,
		FrameTicks->None,
		GridLines->None,
		Axes->None,
		PlotLegends->None,
		AxesLabel->None,
		"Tlines"->Tlines,
		ImageSize->Dynamic[{Automatic,3.5*CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification]}](*PlotStyle->{{Thick},{Thick,Dashed},{Thick,Dashed},{Thick,Dashed}}*)
	]]


ActionFunction::usage="ActionFunction[] object represents results from ActionFit.";
ActionFunction::noprop="Property `1` of \!\(\*
StyleBox[\"ActionFunction\",\nFontWeight->\"Bold\"]\) is not available. It should be one of `2`.";

(* object must by applied to an Association *)
ActionFunction[asc_Association]["Properties"]:=Sort@Keys[asc]

(* Special formats *)
ActionFunction[asc_Association][Dataset]:=Dataset[asc]
ActionFunction[asc_Association][Association]:=asc
ActionFunction[asc_Association][T_?NumericQ]:=asc["Function"][T]
(* lookup value *)
ActionFunction[asc_Association][key_]:=With[{
	value=Lookup[asc,key],
	supported=Sort@Keys[asc]
	},
	(* Check explicitly for Missing["KeyAbsent",_]: other values like Missing["NotAvailable"] could be valid results. *)
	(*If[
		MatchQ[value,Missing["KeyAbsent",_]],
		Message[ActionFunction::noprop,Style[key,ShowStringCharacters->True],supported]
	];*)
	value
];

(* Nice styling of output, see https://mathematica.stackexchange.com/questions/77658 *)
ActionFunction/:MakeBoxes[obj:ActionFunction[asc_Association],form:(StandardForm|TraditionalForm)]:=Module[
	{close,open,icon},
	(* icon *)
	icon = ActionGraphics[obj];
	(* quantities displayed in closed form *)
	close = {
		BoxForm`SummaryItem[{"Type: ", obj["ActionMethod"]}],
		BoxForm`SummaryItem[{"Domain: ", obj["Domain"]}]
	};
	(* additional quantities displayed in open form *)
	open = {
		BoxForm`SummaryItem[{"\!\(\*SubscriptBox[\(T\), \(c\)]\): ", obj["Tc"],obj["Unit"]}],
		BoxForm`SummaryItem[{"\!\(\*SubscriptBox[\(N\), \(points\)]\): ", obj["NActionPoints"]}],
		BoxForm`SummaryItem[{"\!\(\*SubscriptBox[\(\[Sigma]\), \(std\)]\): ", obj["StandardDeviation"]}]
		};
	(* arrange objects *)
	BoxForm`ArrangeSummaryBox[
		ActionFunction, (* head *)
		obj,      (* interpretation *)
		Deploy@icon,    (* icon, use None if not needed *)
		(* close and open must be in a format suitable for Grid or Column *)
		close,    (* always shown content *)
		open,    (* expandable content *)
		form,
		(* "Interpretable"->Automatic works only in version 11.2+ *)
		"Interpretable" -> True
	]
];


(* ::Subsection:: *)
(*Phase Transition Parameters*)


(* ::Subsubsection::Closed:: *)
(*Phase tracing*)


TracePhases::usage="TracePhases[V,\"TRange\"->list] numerically traces the phases of a thermal potential V in the range list.
TracePhases[V,\"TracingMethod\"->NSolve] traces the phases semi-analytically for simple potentials.";
TracePhases::optTrace=msg["optTrace"];
TracePhases::noTRange=msg["noTRange"];
Options[TracePhases]={"TracingMethod"->"Numeric","PlotPhaseDiagram"->True,
	"TRange"->None,"NTracingPoints"->100,
	"SymmetricPhaseThreshold"->1.,"BrokenPhaseScale"->10.^6,
	"ShiftSymmetricPhase"->False,
	ProgressIndicator->True,Print->True
};
TracePhases[V_,opt:OptionsPattern[{TracePhases,Solve,Plot}]]:=Module[{
	phases,\[Phi],Tmin,Tmax,Tlist,n,\[Phi]S,\[Phi]B,\[Phi]th,\[Phi]Bscale,\[Phi]max,
	overlaps,printProg,printTracing,optPlot,
	unboundedV={},
	method=OptionValue["TracingMethod"],
	TRange=OptionValue["TRange"]
	},
	optPlot=Evaluate@FilterRules[{opt},Options[Plot]];
	Switch[method,
		(* analytic methods *)
		Solve|NSolve,
		(* optional progress bar *)
		If[OptionValue[ProgressIndicator],
			printProg=PrintTemporary[
				ProgressIndicator[Appearance->"Indeterminate"],
				"   Determining phase structure"
				]
			];
		(* numerical methods *)
		phases=Function[Global`T,#//Evaluate]&/@(\[Phi]/.method[{\!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Phi]\)]\(V[\[Phi], Global`T]\)\)==0,\!\(
\*SubscriptBox[\(\[PartialD]\), \({\[Phi], 2}\)]\(V[\[Phi], Global`T]\)\)>0,Global`T>0},\[Phi]]),
		"Numeric"|"Semianalytic", (* interpolate between fixed-T minima *)
		If[!MatchQ[TRange,{_?NumericQ,_?NumericQ}],
			Message[TracePhases::noTRange,TRange];
			Return[]
			];
		{Tmin,Tmax}=TRange;
		If[OptionValue[Print],printTracing=PrintTemporary@msg["tracing"]];
		n=OptionValue["NTracingPoints"];
		\[Phi]th=OptionValue["SymmetricPhaseThreshold"];
		\[Phi]Bscale=OptionValue["BrokenPhaseScale"];
		Tlist=Subdivide[Tmin,Tmax,n];
		(* optional progress bar *)
		If[OptionValue[ProgressIndicator],
			printProg=PrintTemporary[
				ProgressIndicator[Dynamic@\[CapitalTau], {Tmin,Tmax}],
				(*Dynamic[StringTemplate["  Evaluating action at T = `1` `2`"][Dynamic@\[CapitalTau],Unit]]*) (* String are not dynamic *)
				"  Minimizing potential at T = ",Dynamic[\[CapitalTau]]," ",Unit
				]
			];
		Switch[method,
			"Semianalytic",
			\[Phi]S=Table[Flatten[{\[CapitalTau],\[Phi]/.#}]&/@NSolve[{\!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Phi]\)]\(V[\[Phi], \[CapitalTau]]\)\)==0,\!\(
\*SubscriptBox[\(\[PartialD]\), \({\[Phi], 2}\)]\(V[\[Phi], \[CapitalTau]]\)\)>0//Simplify,Abs[\[Phi]]<\[Phi]th},\[Phi]],{\[CapitalTau],Tlist}]/.{}->Nothing/.{{x__}}->{x};
			\[Phi]B=Table[Flatten[{\[CapitalTau],\[Phi]/.#}]&/@NSolve[{\!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Phi]\)]\(V[\[Phi], \[CapitalTau]]\)\)==0,\!\(
\*SubscriptBox[\(\[PartialD]\), \({\[Phi], 2}\)]\(V[\[Phi], \[CapitalTau]]\)\)>0//Simplify,\[Phi]>\[Phi]th},\[Phi]],{\[CapitalTau],Tlist}]/.{}->Nothing/.{{x__}}->{x},
			"Numeric",
			(* numerically minimize the real part of the potential () *)
			\[Phi]S=Table[Quiet[Check[{\[CapitalTau],\[Phi]}/.FindMinimum[Re@V[\[Phi],\[CapitalTau]],{\[Phi],0.1\[Phi]th,-\[Phi]th,\[Phi]th}][[2]],Nothing,FindMinimum::reged],
				FindMinimum::reged],{\[CapitalTau],Tlist}];
			\[Phi]B=Table[Quiet[Check[
				Check[{\[CapitalTau],\[Phi]}/.FindMinimum[Re@V[\[Phi],\[CapitalTau]],{\[Phi],\[Phi]Bscale,Sequence@@NumericalSort[Sign[\[Phi]Bscale]{\[Phi]th,\[Infinity]}]}][[2]],
				AppendTo[unboundedV,\[CapitalTau]];Nothing,FindMinimum::cvmit], (* filter for unbounded potential (or not converged) *)
				Nothing,FindMinimum::reged], (* filter by border field value (solution at lies outside) *)
				{FindMinimum::reged,FindMinimum::cvmit}],{\[CapitalTau],Tlist}];
			If[unboundedV!={},Print@msg["UnboundedV"][MinMax[unboundedV]//Union]];
			(* filter out remaining \[Phi]th values (from FindMinimum::lstol error, if any) *)
			\[Phi]S=DeleteCases[\[Phi]S,{_,\[Phi]_}/;Abs[\[Phi]-\[Phi]th]<0.1 \[Phi]th];
			\[Phi]B=DeleteCases[\[Phi]B,{_,\[Phi]_}/;Abs[\[Phi]-\[Phi]th]<0.1 \[Phi]th];
			(* check potential is real at the phases *)
			If[!FreeQ[V@@@Reverse[\[Phi]S,2],_Complex],Print@msg["Vcomplex"]["symmetric"]];
			If[!FreeQ[V@@@Reverse[\[Phi]B,2],_Complex],Print@msg["Vcomplex"]["broken"]];
			];
		Which[\[Phi]S=={},Print["No symmetric phase."],\[Phi]B=={},Print["No broken phase."]];
		phases={\[Phi]S,\[Phi]B}/.{}->Nothing;
		(* shift symmetric phase, which might result in divergences *)
		If[OptionValue["ShiftSymmetricPhase"],
			Print["!! Phase shift obsolete: use Check[Veff,Limit[Veff,\[Phi]\[Rule]0.] instead !!"];
			(*\[Phi]max=\[Phi]/.Echo[MinimalBy[FindMaximum[V[\[Phi],#[[1]]],{\[Phi],#[[2]]}]&/@\[Phi]S,Abs[First#]&],"Subscript[\[Phi], max]"][[2]];*)
			\[Phi]S=#+{0,10^-6(*\[Phi]max[[1]]*)Min[Abs[\[Phi]B[[All,2]]]]}&/@\[Phi]S];
		(* interpolation: avoid extrapolation *)
		phases=Interpolation[#,"ExtrapolationHandler"->{Indeterminate &,"WarningMessage"->False}]&/@phases,
		True,
		Message[TracePhases::optTrace,method]
		];
	If[OptionValue[Print],NotebookDelete[printTracing]];
	If[OptionValue[ProgressIndicator],NotebookDelete[printProg]];
	(* plot phase diagram (optional) *)
	If[OptionValue["PlotPhaseDiagram"],Print@PlotPhases[phases,optPlot]];
	phases
	]
AutoComplete[TracePhases];


PlotPhases::usage="PlotPhases[{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)}] plots the phases \!\(\*SubscriptBox[\(\[Phi]\), \(1, 2\)]\).
PlotPhases[Transition] plots the phase of the Transition object.";
Options[PlotPhases]={Range->Automatic};
PlotPhases[phases_List,opt:OptionsPattern[{PlotPhases,Plot}]]:=Module[{Tmin,Tmax,Tscale=0.1,optPlot},
	optPlot=Evaluate@FilterRules[{opt},Options[Plot]];
	{Tmin,Tmax}=OptionValue[Range]/.Automatic->MinMaxPhases[phases,Scaled[Tscale]];
	Plot[Through[phases[T]]//Evaluate,{T,Tmin,Tmax},optPlot//Evaluate,
		PlotLabel->"Phase diagram",
		PlotStyle->ColorData[89],
		AxesLabel->{"T / "<>Unit,"\!\(\*SubscriptBox[\(\[Phi]\), \(min\)]\) / "<>Unit}
		]
	]
PlotPhases[Tr_Transition,opt:OptionsPattern[{PlotPhases,Plot}]]:=PlotPhases[Tr["Phases"],opt]


(* ::Subsubsection::Closed:: *)
(*Phases overlap*)


Overlap::usage="Overlap[{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)}] searches for temperature-ranges where 2 or more input phases coexist.";
Options[Overlap]={RegionBounds->True};
Overlap[phases_,OptionsPattern[]]:=Module[{res},
	Off[Reduce::ratnz]; (* Reduce complaints with Real number input *)
	res=If[OptionValue[RegionBounds],RegionBounds[ImplicitRegion[#,T]]//Flatten,Identity[#]]&@
		Reduce[
			If[Head[#]===InterpolatingFunction,
				Between[T,#[[1]]],            (* if InterpolationFunction *)
				FunctionDomain[#[T],T]]&    (* if Function or Symbol *)
			/@phases,T];
	On[Reduce::ratnz];
	res
	]


MinMaxPhases[phases_List,opt___]:=Module[{overlaps},
	overlaps=Overlap/@Subsets[phases,{2}];
	MinMax[overlaps//Flatten,opt]
	]


(* ::Subsubsection:: *)
(*Tc - critical temperature*)


(* find critical temperature *)
FindTcrit::usage="FindTcrit[V,{Subscript[\[Phi], 1],Subscript[\[Phi], 2]}] searches for critical temperatures between the phases \!\(\*SubscriptBox[\(\[Phi]\), \(1, 2\)]\)(T).";
FindTcrit::noOverlap="No overlap between the phases `2` of the potential `1`";
FindTcrit::empty="No transition between the phases `2` of the potential `1`";
FindTcrit::multiTc="Multiple transitions between the phases `2` of the potential `1`";
FindTcrit[V_,phases_]:=Module[{sol,phasesTOverlap,Tguess,\[CapitalDelta]V},
	(* overlap region between the 2 phases *)
	phasesTOverlap=Overlap[phases];
	If[phasesTOverlap=={},Message[FindTcrit::noOverlap,V,phases];sol=None,
		(* If overlap between phases, solve V(T,\[Phi]1)=V(T,\[Phi]2) for T *)
		\[CapitalDelta]V[T_]:=Subtract@@(V[#[T],T]&/@phases);
		Tguess=phasesTOverlap//Mean;
		sol=Check[
			T/.FindRoot[\[CapitalDelta]V[T],{T,Tguess,Sequence@@phasesTOverlap}],(*NSolve[{\[CapitalDelta]V[T]==0,T>0},T]*)
			None,{FindRoot::reged,FindRoot::nlnum} (* FindRoot returns the last iteration: instead, return empty solution if reged message appears *)
		];
		(*DeleteCases[sol,t_->(x_/;x==phasesTOverlap[[1]] \[Or] x==phasesTOverlap[[2]])]; (* FindRoot might return edges with no warning *)*)
		Which[sol==None,Message[FindTcrit::empty,V,phases],
			Length[sol]>1,Message[FindTcrit::multiTc,V,phases](* NB FindRoot is a local solver: no multiple solutions! *)]
		];
	sol
	]


(* ::Subsubsection::Closed:: *)
(*Tn - nucleation temperature*)


(* find the nucleation temperature *)
FindTnuc::usage="FindTnuc[V,{Subscript[T, min],Subscript[T, max]},{Subscript[\[Phi], 1],Subscript[\[Phi], 2]}] searches for a nucleation temperature in the given range and for the given phases.";
Options[FindTnuc]={
	"NucleationCriterion"->"GammaOverH4","NucleationMethod"->"Bisection",
	AccuracyGoal->1,PrecisionGoal->\[Infinity],Return->"TnRule",
	ProgressIndicator->True,Print->False
	};
(*SetOptions[FindTnuc,{"FieldPoints"->201}];*)
FindTnuc[V_,{Tmin_,Tmax_},phases_,opt:OptionsPattern[{FindTnuc,Action,Bisection,FindRoot}]]:=Module[{
	nucleationFun,phasesTOverlap,Tlow,TnRule,Tn,nucFun,
	printProg,optST,optBis0,optBis,criterions,intMethods,
	method=OptionValue["NucleationMethod"],nucleationCrit=OptionValue["NucleationCriterion"]
	},
	optST=Evaluate@FilterRules[{opt}~Join~Options[FindTnuc]~Join~Options[Action],Options[Action]]//DeleteDuplicatesBy[#,First]&;
	(* check input options *)
	criterions={"GammaOverH4","IntegralGammaOverH4",{"ActionValue"->_}};
	intMethods={"ActionFunction"->"","Tc"->"","TnEst"->""};
	If[NoneTrue[criterions,MatchQ[nucleationCrit,#]&],Print@msg["noTnCriterion"][nucleationCrit,criterions];Return["Tn"->Indeterminate]];
	If[nucleationCrit=="IntegralGammaOverH4" \[And] (intMethods=Select[intMethods,!MemberQ[Keys@method,First[#]]&];intMethods=!={}),
		Print@msg["missingIntGammaMethod"][intMethods];
		Return["Tn"->Indeterminate]
		];
	(* initialize *)
	If[OptionValue[Print],Print["Computing nucleation temperature via ",
		nucleationCrit/.{"IntegralGammaOverH4":>"\[Integral]dT/T \[CapitalGamma]/\!\(\*SuperscriptBox[\(H\), \(4\)]\)\[TildeTilde]1","GammaOverH4":>"\[CapitalGamma]/\!\(\*SuperscriptBox[\(H\), \(4\)]\)\[TildeTilde]1",{"ActionValue"->target_}:>("\!\(\*SubscriptBox[\(S\), \(3\)]\)/T\[TildeTilde]"<>ToString[target])},
		" criterion and ",
		method/.{{"ActionFunction"->STfun_,___}:>"action fit"},
		" method..."]
		];
	If[OptionValue[ProgressIndicator],printProg=PrintTemporary[ProgressIndicator[Appearance->"Indeterminate"],"  Estimating \!\(\*SubscriptBox[\(T\), \(n\)]\)"]];
	phasesTOverlap=Overlap[phases];
	Tlow=Max[Tmin,phasesTOverlap[[1]]];
	(* pick criterion *)
	nucleationFun=nucleationCrit/.{
		"IntegralGammaOverH4":>(Log[Int\[CapitalGamma]OverH4[#]]&),
		"GammaOverH4":>(LogR[\[CapitalGamma]OverH4[#]]&),
		{"ActionValue"->target_}:>(STfun[#]-target&)
		};
	(* pick method *)
	nucleationFun=nucleationFun/.(method/.{
		"Bisection"->{\[CapitalGamma]OverH4[x_]:>\[CapitalGamma]OverH4[x,V,phases,optST],STfun[x_]:>Action[x,V,phases,optST]},
		FindRoot->{\[CapitalGamma]OverH4[x_]:>\[CapitalGamma]OverH4[x,V,phases,optST],STfun[x_]:>Action[x,V,phases,optST]},
		{"ActionFunction"->STfun_}->{\[CapitalGamma]OverH4[x_]:>\[CapitalGamma]OverH4[x,STfun,V,phases,optST]},
		{"ActionFunction"->STfun_,___,"Tc"->Tc_,___}->{Int\[CapitalGamma]OverH4[x_]:>Int\[CapitalGamma]OverH4[x,Tc,STfun,V,phases,optST]}
		(* Tc must be provided in NucleationMethod *)
		});
	If[method=="Bisection",
		optBis0=Evaluate@FilterRules[{opt},Options[Bisection]]//DeleteDuplicatesBy[#,First]&;
		optBis=Sequence@@{optBis0,"Ascending"->(nucleationCrit/.{"GammaOverH4"->False,{"ActionValue"->target_}->True}),
							Direction->Up(*(nucleationCrit/.{"GammaOverH4"->Up,{"ActionValue"->target_}->Up})*)
							}
	];
	(* solve for nucleation temperature *)
	Tn=method/.{
		"Bisection":>(Bisection[nucleationFun,{Tlow,Tmax},10^-4(Tmax-Tlow),1.,Return->"x",optBis]),
		{"ActionFunction"->_}:>(T/.First@FindRoot[nucleationFun[T],{T,Mean@{Tlow,Tmax},Tlow,Tmax},
			AccuracyGoal->OptionValue[AccuracyGoal],PrecisionGoal->OptionValue[PrecisionGoal]]),
		{"ActionFunction"->_,___,"TnEst"->Tn0_,___}:>(T/.First@FindRoot[nucleationFun[T],{T,Tn0,Tlow,Tmax},
			AccuracyGoal->OptionValue[AccuracyGoal],PrecisionGoal->OptionValue[PrecisionGoal]]),
		FindRoot:>(T/.First@FindRoot[nucleationFun[T],{T,Mean@{Tlow,Tmax},Tlow,Tmax},
			AccuracyGoal->OptionValue[AccuracyGoal],PrecisionGoal->OptionValue[PrecisionGoal](*,DampingFactor->2*)])
		};
	If[OptionValue[ProgressIndicator],NotebookDelete[printProg]];
	(* return result *)
	If[!NumericQ[Tn],Print@msg["NoTnuc"]];
	If[Tn==Tlow,Print@msg["NoTnuc"];Tn=Indeterminate];
	TnRule="T"->Tn;
	OptionValue[Return]/.{
		"TnRule":>TnRule,"STnRule":>("ActionValue"->If[NumericQ@Tn,Action[Tn,V,phases,optST],None]),
		"Tn":>Tn,
		"NucleationAction":>If[NumericQ@Tn,Action[Tn,V,phases,optST],None],
		"NucleationGammaOverH4":>If[NumericQ@Tn,\[CapitalGamma]OverH4[Tn,V,phases,optST],None],
		"NucleationIntegralGammaOverH4":>Module[{STfun,Tc},
			{STfun,Tc}={"ActionFunction","Tc"}/.method;
			If[NumericQ@Tn,Int\[CapitalGamma]OverH4[Tn,Tc,STfun,V,phases,optST],None]
			]
		}
	]
AutoComplete[FindTnuc,{FindTnuc,Action,Bisection,FindRoot}];


(* ::Subsubsection::Closed:: *)
(*Tp - percolation temperature*)


(* ::Text:: *)
(*Percolation temperature*)


FindTperc::usage="FindTperc[ActionFunction,{\!\(\*SubscriptBox[\(T\), \(pGuess\)]\),\!\(\*SubscriptBox[\(T\), \(c\)]\)}] searches for a percolation temperature for the given Euclidean action function Subscript[S, 3](T)/T.
FindTperc[ActionFunction,{\!\(\*SubscriptBox[\(T\), \(pGuess\)]\),\!\(\*SubscriptBox[\(T\), \(c\)]\)},V,{Subscript[\[Phi], 1],Subscript[\[Phi], 2]}] includes the vacuum energy \[CapitalDelta]V in the Hubble parameter.";
Options[FindTperc]={Method->FindRoot,"Target"->0.34,"TRange"->{},\[Epsilon]->0.01,Return->"TpRule",Print->False,ProgressIndicator->True};
FindTperc::method="The Method `1` is not FindRoot or \"Bisection\".";
FindTperc::TRange="Invalid option \"TRange\"\[Rule]`1`. With Method\[Rule]\"Bisection\", a valid temperature range must be provided.";
FindTperc[STfun_,{TpGuess_,Tc_},vw_,V_:None,phases_:None,opt:OptionsPattern[{FindTperc,FindRoot,Bisection}]]:=Module[{
	Tp,TpFun,TpRule,int,Trange,Tmin,Tmax,printProg,simpleInt,
	target=OptionValue["Target"],method=OptionValue[Method],
	optBis
	},
	optBis=Evaluate@FilterRules[{opt}~Join~Options[FindTperc]~Join~Options[Bisection],Options[Bisection]]//DeleteDuplicatesBy[#,First]&;
	(* initialize *)
	If[OptionValue[ProgressIndicator],printProg=PrintTemporary[ProgressIndicator[Appearance->"Indeterminate"],"  Estimating \!\(\*SubscriptBox[\(T\), \(p\)]\)"]];
	(* select percolation function *)
	If[V=!=None,
		(* full fractional volume I_\[ScriptCapitalF]\[Congruent]-ln(P_\[ScriptCapitalF]) (double integral) *)
		simpleInt=False;
		TpFun[t_?NumericQ]:=IFV[STfun,t,Tc,vw,V,phases],
		(* simplified expression (single integral) *)
		simpleInt=True;
		TpFun[t_]:=IFV[STfun,t,Tc,vw]
	];
	(* select method *)
	Switch[method,
		FindRoot, (* standard resolution with FindRoot *)
			Print[msg["searchingTp"][<|"simple"->If[simpleInt,"simplified ",""],"target"->target,"method"->findRootLink|>]];
			Tp=(\[ScriptCapitalT]/.First@FindRoot[Log[TpFun[\[ScriptCapitalT]]]==Log[target],{\[ScriptCapitalT],TpGuess}]), (* NB Sensitive to variable name !!! *)
		"Bisection", (* alternative bisection method *)
			Print[msg["searchingTp"][<|"simple"->If[simpleInt,"simplified ",""],"target"->target,"method"->"bisection"|>]];
			Trange=OptionValue["TRange"]//Echo; (* in principle we should extract the T range from the action function *)
			If[Length[Trange]=!=2,Message[FindTperc::TRange,Trange];Return[]];
			Tp=Bisection[Log[TpFun[#]]-Log[target]&,Trange,10^-4(Trange[[2]]-Trange[[1]]),.1,Return->"x",optBis],
		_,
			Message[FindTperc::method,method];
			Return[]
		];
	If[OptionValue[ProgressIndicator],NotebookDelete[printProg]];
	If[!NumericQ[Tp],Print@msg["NoTperc"]];
	TpRule=T->Tp;
	OptionValue[Return]/.{"TpRule":>TpRule,"Tp":>Tp,"Value":>TpFun[Tp],"Action":>STfun[Tp]}
]


(* check on percolation *)
PercolationCheck::usage="PercolationCheck[ActionFunction,T,\!\(\*SubscriptBox[\(T\), \(c\)]\),\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\),V,phases] returns a negative number if percolation effectively occured, accounting for the Universe expansion.";
PercolationCheck[Action_,T_,Tc_,vw_,V_,phases_,opt:OptionsPattern[]]:=
H[T,V,phases](3+T ND[IFV[Action,t,Tc,vw,V,phases],t,T,opt])


(* ::Subsubsection::Closed:: *)
(*\[Alpha] - strength*)


\[Alpha]Fun::usage="\[Alpha]Fun[T,Potential,Phases] gives the strength of a first order phase transition (NB: Uses numerical derivatives).";
\[Alpha]Fun[T_,V_,phases_]:=Module[{xm1,xm2,x,t},
	{xm1,xm2}=Through[phases[T]];
	1/Subscript[\[Rho], \[Gamma]][T] (V[xm1,T]-V[xm2,T] - T/4(ND[V[xm1,t],t,T]-ND[V[xm2,t],t,T]))(*(#/.x->xm1)-(#/.x->xm2)&@(\!\(
\*SubscriptBox[\(\[PartialD]\), \(t\)]\(V[x, t]\)\)/.t->T)))*)
	]


(* ::Subsubsection::Closed:: *)
(*\[Beta] (inverse duration)*)
(*Inverse duration of the PT*)


\[Beta]HFun::usage="\[Beta]HFun[T,ActionFunction] gives the inverse duration in Hubble units of a first-order phase transition.";
\[Beta]HFun[T_,Action_Function|Action_Symbol]:=(\[ScriptCapitalT] \!\(
\*SubscriptBox[\(\[PartialD]\), \(\[ScriptCapitalT]\)]\(Action[\[ScriptCapitalT]]\)\))/.\[ScriptCapitalT]->T


(* ::Subsection:: *)
(*GW Plot*)


(* ::Subsubsection:: *)
(*Detector Sensitivities Plot*)


PlotGWSensitivities[{fmin_,fmax_},detectors_List,opt:OptionsPattern[]]:=Module[{colors=\!\(TraditionalForm\`ColorData[68, "\<ColorList\>"]\)},
	LogLogPlot[PISC[f,#]&/@detectors//Evaluate,{f,fmin,fmax},
		opt,
		PlotStyle->If[Length[detectors]>1,colors,colors[[1]]],
		Filling->Top,
		GridLines->Automatic,
		PlotLegends->SwatchLegend@detectors,
		AxesLabel->{"f / Hz","\!\(\*SuperscriptBox[\(h\), \(2\)]\)\!\(\*SubscriptBox[\(\[CapitalOmega]\), \(GW\)]\)"}]/._Line:>Sequence[]
	]


(* ::Subsubsection:: *)
(*GW plot*)


(* given a function h2\[CapitalOmega](f) *)
PlotGW::usage="PlotGW[\"\!\(\*SuperscriptBox[\(h\), \(2\)]\)\[CapitalOmega]\"] plots the gravitational wave spectra for the function \"\!\(\*SuperscriptBox[\(h\), \(2\)]\)\[CapitalOmega]\".
PlotGW[Transition] plots the gravitational wave spectra for the Transition object.";
Options[PlotGW]={
	"FrequencyRange"->Automatic,"h2OmegaRange"->Automatic,
	"Sources"->{"Collisions","Soundwaves","Turbulence"},
	"Detectors"->{"LISA","DECIGO","BBO"},
	"GWPeak"->True
	};
PlotGW[h2Omega_Association,opt:OptionsPattern[{PlotGW,Plot}]]:=Module[{
	h2\[CapitalOmega]=h2Omega, (* make copy *)
	fPeak,\[CapitalOmega]Peak,fmin,fmax,h2\[CapitalOmega]Range,
	minSens,detsPlot,
	dets=OptionValue["Detectors"],
	sources=Flatten[{OptionValue["Sources"]}]\[Intersection]Keys[h2Omega]
	},
	(* handle multiple sources *)
	If[Length@sources>1,
		h2\[CapitalOmega]["Combined"]=Function[f,Total@Lookup[h2\[CapitalOmega],sources,Nothing,#[f]&]//Simplify//Evaluate];
		PrependTo[sources,"Combined"],
		h2\[CapitalOmega]["Combined"]=h2\[CapitalOmega][First@sources]
		];
	(* SGWB peak *)
	{fPeak,\[CapitalOmega]Peak}=Module[{logf},E^{logf/.#[[2]],#[[1]]}&@NMaximize[{Log[h2\[CapitalOmega]["Combined"][E^logf]]},logf,Method->"NelderMead"]];
	(* ranges *)
	{fmin,fmax}=OptionValue["FrequencyRange"]/.Automatic->10^{-5,5}fPeak;
	h2\[CapitalOmega]Range=OptionValue["h2OmegaRange"]/.Automatic->10^{-10,5}\[CapitalOmega]Peak;
	(* plot *)
	With[{optPlot=Evaluate@FilterRules[{opt}~Join~Options[PlotGW],Options[LogLogPlot]]},
	Show[{
		LogLogPlot[Lookup[h2\[CapitalOmega],sources,Nothing,#[f]&]//Evaluate,{f,fmin,fmax},
			optPlot,
			AxesLabel->{"f (Hz)","\!\(\*SuperscriptBox[\(h\), \(2\)]\)\!\(\*SubscriptBox[\(\[CapitalOmega]\), \(GW\)]\)"},
			GridLines->Automatic,
			PlotLegends->If[Length[sources]>1,sources,None],
			PlotRange->h2\[CapitalOmega]Range,
			PlotStyle->(sources/.{"Combined":>Dashing[None],x_String->Dashed}),
			Epilog->If[OptionValue["GWPeak"],
				With[{peak=Log@{fPeak,\[CapitalOmega]Peak}},{Red,Gray,Dashed,HalfLine[peak,{-1,0}],HalfLine[peak,{0,-1}]}],
				None]
			],
		(* include detector sensitivities (optional) *)
		(*minSens=AssociationThread[dets->
			NMinimize[{Log@PISC[\[ExponentialE]^logf,#],Between[logf,Log@{fmin,fmax}]},logf,Method->"NelderMead"]&/@dets
			]; (* minimum values of detector sensitivities *)
		detsPlot=Keys@Select[minSens,#>h2\[CapitalOmega]Range[[2]]&]; (* select only detectors entering the plot (for the legend) *)*)
		If[MatchQ[dets,_List],PlotGWSensitivities[{fmin,fmax},dets,optPlot],Nothing]
		}]]
	]
AutoComplete[PlotGW];


(* given a Transition object *)
PlotGW[tr_Transition,opt:OptionsPattern[]]:=Module[{h2Omega},
	h2Omega=Lookup[tr[Association],"h2Omega",
		Print["Missing h2Omega!"];Return[$Failed,Module]];
	PlotGW[h2Omega,opt]
	]


(* ::Subsection:: *)
(*SearchPotential*)


(* ::Subsubsection::Closed:: *)
(*PlotTransition*)


PlotTransition::usage="PlotTransition[T,{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)}] plots the diagram of the phases \!\(\*SubscriptBox[\(\[Phi]\), \(1, 2\)]\), with an arrow indicating the transition temperature.
PlotTransition[Transition] plots a phase diagram for the Transition object.";
PlotTransition[T_?NumericQ,phases_List,opt:OptionsPattern[]]:=Module[{},
	PlotPhases[phases,opt,Epilog->Arrow[{T,#[T]}&/@phases]]
	]
PlotTransition[{Tn_?NumericQ,Tp_?NumericQ},phases_List,opt:OptionsPattern[]]:=Module[{},
	PlotPhases[phases,opt,Epilog->{Arrowheads[Medium],Arrow[{{Tn,phases[[1]][Tn]},{Tp,phases[[2]][Tp]}}]}]
	]
PlotTransition[tr_Transition,opt:OptionsPattern[]]:=Module[{Tn,Tp,phases},
	{Tn,Tp}=Lookup[tr[Association],{"Tn","Tp"},tr["Tn"]];
	phases=tr["Phases"];
	PlotTransition[{Tn,Tp},phases,opt]
	]


(* ::Subsubsection::Closed:: *)
(*Transition object*)


TransitionGraphics[tr_Transition]:=PlotTransition[tr,
	PerformanceGoal->"Speed",
	Background->White,
	Axes->None,
	PlotLabel->None,
	PlotLegends->{},
	Epilog->Module[{Tn,Tp,phases},{Tn,Tp,phases}=Lookup[tr[Association],{"Tn","Tp","Phases"},tr["Tn"]];
		{Arrowheads[Small],Arrow[{{Tn,phases[[1]][Tn]},{Tp,phases[[2]][Tp]}}]}],
	AxesLabel->None,
	ImageSize->Dynamic[{Automatic,3.5*CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification]}]
	]


GWGraphics[tr_Transition]:= PlotGW[tr,
	PerformanceGoal->"Speed",
	Background->White,
	GridLines->None,
	Axes->None,
	PlotLegends->None,
	Epilog->{},
	PlotLegends->{},
	AxesLabel->None,
	ImageSize->Dynamic[{Automatic,3.5*CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification]}](*PlotStyle->{{Thick},{Thick,Dashed},{Thick,Dashed},{Thick,Dashed}}*)
	(*ImagePadding->{{40,50},{20,30}}*)
	]


Transition::usage="Transition[] object represents results from the \!\(\*TemplateBox[{\"SearchPhases\", {\"SearchPotential/ref/SearchPhases\", None}, \"SearchPotential/ref/SearchPhases\"},\n\"HyperlinkDefault\"]\) function."
Transition::noprop="Property `1` of Transition is not available. It should be one of `2`.";

(* object must by applied to an Association *)
Transition[asc_Association]["Properties"]:=Sort@Keys[asc]

(* lookup value *)
Transition[asc_Association][key_]:=With[{
	value=Lookup[asc,key],
	supported=Sort@Keys[asc]
	},
	(* Check explicitly for Missing["KeyAbsent",_]: other values like Missing["NotAvailable"] could be valid results. *)
	(*If[
		MatchQ[value,Missing["KeyAbsent",_]],
		Message[Transition::noprop,Style[key,ShowStringCharacters->True],supported]
	];*)
	value
];

(* Dataset format *)
Transition[asc_Association][Dataset]:=Dataset[asc]
Transition[asc_Association][Association]:=asc
Transition[asc_Association][Keys]:=Keys[asc]
Transition[ds_Dataset]:=Transition[ds//Normal]

(* Nice styling of output, see https://mathematica.stackexchange.com/questions/77658 *)
Transition/:MakeBoxes[obj:Transition[asc_Association],form:(StandardForm|TraditionalForm)]:=Module[
	{close,open,icon},
	(* autocomplete Transition keys *)
	AutoComplete[obj];
	(* icon *)
	icon = Which[
		(*KeyExistsQ[asc,"h2OmegaPeak"],Deploy@GWGraphics[obj],*)
		(*KeyExistsQ[asc,"ActionFunction"],Deploy@ActionGraphics[obj],*)
		KeyExistsQ[asc,"Phases"],Deploy@TransitionGraphics[obj],
		True,None
		];
	(* quantities displayed in closed form *)
	close = {
		BoxForm`SummaryItem[{"\[Alpha]: ", Lookup[asc,"\[Alpha]",None]}],
		BoxForm`SummaryItem[{"\[Beta]/H: ", Lookup[asc,"\[Beta]/H",None]}]
	};
	(* additional quantities displayed in open form *)
	open = {
		BoxForm`SummaryItem[{"\!\(\*SubscriptBox[\(T\), \(p\)]\): ", Lookup[asc,"Tp",None,Quantity[#,obj["Unit"]]&]}],
		BoxForm`SummaryItem[{"\!\(\*SubsuperscriptBox[\(f\), \(GW\), \(peak\)]\): ", Lookup[asc,"fPeak",None,Quantity[#,"Hz"]&]}],
		BoxForm`SummaryItem[{"\!\(\*SuperscriptBox[\(h\), \(2\)]\)\!\(\*SubsuperscriptBox[\(\[CapitalOmega]\), \(GW\), \(peak\)]\): ", Lookup[asc,"h2OmegaPeak",None]}]
		(*,
		BoxForm`SummaryItem[{"Subscript[T, n]: ", obj["Tn"],obj["Unit"]}],
		BoxForm`SummaryItem[{"Subscript[S, 3]/T(Subscript[T, n]): ", obj["NucleationAction"]}],
		BoxForm`SummaryItem[{"\[CapitalGamma]/H^4(Subscript[T, n]): ", obj["NucleationGammaOverH4"]}],
		BoxForm`SummaryItem[{"Subscript[S, 3]/T(Subscript[T, p]): ", obj["PercolationAction"]}],		
		BoxForm`SummaryItem[{"f: ", obj["PathIterations"]}],
		BoxForm`SummaryItem[{"h^2Subscript[\[CapitalOmega], GW]: ", Length@obj["Path"]}]*)
		};
	(* arrange objects *)
	BoxForm`ArrangeSummaryBox[
		Transition, (* head *)
		obj,      (* interpretation *)
		icon,    (* icon, use None if not needed *)
		(* close and open must be in a format suitable for Grid or Column *)
		close,    (* always shown content *)
		open,    (* expandable content *)
		form,
		(* "Interpretable"->Automatic works only in version 11.2+ *)
		"Interpretable" -> Automatic
	]
];


(* ::Subsubsection:: *)
(*SearchPhases*)


SearchPhases::usage="SearchPhases[V,{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)},\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\)] finds first order phase transitions(if any) and derive associated gravitational wave spectra from a given particle physics potential V and two phases Subscript[\[Phi], 1,2](T).";
Options[SearchPhases]={
	"\[CapitalDelta]TFraction"->0.8,
	"STanalytic"->None,"analyticTn"->False,"analyticTp"->False,
	"NucleationMethod"->"Bisection",
	"CollisionData"->None,
	"Metadata"->{},
	UpTo->All,
	Print->True,"PlotGW"->False};
functionList={SearchPhases,ActionFit,FindTnuc,Bisection,ComputeSGWB};
SearchPhases[V_Function|V_Symbol,phases_List,vw_?NumericQ,opt:OptionsPattern[functionList]]:=Module[{
	phasesTOverlap,Tlow,Tup,  (* temperature phase overlap *)
	Tc,Tn,Tp,T0,
	\[Alpha],\[Kappa],K,Kstr,\[Beta]H,HR,h2\[CapitalOmega],fPeak,\[CapitalOmega]Peak,
	TcRule,Tn0,STn0,\[CapitalGamma]H40,STn,\[CapitalGamma]H4,Int\[CapitalGamma]H4,Ip,STp,
	actionFun,STfun,TvsST,STlist,STanalytic,optActionFit,optFindTnuc,optGW,(*STanalytic,*)
	\[CapitalDelta]T,Tmin,Tmax,TpCond,
	targetForms,computations,res,out,
	colData=OptionValue["CollisionData"],
	\[CapitalDelta]Tfrac=OptionValue["\[CapitalDelta]TFraction"],
	metadata=OptionValue["Metadata"],
	printOutput=OptionValue[Print]
	},
	optActionFit=Evaluate@FilterRules[{opt}~Join~Options[ActionFit],Options[ActionFit]]//DeleteDuplicatesBy[#,First]&;
	optFindTnuc=Evaluate@FilterRules[{opt}~Join~Options[SearchPhases]~Join~Options[FindTnuc],
		Options[FindTnuc]~Join~Options[Bisection]]//DeleteDuplicatesBy[#,First]&;
	optGW=Evaluate@FilterRules[{opt},Options[ComputeSGWB]]//DeleteDuplicatesBy[#,First]&;
	(* preliminary tests *)
	If[!Between[vw,{0,1}],Print[msg["vw"][vw]];Return[<||>]];
	
	(* check: computation result must match these forms *)
	targetForms={
		"Unit" -> _String, "gstar" -> _?NumericQ, "Phases" -> _List, "vw" -> _Real,
		"Tc" -> _Real, "TnEst" -> _Real, "ActionFunction" -> _ActionFunction|_Function|_Symbol,
		"Tn" -> _Real, "Tp" -> _Real,"Domain" -> {_?NumericQ,_?NumericQ},
		"\[Alpha]" -> _Real, "K"->_Real, "Kstr"->_Real, "\[Kappa]"->_Real, "\[Beta]/H" -> _Real, "TpCondition" -> _Real,
		"fPeak"->_Real, "h2OmegaPeak"->_Real,
		"NucleationAction" -> _Real, "NucleationGammaOverH4" -> _Real, "NucleationIntegralGammaOverH4"->_Real,
		"PercolationAction" -> _Real, "PercolationIntegralValue" -> _Real
		};
	(* list the computations to perform & check *)
	computations={
		(* input data *)
		"Unit":>(
			(* Metadata Keys must be Strings, for proper Dataset structure *)
			If[!ContainsOnly[Head/@Keys@metadata,{String}],
				Print[msg["MetaKeys"][Cases[Keys@metadata,Except[_String]]]];
				metadata=KeyMap[ToString,Association[metadata]]
				];
			AssociateTo[out,metadata];
			Unit),
		"gstar":>gstar,
		"Phases":>phases,
		"vw":>vw,
		(* Search for phase transitions *)
		"Tc":>(
			Tc=FindTcrit[V,phases];
			If[Tc==None,Throw[out]]; (* Run the following only if critical temperature is found *)
			Print["Found transition at critical temperature"];
			Echo[Tc,"\!\(\*SubscriptBox[\(T\), \(c\)]\)  \[Rule]",Quantity[#,Unit]&]
			),
		(* Tn: analytic vs numerical bisection method *)
		"TnEst":>(
			(* overlap region between the 2 phases *)
			phasesTOverlap=Overlap[phases];
			Tlow=phasesTOverlap[[1]];
			STanalytic=OptionValue["STanalytic"];
			If[OptionValue["analyticTn"],
				(* analytic Tn *)
				Print["Using analytic S/T to determine \!\(\*SubscriptBox[\(T\), \(n\)]\)"];
				{Tn0,STn0,\[CapitalGamma]H40}=FindTnuc[V,{Tlow,Tc},phases,"NucleationMethod"->{"ActionFunction"->STanalytic},
					Return->{"Tn","NucleationAction","NucleationGammaOverH4"}],
				(* numerical Tn (FindRoot) *)
				T0=phasesTOverlap[[1]];
				{Tn0,STn0,\[CapitalGamma]H40}=FindTnuc[V,{Tlow,Tc},phases,
					Return->{"Tn","NucleationAction","NucleationGammaOverH4"},Print->True,optFindTnuc
					];
			];
			If[!NumericQ[Tn0],Throw@out];
			Tn0),
		"ActionFunction":>(
			Echo[msg["Tn"][<|"Tn"->Quantity[Tn0,Unit],"STn"->STn0,"GammaH4"->\[CapitalGamma]H40|>],"\!\(\*SubsuperscriptBox[\(T\), \(n\), \(estimate\)]\)  \[Rule]"];
			(* Action function: analytic vs fitting method *)
			If[OptionValue["analyticTp"],
				(* analytic action function *)
				STfun=STanalytic,
				(* numerical fit of the action function *)
				Print["Fitting action..."];
				\[CapitalDelta]T=\[CapitalDelta]Tfrac(Tc-Tn0);
				Tmin=Tn0-\[CapitalDelta]T;
				Tmax=Tn0+\[CapitalDelta]T;
				(* refine T range *)
				Tlow+=10^-6 Min[\[CapitalDelta]T,Tn0-Tlow]; (* phases are undetermined at Tlow: numerical shift  *)
				Off[Reduce::ratnz];                  (* Reduce complains with Real number input *)
				{Tmin,Tmax}=RegionBounds[ImplicitRegion[Reduce[{Tmin<t<Tmax,t>Tlow},t],t]]//First;
				On[Reduce::ratnz];
				actionFun=ActionFit[V,phases,{Tmin,Tmax},Tc,"PlotAction"->False,optActionFit];
				{STfun,TvsST}=actionFun[{"Function","Data"}];
			];
			Echo[actionFun,"Action function \[Rule]"]),
		"Domain":>{Tmin,Tc}, (* NB Domain doesn't hold for analytic action *)
		"Tn":>(
			(* Tn: analytic vs fitting method *)
			If[OptionValue["analyticTp"],
				(* analytic action function *)
				Tn=Tn0,
				(* action fit: recompute Tn *)
				{Tn,STn,\[CapitalGamma]H4,Int\[CapitalGamma]H4}=FindTnuc[V,{Tmin,Tmax},phases,
					"NucleationCriterion"->"IntegralGammaOverH4",
					"NucleationMethod"->{"ActionFunction"->STfun,"Tc"->Tc,"TnEst"->Tn0},
					Return->{"Tn","NucleationAction","NucleationGammaOverH4","NucleationIntegralGammaOverH4"},Print->True];
				];
			(* nucleation temperature Tn *)
			If[!NumericQ[Tn],Throw@out];
			If[Abs[Log10@Int\[CapitalGamma]H4]>1,Print@msg["NoTnuc"];Throw@out];
			Tn),
		"NucleationAction":>(
			Echo[msg["Tn"][<|"Tn"->Quantity[Tn,Unit],"STn"->STn,"GammaH4"->\[CapitalGamma]H4,"IntGammaH4"->Int\[CapitalGamma]H4|>],"\!\(\*SubscriptBox[\(T\), \(n\)]\)  \[Rule]"];
			STn),
		"NucleationGammaOverH4":>\[CapitalGamma]H4,
		"NucleationIntegralGammaOverH4":>Int\[CapitalGamma]H4,
		"Tp":>(
			Print["Computing phase transition parameters..."];
			{Tp,Ip,STp}=FindTperc[STfun,{Tn,Tc},vw,V,phases,Return->{"Tp","Value","Action"}];
			If[!NumericQ[Tp],Throw@out];
			Echo[Tp,"\!\(\*SubscriptBox[\(T\), \(p\)]\)  \[Rule]",Quantity[#,Unit]&]
			),
		"PercolationAction":>STp,
		"PercolationIntegralValue":>Ip,
		"TpCondition":>(TpCond=PercolationCheck[STfun,Tp,Tc,vw,V,phases,Scale->10.^-3 (Tp-Tmin)];
			Echo[TpCond,"Percolation condition:",Row@{If[#<0,Style["satisfied",Darker@Green],Style["violated",Red]]," (",#,")"}&]
			),
		(* phase transition parameters *)
		"\[Alpha]":>(
			(* optional plot *)
			If[OptionValue["PlotAction"],
				Print@PlotAction[Transition[out]]];
			\[Alpha]=Echo[\[Alpha]Fun[Tp,V,phases],"\[Alpha] \[Rule] "]
			),
		"\[Beta]/H":>(\[Beta]H=Echo[\[Beta]HFun[Tp,STfun],"\[Beta]/H \[Rule] "]),
		(* compute and append GW data *)
		"fPeak":>(
			If[MatchQ[colData,_Association|_List],colData=Association[colData];colData["Potential"]=V];
			AssociateTo[out,ComputeSGWB[out,"CollisionData"->colData,optGW]];
			h2\[CapitalOmega]=out["h2Omega"]; (* h2Omega association must be defined! *)
			If[OptionValue["PlotGW"],Print@PlotGW[h2\[CapitalOmega]]];
			(* GW peak *)
			{fPeak,\[CapitalOmega]Peak}=Module[{logf},E^{logf/.#[[2]],#[[1]]}&@NMaximize[{Log[h2\[CapitalOmega]["Combined"][E^logf]]},logf,Method->"NelderMead"]];
			Echo[fPeak,"\!\(\*SuperscriptBox[\(f\), \(peak\)]\)  \[Rule]",Quantity[#,"Hz"]&]
			),
		"h2OmegaPeak":>Echo[\[CapitalOmega]Peak,"\!\(\*SuperscriptBox[\(h\), \(2\)]\)\!\(\*SubsuperscriptBox[\(\[CapitalOmega]\), \(GW\), \(peak\)]\) \[Rule] "]
		};
	out=<||>;
	out=Catch[
		CheckAbort[
			Do[
				(* perform computation *)
				res=comp/.computations;
				AppendTo[out,comp->res];
				(* throw <||> if no transition *)
				If[comp=="Tc"\[And]res=={},Throw[<||>]];
				(* throw current output if heads don't match *)
				If[KeyExistsQ[targetForms,comp] \[And] Not@MatchQ[out[comp],comp/.targetForms],
					Print[msg["computations"][comp,out[comp],comp/.targetForms]];
					Throw[out]];
				(* throw current output if UpTo option matches the computation label *)
				If[OptionValue[UpTo]==comp,Throw[out]],
				{comp,Keys@computations}
				],
			(* throw current output if evaluation is manually aborted *)
			Print[msg["aborted"]];
			Throw[out]];
		Throw[out]
		];
	(* output Transition object *)
	Which[
		Lookup[out,"Tn",False,NumericQ],Transition[out],
		Lookup[out,"Tc",False,NumericQ],out,
		True,<||>
		]
	]
AutoComplete[SearchPhases,functionList];


(* ::Subsubsection:: *)
(*SearchPotential*)


(* ::Text:: *)
(*PT and GW parameters (full potential)*)


SearchPotential::usage="SearchPotential[V,\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\)] runs the full BSM\[Rule]GW pipeline for a given temperature-dependent scalar potential and bubble wall velocity \!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\).
SearchPotential[V] sets the wall velocity to \!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\)=0.9.";
Options[SearchPotential]={"PrintNoTc"->False,Dataset->False};
functionList={SearchPotential,SearchPhases,TracePhases,ActionFit,FindTnuc,Bisection,ComputeSGWB};
SearchPotential[V_,Optional[vw_/;NumericQ[vw],.9],opt:OptionsPattern[functionList]]:=Module[{
	transitions,trans,xm,xmFuns,phases,
	optTrace,optTrans,optActionFit,optFindTnuc,
	dataset=OptionValue[Dataset]
	},
	(* initialize transitions *)
	transitions=If[dataset,Dataset[{}],{}];
	(* extract options *)
	optTrace=Evaluate@FilterRules[{opt}~Join~Options[TracePhases],Options[TracePhases]]//DeleteDuplicatesBy[#,First]&;
	optTrans=Evaluate@FilterRules[{opt}~Join~Options[SearchPhases],Options[SearchPhases]]//DeleteDuplicatesBy[#,First]&;
	optActionFit=Evaluate@FilterRules[{opt}~Join~Options[ActionFit],Options[ActionFit]]//DeleteDuplicatesBy[#,First]&;
	optFindTnuc=Evaluate@FilterRules[{opt}~Join~Options[FindTnuc],Join@@Options/@{FindTnuc,Bisection}]//DeleteDuplicatesBy[#,First]&;
	phases=TracePhases[V,optTrace,"PlotPhaseDiagram"->False]//Simplify;
	(* only one phase found *)
	If[Length[phases]<2,Print@msg["SinglePhase"];Return[If[dataset,Dataset[{}],{}]]];
	If[\[Not]OptionValue["PrintNoTc"],Off[FindTcrit::empty,FindTcrit::noOverlap]];
	(* loop over pairs of phases *)
	Print@msg["loopPhases"];
	Do[
		trans=SearchPhases[V,phaseCouple,vw,optTrans,optActionFit,optFindTnuc];
		If[MatchQ[trans,_Transition],
			Echo[trans,"Transition \[Rule]"];
			transitions=AppendTo[transitions,If[dataset,trans[Dataset],trans]]
			],
		{phaseCouple,Subsets[phases,{2}]}
		];
	If[Normal@transitions=={},Print@msg["NoTran"]];
	transitions
	]
AutoComplete[SearchPotential,functionList];


(* ::Section::Closed:: *)
(*End Package*)


End[]; (*"`Private`"*)


(* ReadProtected attribute on public symbols prevents rendering of huge box with all 
definitions (DownValues) when they are called in Information or with shortcut ?FindBounce. *)
SetAttributes[Evaluate@Names["`*"],{ReadProtected}]; (* from FindBounce.m *) 


EndPackage[]


Print["Imported \!\(\*
StyleBox[\"TBounce`\",\nFontWeight->\"Bold\"]\) \[Checkmark]"];
