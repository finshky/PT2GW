(* ::Package:: *)

(* ::Section::Closed:: *)
(*Header*)


(* :Title: PT2GWFinder *)
(* :Context: PT2GW` *)
(* :Author: Vedran Brdar, Marco Finetti, Marco Matteini, Ant\[OAcute]nio Morais, Miha Nemev\[SHacek]ek *)
(* :Summary: Search for 1^st-order phase transitions\nand compute gravitational wave spectra. *)
(* :Keywords: bounce, cosmology, tunneling, first order phase transitions, bubble nucleation, gravitational waves, BSM, thermal potential *)


(* ::Section::Closed:: *)
(*Begin Package*)


BeginPackage["PT2GW`",{"FindBounce`","NumericalCalculus`","GW`"}]


(* ::Subsection::Closed:: *)
(*Available public Symbols*)


(* utilities *)
$PT2GWPrint;
DefineUnits;
NewMessageGroup;
Bisection;
MassFunction;
AutoComplete;
UpdateTransition;


(* cosmology *)
RadiationEnergyDensity;
H;
DecayRate;
DecayOverHubble;
IntegralDecay;
IntegralFalseVacuum;


(* phase transition *)
Overlap;
FindCritical;
FindNucleation;
FindPercolation;
PercolationCheck;
Alpha;
BetaHubble;


(* action functions *)
Action;
ActionFit;
ActionRefineInflection;


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
RelativisticDOF;
$Unit; (* energy units *)
$FrequencyUnit; (* frequency unit, must be defined in GW` *)
$PlanckMass;  (* reduced Planck mass *)
$PlanckMassN; (* numerical value of Planck mass *)
PrintPlanckMass;


(* Clear definitions from package symbols in public and private context. *)
(*ClearAll["`*","`*`*"];*) (* clear everything in PT2GW` *)
Unprotect["`*"];
Clear@@DeleteCases[Names@"`*", "$PT2GWPrint"]; (* clear all except $PT2GWPrint *)


(* ::Section:: *)
(*Code*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Utilities*)


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
		If[print,PT2GWEcho[ym,ToString[Round[xm,0.01]]<>" \[Function]"]];
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
     If[print,If[NumericQ[ym y],PT2GWEcho[f[res],ToString[Round[res,0.01]]<>" \[Function]"],PT2GWEcho[Indeterminate,"Result \[Rule] "]]]; (* print last iteration *)
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
LogPTGW::usage="LogPTGW[x] is the analytic version of Log, such that Log[0.]=-\[Infinity].";
LogPTGW[0.]:=-\[Infinity]
LogPTGW[x_]:=Log[x]


(* ::Subsubsection::Closed:: *)
(*Plot Potential*)


PlotPotential::usage="PlotPotential[V,{\!\(\*SubscriptBox[\(\[Phi]\), \(min\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(max\)]\)},T] makes an interactive plot of V vs \[Phi].";
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


(* ::Subsubsection:: *)
(*Printing*)


$PT2GWPrint::usage="Set this variable to False if you want to suppress all information printed by PT2GW.";
If[!ValueQ[$PT2GWPrint],$PT2GWPrint=True]; (* initialize printing if not already set *)
PT2GWPrint:=If[$PT2GWPrint,Print];
PT2GWEcho:=If[$PT2GWPrint===True,Echo,#1&];


PacletFind["PT2GW"][[1]]["Location"]


"FileName"/.NotebookInformation[][[1]]


(*PT2GWPrint["\[Checkmark] Imported \!\(\*
StyleBox[\"PT2GW`\",\nFontWeight->\"Bold\"]\)"];*)
With[{icon=Import[FileNameJoin[{PacletObject["PT2GW"]["Location"],"FrontEnd","icon.png"}]]},
	PT2GWPrint@Panel@Row[{
	Image[icon,ImageSize->Tiny],
	TableForm[Normal@KeyTake[PacletFind["PT2GW"][[1]]["PacletInfo"],{"Name","Version","Description","WolframVersion","Creator"}]/.
		Rule[x_,y_]:>{x<>":",y}]
	},Spacer[20]];]


(* ::Subsubsection:: *)
(*Messagess*)


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
	msgGroup="PT2GW":>{(*FindRoot::reged,*)FindRoot::jsing,FindMinimum::lstol,FindBounce::cvmit,FindBounce::errbc,NSolve::ratnz},
	optMsg=Evaluate@FilterRules[{opt}~Join~Options[NewMessageGroup],Options[NewMessageGroup]]
	},
	NewMessageGroup[msgGroup,optMsg];
	]
NewMessageGroup[];
Off["PT2GW"]; (* turn off messages by default. To turn on, use On["SearchPotential"] *)


(* ::Text:: *)
(*Print messages*)


findRootLink="\!\(\*TemplateBox[{Cell[TextData[\"FindRoot\"]], \"paclet:ref/FindRoot\"},\n\"RefLink\",\nBaseStyle->{\"InlineFormula\"}]\)";


msg["tracing"]="Tracing the phases..";
msg["optTrace"]="Bad option \"TracingMethod\"\[Rule]`1`. Valid options are `2`.";
msg["noTRange"]="Bad temperature range ``.\nWith \"TracingMethod\"\[Rule]\"Numeric\", a temperature range in the form \"TRange\"\[Rule]{\!\(\*SubscriptBox[\(T\), \(min\)]\),\!\(\*SubscriptBox[\(T\), \(max\)]\)} must be provided.";
msg["Vcomplex"]=StringTemplate["Complex potential detected at `` phase!"];
msg["UnboundedV"]=StringTemplate["Solution did not converge for T\[Element]``. Potential might be unbounded from below!"];


msg["actionValue"][asc_Association]:=If[asc["Action/T"],StringTemplate["\!\(\*FractionBox[SubscriptBox[\(S\), \(3\)], \(T\)]\)(``)"],StringTemplate["S(``)"]][asc["T"]]
msg["stopAtFailure"]="Stopping at first action failure (turn off this option with \"StopAtFailure\"\[Rule]False).";
msg["ActionFilter"]=StringTemplate["`1` filtered action points (<* #1/#2//N[#,2]&//PercentForm *>)"];
msg["PhaseShift"]="!! Phase shift obsolete: use Check[Veff,Limit[Veff,\[Phi]\[Rule]0.] instead !!";
msg["analyticAction"]="Using analytic action to determine \!\(\*SubscriptBox[\(T\), \(n\)]\)";


msg["TransitionTemperature"]=StringTemplate["Bad option \"TransitionTemperature\"\[Rule]\!\(\*
StyleBox[\"``\",\nStripOnInput->False,\nLineColor->RGBColor[1, 0, 0],\nFrontFaceColor->RGBColor[1, 0, 0],\nBackFaceColor->RGBColor[1, 0, 0],\nGraphicsColor->RGBColor[1, 0, 0],\nFontColor->RGBColor[1, 0, 0]]\). Transition temperature set to percolation."];
msg["badTnEst"]=StringTemplate["Bad option \"TnEstimate\"\[Rule]\!\(\*
StyleBox[\"`1`\",\nStripOnInput->False,\nLineColor->RGBColor[1, 0, 0],\nFrontFaceColor->RGBColor[1, 0, 0],\nBackFaceColor->RGBColor[1, 0, 0],\nGraphicsColor->RGBColor[1, 0, 0],\nFontColor->RGBColor[1, 0, 0]]\). Estimate must be between 0 and \!\(\*SubscriptBox[\(T\), \(c\)]\) = `2`."];
msg["Tn"][a_Association]:=Row[If[KeyExistsQ[a,#],
	Style[Row[{#/.{"Tn"->"","STn"->Style["\!\(\*SubscriptBox[\(S\), \(3\)]\)/T \[Rule] ",Gray],"GammaH4"->"\[CapitalGamma]/\!\(\*SuperscriptBox[\(H\), \(4\)]\) \[Rule] ","IntGammaH4"->"\!\(\*SubsuperscriptBox[\(\[Integral]\), SubscriptBox[\(T\), \(n\)], SubscriptBox[\(T\), \(c\)]]\)\!\(\*FractionBox[\(\[DifferentialD]T\), \(T\)]\)\!\(\*FractionBox[\(\[CapitalGamma]\), SuperscriptBox[\(H\), \(4\)]]\) \[Rule] "},
	#/.a}],If[#=="Tn",Black,Gray]]]&/@{"Tn","STn","GammaH4","IntGammaH4"},Spacer[20]
	]
nucleationLabels={"IntegralDecay":>"\[Integral]dT/T \[CapitalGamma]/\!\(\*SuperscriptBox[\(H\), \(4\)]\)\[TildeTilde]1","DecayOverHubble":>"\[CapitalGamma]/\!\(\*SuperscriptBox[\(H\), \(4\)]\)\[TildeTilde]1",{"ActionValue"->target_}:>("\!\(\*SubscriptBox[\(S\), \(3\)]\)/T\[TildeTilde]"<>ToString[target])};
methodLabels={{"ActionFunction"->actionFun_,___}:>"action fit"};
msg["ComputingTn"]=StringTemplate["Computing nucleation temperature via <* #1/.nucleationLabels *> criterion and <* #2/.methodLabels *> method..."];
msg["NoTnuc"]="Nucleation not reached!";
msg["noTnCriterion"]=StringTemplate["Bad \"NucleationCriterion\"\[Rule]\"\!\(\*
StyleBox[\"`1`\",\nStripOnInput->False,\nLineColor->RGBColor[1, 0, 0],\nFrontFaceColor->RGBColor[1, 0, 0],\nBackFaceColor->RGBColor[1, 0, 0],\nGraphicsColor->RGBColor[1, 0, 0],\nFontColor->RGBColor[1, 0, 0]]\)\" in FindNucleation.\nUse one of `2`."];
msg["missingIntGammaMethod"]=StringTemplate["With \"NucleationCriterion\"\[Rule]\"IntegralDecay\", \"NucleationMethod\" must include ``."];


msg["NoTperc"]="Percolation not reached!";
msg["searchingTp"]=StringTemplate["Solving \!\(\*SubscriptBox[\(`simple`I\), \(\[ScriptCapitalF]\)]\)(\!\(\*SubscriptBox[\(T\), \(p\)]\)) = `target` for \!\(\*SubscriptBox[\(T\), \(p\)]\) with `method` method..."];


msg["SinglePhase"]="Only 1 phase detected.";
msg["loopPhases"]="Looping over pairs of phases";
msg["MetaKeys"]=StringTemplate["Metadata Keys `` will be converted to String!"];
msg["WallVelocity"]=StringTemplate["Bubble wall velocity \!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\)=`` must be bewteen 0 and 1."];
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
(* number of relativistic degrees of freedom g* *)
RelativisticDOF::usage="Effective number of relativistic degrees of freedom \!\(\*SubscriptBox[\(g\), \(*\)]\).";
RelativisticDOF=106.75;
(* reduced Planck mass in GeV (1.22 10^19) *)
$PlanckMass=Quantity["ReducedPlanckMass"];
Protect[$PlanckMass];


(* ::Subsubsection::Closed:: *)
(*Units*)


$Unit::usage="$Unit is the energy unit symbol (defaults to \"GeV\").";
$PlanckMass::usage="$PlanckMass is the reduced Planck mass.";
$PlanckMassN::usage="$PlanckMassN is numerical value of the reduced Planck mass, in the energy unit set by DefineUnits[\"unit\"].";
DefineUnits::usage="DefineUnits[\"unit\"] defines the energy unit symbol $Unit, and computes the reduced Planck mass in corresponding units.
DefineUnits[] resets $Unit to \"GeV\".";
DefineUnits[u_:"GeV"]:=Module[{},
	Unprotect[$Unit,$PlanckMassN];
	$Unit=PT2GWEcho[u,"Energy units set to",Quantity];
	$PlanckMassN=$PlanckMass Quantity["SpeedOfLight"]^2//UnitConvert[#,u]&//QuantityMagnitude;
	Protect[$Unit,$PlanckMassN];
	]
DefineUnits[] (* initialize to GeV units *)


PrintPlanckMass::usage="PrintPlanckMass[] prints the numerical value of the reduced Planck mass, at the currently defined energy $Unit.";
PrintPlanckMass[]:=PT2GWEcho[Quantity[$PlanckMassN,$Unit],Row@{$PlanckMass," \[Rule]"}];


(* ::Subsubsection::Closed:: *)
(*Cosmological definitions*)


(* vacuum energy radiation density *)
RadiationEnergyDensity[T_]:=\[Pi]^2/30 RelativisticDOF T^4


(* Hubble parameter *)
H::usage="H[T] gives the Hubble parameter at temperature T.
H[T,V,{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)}] includes the vacuum contribution to the total energy density.
";
H[T_?NumericQ]:= Sqrt[RadiationEnergyDensity[T]/3]/$PlanckMassN
H[T_?NumericQ,V_,phases_]:=Module[{xm1,xm2},{xm1,xm2}=phases;
	\[Sqrt]((RadiationEnergyDensity[T]+Abs[V[xm1[T],T]-V[xm2[T],T]])/3)/$PlanckMassN
	]
Hc=H[1];


(* nucleation rate *)
DecayRate::usage="DecayRate[T,ActionFunction] gives the vacuum decay rate.
DecayRate[T,ActionValue] gives the vacuum decay rate.
DecayRate[T,V,{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)}] includes the vacuum energy contribution to the Hubble parameter.
";
DecayRate[T_?NumericQ,action:(_Function|_Symbol),opt:OptionsPattern[]]:=With[{s=action[T,opt]},
	If[s<-Log@$MinMachineNumber,T^4 ((1/(2\[Pi]))s)^(3/2) E^-s,0.]]
DecayRate[T_?NumericQ,action_?NumericQ,OptionsPattern[]]:=If[action<-Log@$MinMachineNumber,T^4 (action/(2\[Pi]))^(3/2) E^-action,0]
DecayRate[T_?NumericQ,V_,phases_,opt:OptionsPattern[]]:=With[{s=Action[T,V,phases,opt]},
	If[(s<-Log@$MinMachineNumber),T^4 (s/(2\[Pi]))^(3/2) E^-s,0]]


(* nucleation criterion: \[CapitalGamma]/H^4 *)
DecayOverHubble::usage="DecayOverHubble[T,V,{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)}] gives the vacuum decay rate per Hubble volume and Hubble time: \[CapitalGamma]/\!\(\*SuperscriptBox[\(H\), \(4\)]\).
DecayOverHubble[T,ActionFunction,V,{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)}] uses either a value or function as input for the action, instead of computing it with FindBounce."
DecayOverHubble[T_,V_,phases_List,opt:OptionsPattern[]]:=DecayRate[T,V,phases,opt]/H[T,V,phases]^4
DecayOverHubble[T_?NumericQ,action:(_?NumericQ|_Function|_Symbol),V_,phases_,opt:OptionsPattern[]]:=DecayRate[T,action,opt]/H[T,V,phases]^4


(* nucleation criterion: \[Integral]dT/T \[CapitalGamma]/H^4 *)
IntegralDecay::usage="IntegralDecay[T,Tc,ActionFunction,V,phases] gives the integral \[Integral]dT/T \[CapitalGamma]/\!\(\*SuperscriptBox[\(H\), \(4\)]\). Nucleation is achieved when IntegralDecay=1.";
IntegralDecay[T_?NumericQ,Tc_,action:(_Function|_Symbol),V_,phases_,opt:OptionsPattern[]]:=NIntegrate[DecayOverHubble[\[ScriptCapitalT],action,V,phases,opt]/\[ScriptCapitalT],{\[ScriptCapitalT],T,Tc}]


(* IntegralFalseVacuum=-Log(Pf), where Pf is the false-vacuum fractional volume *)
IntegralFalseVacuumHubble::usage="IntegralFalseVacuumHubble[T,T'] gives the helper function \!\(\*SubsuperscriptBox[\(\[Integral]\), \(T\), \(T'\)]\)\[DifferentialD]T\[CloseCurlyDoubleQuote]/H(T\[CloseCurlyDoubleQuote]), entering IntegralFalseVacuum.";
IntegralFalseVacuumHubble[T_?NumericQ,T2_?NumericQ]:=NIntegrate[1/H[T3],{T3,T,T2}]
IntegralFalseVacuum::usage="IntegralFalseVacuum[action,T,Tc,\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\)] gives -log(\!\(\*SubscriptBox[\(P\), \(\[ScriptCapitalF]\)]\)), where \!\(\*SubscriptBox[\(P\), \(\[ScriptCapitalF]\)]\) is the false vacuum fractional volume.
IntegralFalseVacuum[action,T,Tc,\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\),V,{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)}] includes the vacuum contribution \[CapitalDelta]V in the Hubble parameter."
IntegralFalseVacuum[action_,T_?NumericQ,Tc_,vw_]:=4\[Pi]/3 vw^3 NIntegrate[(1/T2^4)(DecayRate[T2,action]/H[T2])IntegralFalseVacuumHubble[T,T2]^3,{T2,T,Tc}]
IntegralFalseVacuum[action_,T_?NumericQ,Tc_,vw_,V_,phases_]:=4\[Pi]/3 vw^3 NIntegrate[(1/T2^4)(DecayRate[T2,action]/H[T,V,phases])IntegralFalseVacuumHubble[T,T2]^3,{T2,T,Tc}]
(* radiation-dominated approximation *)
IntegralFalseVacuumRadiation[action_,T_?NumericQ,Tc_,vw_]:=4\[Pi]/3 (vw/T)^3 NIntegrate[((T2-T)^3/T2^9)(DecayRate[T2,action]/Hc^4),{T2,T,Tc}]


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
	"Rebounce"->{True,"ShiftToExitPoint"->True,"LowerActionTolerance"->True},
	"CheckProfile"->{True,"ShiftToExitPoint"->True,"LowerActionTolerance"->True},
	"PrintBounceInfo"->False,"PrintAction"->False,"PrintShiftInfo"->False,"PlotBounce"->False,"Action/T"->True
	};
$dependencies[Action]={Action,FindBounce,ShiftToExitPoint,Plot};
Action[T_?NumericQ,V_,phases_,opt:OptionsPattern[$dependencies[Action]]]:=Module[{
	\[Phi],minima,max,fail,bf,bf0,\[Phi]0,\[Phi]1,xm1,xm2,\[Epsilon],fp,
	\[CapitalDelta]bounce,\[CapitalDelta]bounceMax=10.^-3,
	optFB,optPlot,optShift,
	reBounce=OptionValue["Rebounce"],
	checkProfile=OptionValue["CheckProfile"],
	actionOverT=OptionValue["Action/T"],
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
				PT2GWEcho["Bounce error "<>ToString[Round[\[CapitalDelta]bounce,\[CapitalDelta]bounceMax]]<>" exceeds threshold."]];
			PT2GWEcho["Recomputing with true vacuum shifted towards exit point."]];
		Check[fail=False;
			bf=ShiftToExitPoint[T,V,minima,max,optShift],
			If[reBounce//First,fail=True,fail=False],
			{(*FindBounce::cvmit,*)\!\(TraditionalForm\`FindBounce::errbc\),FindBounce::nosol}];
		If[print\[And]Not[fail]\[And]OptionValue[checkProfile//Rest,"LowerActionTolerance"]\[And](\[CapitalDelta]bounce=BounceError[bf]),
			PT2GWEcho[\[CapitalDelta]bounce,"Bounce error:"]];
		];
	(* 2nd attempt: lower action tolerance *)
	If[(fail\[And]OptionValue[reBounce//Rest,"LowerActionTolerance"]) \[Or]
		(First[checkProfile]\[And]With[{},\[CapitalDelta]bounce=BounceError[bf];\[CapitalDelta]bounce>\[CapitalDelta]bounceMax]\[And]OptionValue[checkProfile//Rest,"LowerActionTolerance"]),
		If[print,
			If[Not@fail,
				PT2GWEcho["Bounce error "<>ToString[Round[\[CapitalDelta]bounce,\[CapitalDelta]bounceMax]]<>" exceeds threshold."]];
			PT2GWEcho["Recomputing with lower ActionTolerance."]];
		Check[fail=False;
			bf=FindBounce[V[\[Phi],T],\[Phi],minima,"MidFieldPoint"->max,"FieldPoints"->3 OptionValue["FieldPoints"],"ActionTolerance"->10.^-16,optFB],
			If[reBounce//First,fail=True,fail=False],
			{(*FindBounce::cvmit,*)\!\(TraditionalForm\`FindBounce::errbc\),FindBounce::nosol}];
		If[print\[And]Not[fail],PT2GWEcho[BounceError[bf],"Bounce error:"]];
		];
	(* plot bounce (optional) *)
	If[OptionValue["PlotBounce"],Print@BouncePlot[bf,optPlot]];
	(* return action *)
	bf["Action"]If[actionOverT,1/T,1]//
		If[OptionValue["PrintAction"],PT2GWEcho[#,msg["actionValue"][<|"T"->T,"Action/T"->actionOverT|>]]&,Identity]
	]
Action[TL_List,V_,phases_,opt:OptionsPattern[]]:=Table[Action[T,V,phases,opt],{T,TL}]
AutoComplete[Action,$dependencies[Action]];


ShiftToExitPoint::usage="ShiftToExitPoint[T,V,Minima,Mamimum] shifts the true vacuum towards exit point, estimated by \!\(\*SubscriptBox[\(\[Phi]\), \(eq\)]\):V(\!\(\*SubscriptBox[\(\[Phi]\), \(eq\)]\))=V(\!\(\*SubscriptBox[\(\[Phi]\), \(FV\)]\)).
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
	\[Phi]eq=\[Phi]/.FindRoot[V[\[Phi],T]==V[\[Phi]F,T],{\[Phi],Mean@{max,\[Phi]T}}~Join~Sort@{\[Phi]T,max}];
	\[Phi]0=\[Phi]T+shift(\[Phi]eq-\[Phi]T);
	If[OptionValue["PrintShiftInfo"],PT2GWEcho[\[Phi]T->\[Phi]0,"Shifting \!\(\*SubscriptBox[\(\[Phi]\), \(TV\)]\)\[Rule]\!\(\*SubscriptBox[\(\[Phi]\), \(eq\)]\)+"<>ToString[shift]<>"(\!\(\*SubscriptBox[\(\[Phi]\), \(TV\)]\)-\!\(\*SubscriptBox[\(\[Phi]\), \(eq\)]\)):"]];
	(* recompute bounce *)
	FindBounce[V[\[Phi],T],\[Phi],{\[Phi]0,\[Phi]F},"MidFieldPoint"->max(*,"ActionTolerance"->10^-16*),optFB]
]


(* ::Subsubsection:: *)
(*Filter & Refine*)


(* ::Text:: *)
(*Filter and refine based on the data distribution*)


BounceError[bounce_]:=Module[{radii,\[Phi]Funs,\[Phi]Extrema,\[CapitalDelta]\[Phi]},
	radii=bounce["Radii"];
	(*\[Phi]Values=bounce["Bounce"][[1]][#]&/@radii;*)
	\[Phi]Funs=Table[bounce["Bounce"][[1]][r][[1,n,1]],{n,Length[radii]-1}];
	\[CapitalDelta]\[Phi]=Table[\[Phi]Funs[[{i-1,i}]]/.r->radii[[i]],{i,2,Length[radii]-1}];
	(*\[Phi]Extrema=Table[{#/.r->radii[[n]],#/.r->radii[[n+1]]}&@\[Phi]Funs[[n]],{n,Length[radii]-1}];*)
	Sqrt[((#[[2]]-#[[1]])/(#[[2]]+#[[1]]))^2&/@\[CapitalDelta]\[Phi]//Mean]
]


ActionFilter::usage="Filter out points from a table of action values {{\!\(\*SubscriptBox[\(T\), \(1\)]\),(S/T\!\(\*SubscriptBox[\()\), \(1\)]\)},..} which violate \[CapitalDelta]^2(ln(S/T))/\[CapitalDelta]T^2<n.\[Sigma],
where the maximum number of standard deviation allowed is defined in the option 'stdMax'. Defaults to 1.";
Options[ActionFilter]={"stdMax"->1.};
ActionFilter[data_,OptionsPattern[]]:=Module[{Tlist,STlist,dT,dT2,dST,d2ST,\[Sigma],stdMax=OptionValue["stdMax"],pos,nFiltered},
	{Tlist,STlist}=data\[Transpose];
	dT=Differences[Tlist];
	dT2=MovingAverage[dT,2];
	dST=Differences[Log@STlist]/dT;
	d2ST=Differences[Log@dST]/dT2;
	\[Sigma]=Sqrt[Variance[d2ST]];
	pos=Position[d2ST,x_/;Abs[x-Mean[d2ST]]<stdMax \[Sigma]]//Flatten;
	nFiltered=Length[STlist]-Length[pos]-2;
	PT2GWPrint@msg["ActionFilter"][nFiltered,Length[STlist]];
	data[[{1,2}~Join~(pos+2)]]
	]

ActionRefine::usage="ActionRefine[V,data,{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)}] refines points filtered out by \!\(\*
StyleBox[\"ActionFilter\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\"by\",\nFontWeight->\"Plain\"]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Plain\"]\)recomputing the action with a lower 'ActionTolerance'.
Defaults to \!\(\*SuperscriptBox[\(10\), \(-15\)]\).";
Options[ActionRefine]={"ActionTolerance"->10.^-15};
ActionRefine[V_,data_,phases_,opt:OptionsPattern[]]:=Module[{optAction,badTemps,newData,filData,Tlist,STlist},
	optAction=Evaluate@FilterRules[{opt}~Join~Options[ActionRefine]~Join~Options[Action],Options[Action]~Join~Options[FindBounce]]//DeleteDuplicatesBy[#,First]&;
	{Tlist,STlist}=data\[Transpose];
	filData=ActionFilter[data];
	badTemps=Complement[data,filData][[All,1]];
	newData={badTemps,Action[V,badTemps,phases,optAction]}\[Transpose];
	SortBy[filData~Join~newData,First]
]


(* ::Text:: *)
(*Filter  and  refine  nearby the inflection point (where FindBounce might fail)*)


ActionRefineInflection::usage="ActionRefineInflection[V,{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)},{\!\(\*SubscriptBox[\(T\), \(miss\)]\),\!\(\*SubscriptBox[\(T\), \(hit\)]\)}] implements the bisection algorithm to refine the action nearby a point of failure, identified by the temperature \!\(\*SubscriptBox[\(T\), \(miss\)]\).";
Options[ActionRefineInflection]={"NHits"->4,"MaxIterations"->20,"Keep"->Last};
ActionRefineInflection[V_,phases_,{Tmiss_,Thit_},opt:OptionsPattern[{ActionRefineInflection,Action}]]:=Module[
	{Tm=Tmiss,Th=Thit,nhits=0,optAction,print=OptionValue["PrintAction"]},
	optAction=Evaluate@FilterRules[{opt}~Join~Options[Action],Options[Action]];
	If[print,PT2GWPrint@"Refining action nearby the point of failure"];
	(* bisection method *)
	(NestWhileList[(
		\[CapitalTau]=#[[1]]; (* private dynamic variable for the ProgressIndicator *)
		If[NumericQ@#[[2]],
			Th=#[[1]],
			Tm=#[[1]]
			];
		{#,Action[#,V,phases,optAction]}&@Mean[{Tm,Th}]
		)&,
		{#,Action[#,V,phases,optAction]}&@Mean[{Tm,Th}], (* starting expression *)
		Total@Boole[NumericQ/@List[##][[All,2]]]<OptionValue["NHits"]&, (* test *)
		All,(* #results to be tested *)
		OptionValue["MaxIterations"] (* max iterations *)
		]//Cases[#,{_,_?NumericQ}]&)[[OptionValue["Keep"]/.{First->{1},Last->{-1}}]]
	]


(* ::Subsubsection:: *)
(*Fit Functions*)


(* ::Text:: *)
(*Laurent polynomial \[Sum]_n c_n (Tc - T)^n*)


Options[laurentFitFun]={"Orders"->Range[-2,0]};
laurentFitFun[data_,Tc_,opt:OptionsPattern[]]:=Module[{
	orders=OptionValue["Orders"],coeffs,optFit,printOutput=OptionValue[Print],laur},
	(* extract fitting options *)
	optFit=Evaluate@FilterRules[{opt}~Join~Options[laurentFitFun]~Join~Options[NonlinearModelFit],
		Options[NonlinearModelFit]]//DeleteDuplicatesBy[#,First]&;
	(* contruct fit function *)
	coeffs=(Subscript[c, #]&/@orders);
	laur=coeffs . ("\!\(\*SubscriptBox[\(T\), \(c\)]\)"-T)^orders;
	If[printOutput,PT2GWEcho[laur,"Fitting to laurent polynomial"]];
	(* perform fit *)
	fit=NonlinearModelFit[data,{laur/."\!\(\*SubscriptBox[\(T\), \(c\)]\)"->Tc,Subscript[c, Min[orders]]>0},coeffs,T,optFit];
	(* return output *)
	fit
]


(* ::Text:: *)
(*Piecewise function, merging*)
(*- interpolation far from Tc*)
(*- Laurent fit close to Tc*)


pwLaurentFun::usage="pwLaurentFun[data,\!\(\*SubscriptBox[\(T\), \(c\)]\)] constructs a picewise function from a (low-T) spline-interpolation and a (high-T) laurent fitting.";
Options[pwLaurentFun]={"Orders"->Range[-2,1]};
pwLaurentFun[data_,Tc_,opt:OptionsPattern[]]:=Module[{optInt,optFit,\[ScriptCapitalD],actionInterp,orders,coeffs,laur,cons,dataFit,laurFit},
	optFit=Evaluate@FilterRules[{opt}~Join~Options[pwLaurentFun],Options[NonlinearModelFit]]//DeleteDuplicatesBy[#,First]&;
	\[ScriptCapitalD]=MinMax[data[[All,1]]];
	(* low T: spline interpolation *)
	actionInterp=Interpolation[data,Method->"Spline"];	
	(* high T: laurent polynomial *)
	orders=OptionValue["Orders"](*Range[-2,1]*);
	coeffs=(Subscript[c, #]&/@orders);
	laur=Function[T,coeffs . ("\!\(\*SubscriptBox[\(T\), \(c\)]\)"-T)^orders//Evaluate];
	(* continuity and smoothness conditions *)
	cons={
		laur[T]==actionInterp[T],
		laur'[T]==actionInterp'[T],
		laur''[T]==actionInterp''[T]
		}/.T->\[ScriptCapitalD][[2]]/."\!\(\*SubscriptBox[\(T\), \(c\)]\)"->Tc;
	dataFit=data[[-3;;]];
	laurFit=Check[
		NonlinearModelFit[dataFit,{laur[T]/."\!\(\*SubscriptBox[\(T\), \(c\)]\)"->Tc,
			Subscript[c, Min[orders]]>0}~Join~cons,coeffs,T,optFit],
		(* in case fitting fails, brute-force the solution by imposing a constraint on Subscript[c, min] *)
		laur/.NSolve[cons~Join~{Subscript[c, Min[orders]]==1},coeffs][[1]]/."\!\(\*SubscriptBox[\(T\), \(c\)]\)"->Tc
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
RowBox[{"actionInterp", "[", "T", "]"}], 
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
	"NActionPoints"->31,
	"ActionMethod"->"PWLaurent",
	"StopAtFailure"->False,
	"RefineInflection"->{True,"NHits"->4,"MaxIterations"->12,"Keep"->Last},
	"Refine"->False,
	(*Function->None,Variables->None,*)
	"PlotAction"->False,ProgressIndicator->True
	};
(*SetOptions[ActionFit,{"Rebounce"->True}];*)
ActionFit[V_,phases_,{Tmin_,Tmax_},Tc_:0,opt:OptionsPattern[{ActionFit,Action,Interpolation,laurentFitFun,pwLaurentFun}]]:=Module[{
	(* internal variables *)
	Tlist,STlist,s,actionFun,printProg,\[Sigma],TMissHit,
	(* options *)
	optAction,optLaur,optInt,optPW,
	refineInf=OptionValue["RefineInflection"],
	data=OptionValue["Data"],
	method=OptionValue["ActionMethod"],
	(*fitFunction=OptionValue[Function],vars=OptionValue[Variables],*)
	npoints=OptionValue["NActionPoints"]
	},
	optAction=Evaluate@FilterRules[{opt}~Join~Options[Action],Options[Action]]//DeleteDuplicatesBy[#,First]&;
	optLaur=Evaluate@FilterRules[{opt}~Join~Options[laurentFitFun],Options[laurentFitFun]]//DeleteDuplicatesBy[#,First]&;
	optPW=Evaluate@FilterRules[{opt}~Join~Options[pwLaurentFun],Options[pwLaurentFun]]//DeleteDuplicatesBy[#,First]&;
	optInt=Evaluate@FilterRules[{opt}~Join~Options[Interpolation],Options[Interpolation]]//DeleteDuplicatesBy[#,First]&;
	(* compute/extract euclidean action values *)
	If[data===None,
		(* solve bounce action over T range *)
		Tlist=Subdivide[Tmax,Tmin,npoints];
		If[OptionValue[ProgressIndicator],
			printProg=PrintTemporary[
				ProgressIndicator[Dynamic@\[CapitalTau], {Tmax,Tmin}], 
				"  Evaluating action at T = ",Dynamic[\[CapitalTau]]," ",$Unit (* NB StringTemplates are not dynamic *)
				]
			];
		(* call Action function on T list *)
		data=Quiet[
		If[OptionValue["StopAtFailure"],
			(* stop at first FindBounce failure *)
			data={};
			Catch[Do[
					s=Action[\[CapitalTau],V,phases,optAction];
					AppendTo[data,{\[CapitalTau],s}];
					(* Throw data at the first non-numerical value (included) *)
					If[!NumericQ[s],PT2GWPrint@msg["stopAtFailure"];Throw[data]],
				{\[CapitalTau],Tlist}];
				Throw[data]],
			(* don't stop at FindBounce failure *)
			STlist=Table[Action[\[CapitalTau],V,phases,optAction],{\[CapitalTau],Tlist}];
			{Tlist,STlist}\[Transpose]
			],
			FindRoot::reged]
			];
	data=SortBy[data,First]; (* sort by temperature *)
	
	(* optional: refine data nearby the inflection point *)
	If[First@refineInf \[And]
		Length[TMissHit=SequenceCases[data,{{Tmiss_,Except[Amiss_?NumericQ]},{Thit_,Ahit_?NumericQ}}:>{Tmiss,Thit}]]>0,
		data=SortBy[ActionRefineInflection[V,phases,First@TMissHit,Rest@refineInf,optAction],First]~Join~data
		];
	NotebookDelete[printProg];
	
	(* filter out non-numerical values *)
	data=Cases[data,{_,_?NumericQ}];
	
	(* optional: refine data based on BounceError *)
	If[OptionValue["Refine"],data=ActionRefine[V,data,phases,"ShiftToExitPoint"->False]];
	
	(* select method *)
	actionFun=Switch[method,
		Interpolation,Interpolation[data,Method->"Spline",optInt],
		"Laurent",laurentFitFun[data,Tc,optLaur],
		"PWLaurent",pwLaurentFun[data,Tc,optPW]
		];
	(* fit error deviation *)
	STlist=Table[actionFun[T],{T,data[[All,1]]}];
	\[Sigma]=Sqrt[Mean[(STlist-data[[All,2]])^2]];
	(* plot (optional) *)
	If[OptionValue["PlotAction"],
		Print@PlotAction[actionFun,{Tmin,Tc},"Data"->data,
			"Tlines"-><|"Tc"->{Dashed,Red,InfiniteLine[{{Tc,0},{Tc,1}}],
				Text[Style["\!\(\*SubscriptBox[\(T\), \(c\)]\)",Medium],{Tc-2,1},Scaled@{.5,-.5}]}
				|>
		]];
	(* output *)
	(*OptionValue[Return]/.{ActionFunction->af,"Function"->actionFun,"Data"->data}*)
	ActionFunction[<|
		"Function"->actionFun,
		"ActionMethod"->method,
		"Domain"->{Min@data[[All,1]],Tc},
		If[Tc=!=0,"Tc"->Tc,Nothing],
		"StandardDeviation"->\[Sigma],
		"NActionPoints"->Length[data],
		"Unit"->$Unit,
		"Data"->data|>]
	]
AutoComplete[ActionFit,{ActionFit,Action,Interpolation,laurentFitFun,pwLaurentFun}];


(* ::Subsubsection::Closed:: *)
(*Plot action*)


PlotAction::usage="PlotAction[ActionFunction,{\!\(\*SubscriptBox[\(T\), \(min\)]\),\!\(\*SubscriptBox[\(T\), \(max\)]\)}] plot the Euclidean action Subscript[S, 3]/T in the given temperature range.
PlotAction[ActionFunction] plots the action \!\(\*SubscriptBox[\(S\), \(3\)]\)(T)/T for the given ActionFunction object.
PlotAction[Transition] \!\(\*SubscriptBox[\(S\), \(3\)]\)(T)/T for the given Transition object.
PlotAction[Associaton] \!\(\*SubscriptBox[\(S\), \(3\)]\)(T)/T for the given Association.";
TlinesStyle[lab_String]:=Directive[Dashed,lab/.<|"Tc"->Red,"Tn"->Orange,"Tp"->Hue[.12,1,.9]|>];
TlinesFun[lab_String,T_]:={TlinesStyle[lab],InfiniteLine[{T,0},{0,1}]};
Options[PlotAction]={"Data"->{},"Temperatures"-><||>,PlotLegends->Automatic};
$dependencies[PlotAction]={PlotAction,LogPlot,ListLogPlot};
PlotAction[actionFun_,{Tmin_,Tmax_},opt:OptionsPattern[$dependencies[PlotAction]]]:=Module[{
	Tm,TM,Ts,TlinesStyles,Tlines,lgData,lgFit,lgTs,plot,
	optLogPlot,optListPlot,data=OptionValue["Data"],temps=OptionValue["Temperatures"]
	},
	{Tm,TM}=MinMax[{Tmin,Tmax},Scaled[.1]];
	optLogPlot=Evaluate[FilterRules[{opt}~Join~Options[PlotAction],Options[LogPlot]]//DeleteDuplicatesBy[#,First]&];
	optListPlot=Evaluate@FilterRules[{opt}~Join~Options[PlotAction],Options[ListLogPlot]](*//DeleteDuplicatesBy[#,First]&*);
	With[{optPlot=optLogPlot},
	Ts=KeyTake[temps,{"Tc","Tn","Tp"}];
	TlinesStyles=KeyValueMap[TlinesStyle[#1]&,Ts];
	Tlines=KeyValueMap[TlinesFun,Ts];
	lgData=PointLegend[{Black},{"FindBounce"}];
	lgFit=LineLegend[{ColorData[3,6]},{"Fit function"}];
	lgTs=If[Length[Ts]>0,LineLegend[TlinesStyles,Keys@Ts/.(T_String:>Subscript[StringTake[T,1],StringTake[T,{2}]])],Nothing];
	plot=Show[
		LogPlot[actionFun[T],{T,Tm,TM},
			optPlot,
			PlotStyle->ColorData[3,6],
			Frame->True,
			FrameLabel->{"T / "<>$Unit,"\!\(\*FractionBox[SubscriptBox[\(S\), \(3\)], \(T\)]\)(T)"},
			PlotLabel->"Action fit",
			GridLines->Automatic,
			Epilog->Tlines
			],
		ListLogPlot[data,PlotStyle->Black]
		];
	If[OptionValue[PlotLegends]===Automatic,Legended[plot,Column@{lgData,lgFit,lgTs}],plot]
	]]


PlotAction[asc_Association,opt:OptionsPattern[{}]]:=Module[{actionFun,Tmin,data,temps},
	actionFun=asc["Function"];
	data=Lookup[asc,"Data",{}];
	temps=KeyTake[asc,{"Tc","Tn","Tp"}];
	PlotAction[actionFun,asc["Domain"],opt,"Data"->data,"Temperatures"->temps]
	]


PlotAction[af_ActionFunction,opt:OptionsPattern[{}]]:=Module[{actionFun,data,temps},
	actionFun=af["Function"];
	data=af["Data",{}];
	temps=KeyTake[af[Association],{"Tc","Tn","Tp"}];
	PlotAction[actionFun,af["Domain"],opt,"Data"->data,"Temperatures"->temps]
	]


PlotAction[tr_Transition,opt:OptionsPattern[{}]]:=Module[{temps},
	temps=KeyTake[tr[Association],{"Tc","Tn","Tp"}];
	PlotAction[tr["ActionFunction"],opt,"Temperatures"->temps]
	]
AutoComplete[PlotAction,$dependencies[PlotAction]];


(* ::Subsubsection::Closed:: *)
(*Action Object*)


ActionGraphics[af_ActionFunction|af_Transition]:=Module[{},
	PlotAction[af,graphicsOptions]]


ActionFunction::usage="The ActionFunction[] object wraps a temperature-dependent action function, including information such as its domain and the temperature energy unit.
ActionFunction[T] gives the value of the action at temperature T, if defined.
ActionFunction[key] gives the value associated to key, if defined.
";
ActionFunction::noprop="Property `1` of \!\(\*
StyleBox[\"ActionFunction\",\nFontWeight->\"Bold\"]\) is not available. It should be one of `2`.";

(* object must by applied to an Association *)
ActionFunction[asc_Association]["Properties"]:=Sort@Keys[asc]

(* Special formats *)
Dataset[ActionFunction[asc_Association]]^:=Dataset[asc]
Normal[ActionFunction[asc_Association]]^:=asc
ActionFunction[asc_Association][Dataset]:=Dataset[asc]
ActionFunction[asc_Association][Association]:=asc
ActionFunction[asc_Association][T_?NumericQ]:=asc["Function"][T]

(* lookup value(s) *)
ActionFunction[asc_Association][keys:(_List|_String),default___,h___]:=Lookup[asc,keys,default,h]

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


(* ::Subsubsection:: *)
(*Phase tracing*)


TracePhases::usage="TracePhases[V,\"TRange\"->{\!\(\*SubscriptBox[\(T\), \(min\)]\),\!\(\*SubscriptBox[\(T\), \(max\)]\)}] numerically traces the phases of a thermal potential V in the range list.
TracePhases[V,\"TRange\"->{\!\(\*SubscriptBox[\(T\), \(min\)]\),\!\(\*SubscriptBox[\(T\), \(max\)]\)},\*StyleBox[\\\"TracingMethod\\\", ShowStringCharacters->True]->\*StyleBox[\\\"SimpleFieldDependence\\\", ShowStringCharacters->True]] numerically traces the phases of a thermal potential V in the range list.
TracePhases[V,\"TracingMethod\"->NSolve] traces the phases analytically, for simple potentials with numerical parameters.
TracePhases[V,\"TracingMethod\"->Solve] traces the phases analytically, for simple potentials.
";
TracePhases::optTrace=msg["optTrace"];
TracePhases::noTRange=msg["noTRange"];
Options[TracePhases]={"TracingMethod"->"Numeric","PlotPhaseDiagram"->True,
	"TRange"->None,"NTracingPoints"->100,
	"SymmetricPhaseThreshold"->.1,"BrokenPhaseScale"->10.^6,
	"ShiftSymmetricPhase"->False,
	ProgressIndicator->True,Print->True
};
TracePhases[V_,opt:OptionsPattern[{TracePhases,Solve,Plot}]]:=Module[{
	phases,\[Phi],Tmin,Tmax,Tlist,n,\[Phi]S,\[Phi]B,\[Phi]th,\[Phi]Bscale,\[Phi]max,
	overlaps,printProg,printTracing,optPlot,
	unboundedV={},
	methods={Solve,NSolve,"Numeric","SimpleFieldDependence"},
	method=OptionValue["TracingMethod"],
	TRange=OptionValue["TRange"],
	print=OptionValue[Print]
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
			phases=Function[Global`T,#//Evaluate]&/@(\[Phi]/.method[{\!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Phi]\)]\(V[\[Phi], Global`T]\)\)==0,\!\(
\*SubscriptBox[\(\[PartialD]\), \({\[Phi], 2}\)]\(V[\[Phi], Global`T]\)\)>0,Global`T>0},\[Phi]]),
		(* numerical methods *)
		"Numeric"|"SimpleFieldDependence", (* interpolate between fixed-T minima *)
			If[!MatchQ[TRange,{_?NumericQ,_?NumericQ}],
				Message[TracePhases::noTRange,Style[TRange,Red]];
				Return[]
				];
			{Tmin,Tmax}=TRange;
			If[print,printTracing=PrintTemporary@msg["tracing"]];
			n=OptionValue["NTracingPoints"];
			\[Phi]th=OptionValue["SymmetricPhaseThreshold"];
			\[Phi]Bscale=OptionValue["BrokenPhaseScale"];
			Tlist=Subdivide[Tmin,Tmax,n];
			(* optional progress bar *)
			If[OptionValue[ProgressIndicator],
				printProg=PrintTemporary[
					ProgressIndicator[Dynamic@\[CapitalTau], {Tmin,Tmax}], 
					"  Minimizing potential at T = ",Dynamic[\[CapitalTau]]," ",$Unit (* NB StringTemplates are not dynamic *)
					]
				];
			Switch[method,
				"SimpleFieldDependence",
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
				If[unboundedV!={},PT2GWPrint@msg["UnboundedV"][MinMax[unboundedV]//Union]];
				(* filter out remaining \[Phi]th values (from FindMinimum::lstol error, if any) *)
				\[Phi]S=DeleteCases[\[Phi]S,{_,\[Phi]_}/;Abs[\[Phi]-\[Phi]th]<0.1 \[Phi]th];
				\[Phi]B=DeleteCases[\[Phi]B,{_,\[Phi]_}/;Abs[\[Phi]-\[Phi]th]<0.1 \[Phi]th];
				(* check potential is real at the phases *)
				If[!FreeQ[V@@@Reverse[\[Phi]S,2],_Complex],PT2GWPrint@msg["Vcomplex"]["symmetric"]];
				If[!FreeQ[V@@@Reverse[\[Phi]B,2],_Complex],PT2GWPrint@msg["Vcomplex"]["broken"]];
				];
			Which[\[Phi]S=={},PT2GWPrint["No symmetric phase."],\[Phi]B=={},PT2GWPrint["No broken phase."]];
			phases={\[Phi]S,\[Phi]B}/.{}->Nothing;
			(* shift symmetric phase, which might result in divergences *)
			If[OptionValue["ShiftSymmetricPhase"],
				PT2GWPrint@msg["PhaseShift"];
				(*\[Phi]max=\[Phi]/.PT2GWEcho[MinimalBy[FindMaximum[V[\[Phi],#[[1]]],{\[Phi],#[[2]]}]&/@\[Phi]S,Abs[First#]&],"Subscript[\[Phi], max]"][[2]];*)
				\[Phi]S=#+{0,10^-6(*\[Phi]max[[1]]*)Min[Abs[\[Phi]B[[All,2]]]]}&/@\[Phi]S];
			(* interpolation: avoid extrapolation and warning message *)
			phases=Interpolation[#,"ExtrapolationHandler"->{Indeterminate &,"WarningMessage"->False}]&/@phases,
		(* bad tracing method *)
		_,Message[TracePhases::optTrace,Style[method,Red],methods/.x_String:>"\""<>x<>"\""];Return[]
		];
	If[print,NotebookDelete[printTracing]];
	If[OptionValue[ProgressIndicator],NotebookDelete[printProg]];
	(* plot phase diagram (optional) *)
	If[OptionValue["PlotPhaseDiagram"],Print@PlotPhases[phases,optPlot]];
	phases
	]
AutoComplete[TracePhases];


PlotPhases::usage="PlotPhases[{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)}] plots the phases \!\(\*SubscriptBox[\(\[Phi]\), \(1, 2\)]\), in a range is given by their overlap.
PlotPhases[transition] plots the two phases of a Transition object.";
Options[PlotPhases]={Range->Automatic};
PlotPhases[phases_List,opt:OptionsPattern[{PlotPhases,Plot}]]:=Module[{Tmin,Tmax,Tscale=0.1,optPlot},
	optPlot=Evaluate@FilterRules[{opt},Options[Plot]];
	{Tmin,Tmax}=OptionValue[Range]/.Automatic->MinMaxPhases[phases,Scaled[Tscale]];
	Tmin=Max[Tmin,0]; (* avoid negative phases *)
	Plot[Through[phases[T]]//Evaluate,{T,Tmin,Tmax},optPlot//Evaluate,
		PlotLabel->"Phase diagram",
		PlotStyle->ColorData[89],
		Frame->True,
		FrameLabel->{"T / "<>$Unit,"\!\(\*SubscriptBox[\(\[Phi]\), \(min\)]\) / "<>$Unit},
		GridLines->Automatic
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
				FunctionDomain[{#[T],T>0},T]]&    (* if Function or Symbol *)
			/@phases,T];
	On[Reduce::ratnz];
	res
	]


MinMaxPhases[phases_List,opt___]:=Module[{overlaps},
	overlaps=Overlap/@Subsets[phases,{2}];
	MinMax[overlaps//Flatten,opt]
	]


(* ::Subsubsection::Closed:: *)
(*Tc - critical temperature*)


(* find critical temperature *)
FindCritical::usage="FindCritical[V,{Subscript[\[Phi], 1],Subscript[\[Phi], 2]}] searches for critical temperatures between the phases \!\(\*SubscriptBox[\(\[Phi]\), \(1, 2\)]\)(T).";
FindCritical::noOverlap="No overlap between the phases `2` of the potential `1`";
FindCritical::empty="No transition between the phases `2` of the potential `1`";
FindCritical::multiTc="Multiple transitions between the phases `2` of the potential `1`";
FindCritical[V_,phases_]:=Module[{sol,phasesTOverlap,Tguess,\[CapitalDelta]V},
	(* overlap region between the 2 phases *)
	phasesTOverlap=Overlap[phases];
	If[phasesTOverlap=={},Message[FindCritical::noOverlap,V,phases];sol=None,
		(* If overlap between phases, solve V(T,\[Phi]1)=V(T,\[Phi]2) for T *)
		\[CapitalDelta]V[T_]:=Subtract@@(V[#[T],T]&/@phases);
		Tguess=phasesTOverlap//Mean;
		sol=Check[
			T/.FindRoot[\[CapitalDelta]V[T],{T,Tguess,Sequence@@phasesTOverlap}],(*NSolve[{\[CapitalDelta]V[T]==0,T>0},T]*)
			None,{FindRoot::reged,FindRoot::nlnum} (* FindRoot returns the last iteration: instead, return empty solution if reged message appears *)
		];
		(*DeleteCases[sol,t_->(x_/;x==phasesTOverlap[[1]] \[Or] x==phasesTOverlap[[2]])]; (* FindRoot might return edges with no warning *)*)
		Which[sol==None,Message[FindCritical::empty,V,phases],
			Length[sol]>1,Message[FindCritical::multiTc,V,phases](* NB FindRoot is a local solver: no multiple solutions! *)]
		];
	sol
	]


(* ::Subsubsection::Closed:: *)
(*Tn - nucleation temperature*)


(* find the nucleation temperature *)
FindNucleation::usage="FindNucleation[V,{Subscript[T, min],Subscript[T, max]},{Subscript[\[Phi], 1],Subscript[\[Phi], 2]}] searches for a nucleation temperature in the given range and for the given phases.";
Options[FindNucleation]={
	"NucleationCriterion"->"DecayOverHubble","NucleationMethod"->"Bisection",
	AccuracyGoal->1,PrecisionGoal->\[Infinity],Return->"TnRule",
	ProgressIndicator->True,Print->False
	};f
(*SetOptions[FindNucleation,{"FieldPoints"->201}];*)
FindNucleation[V_,{Tmin_,Tmax_},phases_,opt:OptionsPattern[{FindNucleation,Action,Bisection,FindRoot}]]:=Module[{
	nucleationFun,phasesTOverlap,Tlow,TnRule,Tn,nucFun,
	printProg,optAction,optBis0,optBis,criterions,intMethods,
	method=OptionValue["NucleationMethod"],nucleationCrit=OptionValue["NucleationCriterion"]
	},
	optAction=Evaluate@FilterRules[{opt}~Join~Options[FindNucleation]~Join~Options[Action],Options[Action]]//DeleteDuplicatesBy[#,First]&;
	(* check input options *)
	criterions={"DecayOverHubble","IntegralDecay",{"ActionValue"->_}};
	intMethods={"ActionFunction"->"","Tc"->"","TnEstimate"->""};
	If[NoneTrue[criterions,MatchQ[nucleationCrit,#]&],
		PT2GWPrint@msg["noTnCriterion"][nucleationCrit,criterions];
		Return["Tn"->Indeterminate]
		];
	If[nucleationCrit=="IntegralDecay" \[And] (intMethods=Select[intMethods,!MemberQ[Keys@method,First[#]]&];intMethods=!={}),
		PT2GWPrint@msg["missingIntGammaMethod"][intMethods];
		Return["Tn"->Indeterminate]
		];
	(* initialize *)
	If[OptionValue[Print],PT2GWPrint@msg["ComputingTn"][nucleationCrit,method]];
	If[OptionValue[ProgressIndicator],
		printProg=PrintTemporary[
			ProgressIndicator[Appearance->"Indeterminate"],"  Estimating \!\(\*SubscriptBox[\(T\), \(n\)]\)"]
		];
	phasesTOverlap=Overlap[phases];
	Tlow=Max[Tmin,phasesTOverlap[[1]]];
	(* pick criterion *)
	nucleationFun=nucleationCrit/.{
		"IntegralDecay":>(Log[IntegralDecay[#]]&),
		"DecayOverHubble":>(LogPTGW[DecayOverHubble[#]]&),
		{"ActionValue"->target_}:>(actionFun[#]-target&)
		};
	(* pick method *)
	nucleationFun=nucleationFun/.(method/.{
		"Bisection"->{DecayOverHubble[x_]:>DecayOverHubble[x,V,phases,optAction],actionFun[x_]:>Action[x,V,phases,optAction]},
		FindRoot->{DecayOverHubble[x_]:>DecayOverHubble[x,V,phases,optAction],actionFun[x_]:>Action[x,V,phases,optAction]},
		{"ActionFunction"->actionFun_}->{DecayOverHubble[x_]:>DecayOverHubble[x,actionFun,V,phases,optAction]},
		{"ActionFunction"->actionFun_,___,"Tc"->Tc_,___}->{IntegralDecay[x_]:>IntegralDecay[x,Tc,actionFun,V,phases,optAction]}
		(* Tc must be provided in NucleationMethod *)
		});
	If[MatchQ[method,"Bisection"|{"ActionFunction"->_}],
		optBis0=Evaluate@FilterRules[{opt},Options[Bisection]]//DeleteDuplicatesBy[#,First]&;
		optBis=Sequence@@{optBis0,"Ascending"->(nucleationCrit/.{"DecayOverHubble"->False,{"ActionValue"->target_}->True}),
							Direction->Up(*(nucleationCrit/.{"DecayOverHubble"->Up,{"ActionValue"->target_}->Up})*)
							}
		];
	(* solve for nucleation temperature *)
	Tn=method/.{
		"Bisection":>(Bisection[nucleationFun,{Tlow,Tmax},10^-4(Tmax-Tlow),1.,Return->"x",optBis]),
		{"ActionFunction"->_}:>(Bisection[nucleationFun,{Tlow,Tmax},10^-4(Tmax-Tlow),1.,Return->"x",optBis]),
		{"ActionFunction"->_,___,"TnEstimate"->Tn0_,___}:>(T/.First@FindRoot[nucleationFun[T],{T,Tn0,Tlow,Tmax},
			AccuracyGoal->OptionValue[AccuracyGoal],PrecisionGoal->OptionValue[PrecisionGoal]]),
		FindRoot:>(T/.First@FindRoot[nucleationFun[T],{T,Mean@{Tlow,Tmax},Tlow,Tmax},
			AccuracyGoal->OptionValue[AccuracyGoal],PrecisionGoal->OptionValue[PrecisionGoal](*,DampingFactor->2*)])
		};
	If[OptionValue[ProgressIndicator],NotebookDelete[printProg]];
	(* return result *)
	If[!NumericQ[Tn],PT2GWPrint@msg["NoTnuc"]];
	If[Tn==Tlow,PT2GWPrint@msg["NoTnuc"];Tn=Indeterminate];
	TnRule="T"->Tn;
	OptionValue[Return]/.{
		"TnRule":>TnRule,"STnRule":>("ActionValue"->If[NumericQ@Tn,Action[Tn,V,phases,optAction],None]),
		"Tn":>Tn,
		"NucleationAction":>If[NumericQ@Tn,Action[Tn,V,phases,optAction],None],
		"NucleationDecayOverHubble":>If[NumericQ@Tn,If[MatchQ[method,{"ActionFunction"->_,___}],
				DecayOverHubble[Tn,"ActionFunction"/.method,V,phases,optAction],
				DecayOverHubble[Tn,V,phases,optAction]],
			None],
		"NucleationIntegralDecay":>If[NumericQ@Tn,Module[{actionFun,Tc},
			{actionFun,Tc}={"ActionFunction","Tc"}/.method;
			IntegralDecay[Tn,Tc,actionFun,V,phases,optAction]],
			None]
		}
	]
AutoComplete[FindNucleation,{FindNucleation,Action,Bisection,FindRoot}];


(* ::Subsubsection::Closed:: *)
(*Tp - percolation temperature*)


(* ::Text:: *)
(*Percolation temperature*)


FindPercolation::usage="FindPercolation[ActionFunction,{\!\(\*SubscriptBox[\(T\), \(pGuess\)]\),\!\(\*SubscriptBox[\(T\), \(c\)]\)},\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\)] searches for a percolation temperature for the given Euclidean action function Subscript[S, 3](T)/T.
FindPercolation[ActionFunction,{\!\(\*SubscriptBox[\(T\), \(pGuess\)]\),\!\(\*SubscriptBox[\(T\), \(c\)]\)},\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\),V,{Subscript[\[Phi], 1],Subscript[\[Phi], 2]}] includes the vacuum energy \[CapitalDelta]V in the Hubble parameter.";
Options[FindPercolation]={Method->FindRoot,"Target"->0.34,"TRange"->{},\[Epsilon]->0.01,Return->"Tp",ProgressIndicator->True};
FindPercolation::method="The Method `1` is not FindRoot or \"Bisection\".";
FindPercolation::TRange="Invalid option \"TRange\"\[Rule]`1`. With Method\[Rule]\"Bisection\", a valid temperature range must be provided.";
FindPercolation[actionFun_,{TpGuess_,Tc_},vw_,V_:None,phases_:None,opt:OptionsPattern[{FindPercolation,FindRoot,Bisection}]]:=Module[{
	Tp,TpFun,TpRule,int,Trange,Tmin,Tmax,printProg,simpleInt,
	target=OptionValue["Target"],method=OptionValue[Method],
	optBis
	},
	optBis=Evaluate@FilterRules[{opt}~Join~Options[FindPercolation]~Join~Options[Bisection],Options[Bisection]]//DeleteDuplicatesBy[#,First]&;
	(* initialize *)
	If[OptionValue[ProgressIndicator],printProg=PrintTemporary[ProgressIndicator[Appearance->"Indeterminate"],"  Estimating \!\(\*SubscriptBox[\(T\), \(p\)]\)"]];
	(* select percolation function *)
	If[V=!=None,
		(* full fractional volume I_\[ScriptCapitalF]\[Congruent]-ln(P_\[ScriptCapitalF]) (double integral) *)
		simpleInt=False;
		TpFun[t_?NumericQ]:=IntegralFalseVacuum[actionFun,t,Tc,vw,V,phases],
		(* simplified expression (single integral) *)
		simpleInt=True;
		TpFun[t_]:=IntegralFalseVacuum[actionFun,t,Tc,vw]
		];
	(* select method *)
	Switch[method,
		FindRoot, (* standard resolution with FindRoot *)
			PT2GWPrint[msg["searchingTp"][<|"simple"->If[simpleInt,"simplified ",""],"target"->target,"method"->findRootLink|>]];
			Tp=(\[ScriptCapitalT]/.First@FindRoot[Log[TpFun[\[ScriptCapitalT]]]==Log[target],{\[ScriptCapitalT],TpGuess}]), (* NB Sensitive to variable name !!! *)
		"Bisection", (* alternative bisection method *)
			PT2GWPrint[msg["searchingTp"][<|"simple"->If[simpleInt,"simplified ",""],"target"->target,"method"->"bisection"|>]];
			Trange=OptionValue["TRange"]//PT2GWEcho; (* in principle we should extract the T range from the action function *)
			If[Length[Trange]=!=2,Message[FindPercolation::TRange,Trange];Return[]];
			Tp=Bisection[Log[TpFun[#]]-Log[target]&,Trange,10^-4(Trange[[2]]-Trange[[1]]),.1,Return->"x",optBis],
		_,
			Message[FindPercolation::method,method];
			Return[]
		];
	If[OptionValue[ProgressIndicator],NotebookDelete[printProg]];
	If[!NumericQ[Tp],PT2GWPrint@msg["NoTperc"]];
	TpRule=T->Tp;
	OptionValue[Return]/.{"TpRule":>TpRule,"Tp":>Tp,"Value":>TpFun[Tp],"Action":>actionFun[Tp]}
]


(* check on percolation *)
PercolationCheck::usage="PercolationCheck[ActionFunction,T,\!\(\*SubscriptBox[\(T\), \(c\)]\),\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\),V,phases] returns a negative number if percolation effectively occured, accounting for the Universe expansion.";
PercolationCheck[Action_,T_,Tc_,vw_,V_,phases_,opt:OptionsPattern[]]:=
H[T,V,phases](3+T ND[IntegralFalseVacuum[Action,t,Tc,vw,V,phases],t,T,opt])


(* ::Subsubsection::Closed:: *)
(*\[Alpha] - strength*)


Alpha::usage="Alpha[T,Potential,Phases] gives the strength of a first order phase transition (NB: Uses numerical derivatives).";
Alpha[T_?NumericQ,V_,phases_]:=Module[{xm1,xm2,x,t},
	{xm1,xm2}=Through[phases[T]];
	1/RadiationEnergyDensity[T] (V[xm1,T]-V[xm2,T] - T/4(ND[V[xm1,t],t,T]-ND[V[xm2,t],t,T]))(*(#/.x->xm1)-(#/.x->xm2)&@(\!\(
\*SubscriptBox[\(\[PartialD]\), \(t\)]\(V[x, t]\)\)/.t->T)))*)
	]


(* ::Subsubsection::Closed:: *)
(*\[Beta] (inverse duration)*)
(*Inverse duration of the PT*)


BetaHubble::usage="BetaHubble[T,ActionFunction] gives the inverse duration in Hubble units of a first-order phase transition.";
BetaHubble[T_,Action:(_Function|_Symbol)]:=(\[ScriptCapitalT] \!\(
\*SubscriptBox[\(\[PartialD]\), \(\[ScriptCapitalT]\)]\(Action[\[ScriptCapitalT]]\)\))/.\[ScriptCapitalT]->T


(* ::Subsection::Closed:: *)
(*GW Plot*)


(* ::Subsubsection::Closed:: *)
(*Detector Sensitivities Plot*)


PlotGWSensitivities::usage="PlotGWSensitivities[{\!\(\*SubscriptBox[\(f\), \(min\)]\),\!\(\*SubscriptBox[\(f\), \(max\)]\)},detectors] plots the sensitivity curves for a given list of detectors.";
$dependencies[PlotGWSensitivities]={GWSensitivities,LogLogPlot};
PlotGWSensitivities[{fmin_,fmax_},detectors_,opt:OptionsPattern[$dependencies[PlotGWSensitivities]]]:=Module[{
	colorList=\!\(TraditionalForm\`RotateLeft[ColorData[54, "\<ColorList\>"], 3]\),colors,optGW,optPlot
	},
	optGW=Evaluate@FilterRules[{opt}~Join~Options[GWSensitivities],Options[GWSensitivities]]//DeleteDuplicatesBy[#,First]&;
	optPlot=Evaluate@FilterRules[{opt},Options[LogLogPlot]]//DeleteDuplicatesBy[#,First]&;
	colors=If[MatchQ[detectors,_String],colorList[[1]],colorList];
	LogLogPlot[Evaluate[detectors/.{det_String:>GWSensitivities[f,det,optGW],All:>GWSensitivities[f,All,optGW]}],
		{f,fmin,fmax},
		optPlot//Evaluate,
		PlotStyle->colors,
		Filling->Top,
		PlotLegends->SwatchLegend[Opacity[.4,#]&/@colors,detectors/.All->GWSensitivities["List"]],
		(*PlotLabels->(detectors/.All->GWSensitivities["List"]),*)
		Frame->True,
		FrameLabel->{"f / "<>$FrequencyUnit,"\!\(\*SuperscriptBox[\(h\), \(2\)]\)\!\(\*SubscriptBox[\(\[CapitalOmega]\), \(GW\)]\)"},
		PlotLabel->"Detector sensitivities",
		GridLines->Automatic
		]/._Line:>Sequence[]
	]
addCodeCompletion["PlotGWSensitivities"][0,GWSensitivities["List"],{"Source"}];


(* ::Subsubsection::Closed:: *)
(*GW plot*)


(* given a function h2\[CapitalOmega](f) *)
PlotGW::usage="PlotGW[\!\(\*SuperscriptBox[\(h\), \(2\)]\)\[CapitalOmega]\] plots the gravitational wave spectra defined by the association \!\(\*SuperscriptBox[\(h\), \(2\)]\)\[CapitalOmega]. The keys should match \"Collisions\", \"Soundwaves\" or \"Turbulence\".
PlotGW[Transition] plots the gravitational wave spectra for the Transition object.";
Options[PlotGW]={
	"FrequencyRange"->Automatic,"h2OmegaRange"->Automatic,
	"Sources"->{"Collisions","Soundwaves","Turbulence"},
	"Detectors"->{"LISA PISC","DECIGO PISC","BBO PISC"},
	"GWPeak"->True
	};
PlotGW[h2Omega_Association,opt:OptionsPattern[{PlotGW,LogLogPlot}]]:=Module[{
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
			Frame->True,
			FrameLabel->{"f / "<>$FrequencyUnit,"\!\(\*SuperscriptBox[\(h\), \(2\)]\)\!\(\*SubscriptBox[\(\[CapitalOmega]\), \(GW\)]\)"},
			PlotLabel->"GW spectra",
			GridLines->Automatic,
			PlotLegends->If[Length[sources]>1,sources,None],
			PlotRange->All(*h2\[CapitalOmega]Range*),
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
		(*If[MatchQ[dets,_List],PlotGWSensitivities[{fmin,fmax},dets,optPlot],Nothing]*)
		PlotGWSensitivities[{fmin,fmax},dets,optPlot]
		}]]
	]
AutoComplete[PlotGW];


(* given a Transition object *)
PlotGW[tr_Transition,opt:OptionsPattern[]]:=Module[{h2Omega},
	h2Omega=Lookup[tr[Association],"h2Omega",
		PT2GWPrint["Missing h2Omega!"];Return[$Failed,Module]];
	PlotGW[h2Omega,opt]
	]


(* ::Subsection::Closed:: *)
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


graphicsOptions:={
	PerformanceGoal->"Speed",
	Background->White,
	Axes->None,
	AxesLabel->None,
	Frame->True,
	FrameTicks->None,
	FrameLabel->None,
	GridLines->None,
	PlotLegends->None,
	PlotLabel->None,
	ImageSize->Dynamic[{Automatic,3.5*CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification]}]
	};


TransitionGraphics[tr_Transition]:=PlotTransition[tr,
	Epilog->Module[{Tn,Tp,phases},{Tn,Tp,phases}=Lookup[tr[Association],{"Tn","Tp","Phases"},tr["Tn"]];
		{Arrowheads[Small],Arrow[{{Tn,phases[[1]][Tn]},{Tp,phases[[2]][Tp]}}]}],
	graphicsOptions
	]


GWGraphics[tr_Transition]:= PlotGW[tr,graphicsOptions]


Transition::usage="Transition[] object represents results from the \!\(\*TemplateBox[{\"SearchPhases\", {\"SearchPotential/ref/SearchPhases\", None}, \"SearchPotential/ref/SearchPhases\"},\n\"HyperlinkDefault\"]\) function."
Transition::noprop="Property `1` of Transition is not available. It should be one of `2`.";

(* object must by applied to an Association *)
(* formatting *)
Dataset[Transition[asc_Association]]^:=Dataset[asc]
Normal[Transition[asc_Association]]^:=asc
(*Association[Transition[asc_Association]]^:=asc*) (* not working: Association has attribute HoldAll *)
(*Lookup[Transition[asc_Association],keys__]^:=Lookup[asc,keys]*) (* not working: Lookup has attribute HoldAll *)
Transition[asc_Association][Dataset]:=Dataset[asc]
Transition[asc_Association][Association]:=asc
Transition[asc_Association][List]:=asc//Normal
Transition[asc_Association][Keys]:=Keys[asc]
Transition[ds_Dataset]:=Transition[ds//Normal]

(* lookup value(s) *)
Transition[asc_Association][keys:(_List|_String),default___,h___]:=Lookup[asc,keys,default,h]

(* box format for output, see https://mathematica.stackexchange.com/questions/77658 *)
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
		BoxForm`SummaryItem[{"\!\(\*SubsuperscriptBox[\(f\), \(GW\), \(peak\)]\): ", Lookup[asc,"fPeak",None,Quantity[#,$FrequencyUnit]&]}],
		BoxForm`SummaryItem[{"\!\(\*SuperscriptBox[\(h\), \(2\)]\)\!\(\*SubsuperscriptBox[\(\[CapitalOmega]\), \(GW\), \(peak\)]\): ", Lookup[asc,"h2OmegaPeak",None]}]
		(*,
		BoxForm`SummaryItem[{"Subscript[T, n]: ", obj["Tn"],obj["Unit"]}],
		BoxForm`SummaryItem[{"Subscript[S, 3]/T(Subscript[T, n]): ", obj["NucleationAction"]}],
		BoxForm`SummaryItem[{"\[CapitalGamma]/H^4(Subscript[T, n]): ", obj["NucleationDecayOverHubble"]}],
		BoxForm`SummaryItem[{"Subscript[S, 3]/T(Subscript[T, p]): ", obj["PercolationAction"]}],		
		BoxForm`SummaryItem[{"f: ", obj["fPeak"]}],
		BoxForm`SummaryItem[{"h^2Subscript[\[CapitalOmega], GW]: ", Length@obj["h2OmegaPeak"]}]*)
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


UpdateTransition::usage="UpdateTransition[transition,key\[Rule]val] updates transition by adding/changing the key-value pair key\[Rule]val.
UpdateTransition[transition,{\!\(\*SubscriptBox[\(key\), \(1\)]\)\[Rule]\!\(\*SubscriptBox[\(val\), \(1\)]\),\!\(\*SubscriptBox[\(key\), \(2\)]\)\[Rule]\!\(\*SubscriptBox[\(val\), \(2\)]\),\[Ellipsis]}] updates transition with a List or Association of key-value pairs.";
UpdateTransition[tr_Transition,asc:(_Association|_List|_Rule)]:=tr=Module[{a=tr[Association]},
AssociateTo[a,asc];Transition[a]]


(* ::Subsubsection::Closed:: *)
(*SearchPhases*)


SearchPhases::usage="SearchPhases[V,{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)},\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\)] finds first order phase transitions(if any) and derive associated gravitational wave spectra from a given particle physics potential V and two phases Subscript[\[Phi], 1,2](T).";
Options[SearchPhases]={
	"TransitionTemperature"->"Tp",
	"TnEstimate"->Automatic,
	"\[CapitalDelta]TFraction"->0.8,
	"ActionFunction"->Automatic,
	"Metadata"->{},
	UpTo->All,
	"PlotGW"->False,
	"Plots"->None
	};
$dependencies[SearchPhases]={SearchPhases,ActionFit,FindNucleation,Bisection,ComputeGW};
SearchPhases[V:(_Function|_Symbol),phases_List,vw_?NumericQ,opt:OptionsPattern[$dependencies[SearchPhases]]]:=Module[{
	phasesTOverlap,Tlow,
	Tc,Tn,Tp,
	\[Alpha],\[Kappa],K,Kstr,\[Beta]H,h2\[CapitalOmega],fPeak,\[CapitalOmega]Peak,
	TcRule,Tn0,STn0,\[CapitalGamma]H40,goodTn0,STn,\[CapitalGamma]H4,Int\[CapitalGamma]H4,Ip,STp,
	actionFit,actionFun,analyticAction,STlist,optAction,optActionFit,optFindTnuc,optFindTperc,optGW,
	\[CapitalDelta]T,Tmin,Tmax,TpCond,
	targetForms,computations,res,out,
	plotAction,plotGW,analyticApply,
	TtransRules,
	Ttrans=OptionValue["TransitionTemperature"],
	optAnalyticAction=OptionValue["ActionFunction"],
	colData=OptionValue["CollisionData"],
	\[CapitalDelta]Tfrac=OptionValue["\[CapitalDelta]TFraction"],
	plots=OptionValue["Plots"],
	metadata=OptionValue["Metadata"]
	},
	optAction=Evaluate@FilterRules[{opt}~Join~Options[Action],Options[Action]]//DeleteDuplicatesBy[#,First]&;
	optActionFit=Evaluate@FilterRules[{opt}~Join~Options[ActionFit],Options[ActionFit]]//DeleteDuplicatesBy[#,First]&;
	optFindTnuc=Evaluate@FilterRules[{opt}~Join~Options[SearchPhases]~Join~Options[FindNucleation],
		Options[FindNucleation]~Join~Options[Bisection]]//DeleteDuplicatesBy[#,First]&;
	optFindTperc=Evaluate@FilterRules[{opt}~Join~Options[FindPercolation],Options[FindPercolation]]//DeleteDuplicatesBy[#,First]&;
	optGW=Evaluate@FilterRules[{opt},Options[ComputeGW]]//DeleteDuplicatesBy[#,First]&;
	plotAction=plots/.{None->False,All->True,{__}->MemberQ[plots,"Action"],_->OptionValue["PlotAction"]};
	plotGW=plots/.{None->False,All->True,{__}->MemberQ[plots,"GW"],_->OptionValue["PlotGW"]};
	(* extract options for analytic action *)
	analyticAction=First[optAnalyticAction/.x_->{x}//Flatten];
	analyticApply=If[analyticAction=!=Automatic,"ApplyTo"/.({Rest[{optAnalyticAction}//Flatten]}//Flatten)/."ApplyTo"->{"TnEstimate","Tn","Tp"},{}];
	
	(* preliminary tests *)
	TtransRules={"Tc":>Tc,"Tn":>Tn,"Tp":>Tp};
	If[FreeQ[Keys@TtransRules,Ttrans],PT2GWPrint[msg["TransitionTemperature"][Ttrans]];Ttrans="Tp"];
	If[!Between[vw,{0,1}],PT2GWPrint[msg["WallVelocity"][vw]];Return[<||>]];
	(* check: computation result must match these forms *)
	targetForms={
		"Unit" -> _String, "RelativisticDOF" -> _?NumericQ, "Phases" -> _List, "WallVelocity" -> _Real,
		"Tc" -> _Real, "TnEstimate" -> _Real, "ActionFunction" -> _ActionFunction|_Function|_Symbol,
		"Tn" -> _Real, "Tp" -> _Real,"Domain" -> {_?NumericQ,_?NumericQ},
		"\[Alpha]" -> _Real, "K"->_Real, "Kstr"->_Real, "\[Kappa]"->_Real, "\[Beta]/H" -> _Real, "TpCondition" -> _Real,
		"fPeak"->_Real, "h2OmegaPeak"->_Real,
		"NucleationAction" -> _Real, "NucleationDecayOverHubble" -> _Real, "NucleationIntegralDecay"->_Real,
		"PercolationAction" -> _Real, "PercolationIntegralValue" -> _Real
		};
	(* list the computation sequence *)
	computations={
		(* input data *)
		"Unit":>(
			(* Metadata Keys must be Strings, for proper Dataset structure *)
			If[!ContainsOnly[Head/@Keys@metadata,{String}],
				PT2GWPrint[msg["MetaKeys"][Cases[Keys@metadata,Except[_String]]]];
				metadata=KeyMap[ToString,Association[metadata]]
				];
			AssociateTo[out,metadata];
			$Unit),
		"RelativisticDOF":>RelativisticDOF,
		"Phases":>phases,
		"WallVelocity":>vw,
		(* Search for phase transitions *)
		"Tc":>(
			Tc=FindCritical[V,phases];
			If[Tc==None,Throw[out]]; (* Run the following only if critical temperature is found *)
			PT2GWPrint["Found transition at critical temperature"];
			PT2GWEcho[Tc,"\!\(\*SubscriptBox[\(T\), \(c\)]\)  \[Rule]",Quantity[#,$Unit]&]
			),
		(* Tn: analytic vs numerical bisection method *)
		"TnEstimate":>(
			(* overlap region between the 2 phases *)
			phasesTOverlap=Overlap[phases];
			Tlow=phasesTOverlap[[1]];
			Which[
				(* input value for Tn estimate *)
				NumericQ[Tn0=OptionValue["TnEstimate"]]\[And](If[Between[Tn0,{0,Tc}],True,PT2GWPrint@msg["badTnEst"][Tn0,Tc];False]),
				STn0=Action[Tn0,V,phases];
				\[CapitalGamma]H40=DecayOverHubble[Tn0,STn0,V,phases,optAction],
				(* analytic Tn estimate *)
				MemberQ[analyticApply,"TnEstimate"],
				PT2GWPrint[msg["analyticAction"]];
				{Tn0,STn0,\[CapitalGamma]H40}=FindNucleation[V,{Tlow,Tc},phases,"NucleationMethod"->{"ActionFunction"->analyticAction},
					Return->{"Tn","NucleationAction","NucleationDecayOverHubble"}],
				(* numerical Tn estimate *)
				True,
				{Tn0,STn0,\[CapitalGamma]H40}=FindNucleation[V,{Tlow,Tc},phases,
					Return->{"Tn","NucleationAction","NucleationDecayOverHubble"},Print->True,optFindTnuc
					];
				];
			If[!NumericQ[Tn0],Throw@out];
			Tn0),
		"ActionFunction":>(
			If[NumericQ[Tn0],
				PT2GWEcho[msg["Tn"][<|"Tn"->Quantity[Tn0,$Unit],"STn"->STn0,"GammaH4"->\[CapitalGamma]H40|>],"\!\(\*SubsuperscriptBox[\(T\), \(n\), \(estimate\)]\)  \[Rule]"]
				];
			(* Action function: analytic vs fitting method *)
			actionFit=If[ContainsAll[analyticApply,{"Tn","Tp"}],
				(* analytic action function *)
				{Tmin,Tmax}=Check[RegionBounds@ImplicitRegion[FunctionDomain[{analyticAction[T],0<T<Tc},T],T]//First,{Tlow,Tc}];
				ActionFunction[<|
					"Function"->analyticAction,
					"ActionMethod"->"Analytic",
					"Domain"->{Tmin,Tmax},
					"Tc"->Tc,
					"Unit"->$Unit
					|>],
				(* numerical fit of the action function *)
				PT2GWPrint["Fitting action..."];
				\[CapitalDelta]T=\[CapitalDelta]Tfrac(Tc-Tn0);
				Tmin=Tn0-\[CapitalDelta]T;
				Tmax=Tn0+\[CapitalDelta]T;
				(* refine T range *)
				Tlow+=10^-6 Min[\[CapitalDelta]T,Tn0-Tlow]; (* phases are undetermined at Tlow: numerical shift  *)
				Off[Reduce::ratnz];                  (* Reduce complains with Real number input *)
				{Tmin,Tmax}=RegionBounds[ImplicitRegion[Reduce[{Tmin<t<Tmax,t>Tlow},t],t]]//First;
				On[Reduce::ratnz];
				ActionFit[V,phases,{Tmin,Tmax},Tc,"PlotAction"->False,optActionFit]
				];
			PT2GWEcho[actionFit,"Action function \[Rule]"]),
		"Domain":>{Tmin,Tc},
		"Tn":>(
			(* Tn: analytic vs fitting method *)
			actionFun=If[MemberQ[analyticApply,"Tn"],analyticAction,actionFit["Function"]];
			{Tn,STn,\[CapitalGamma]H4,Int\[CapitalGamma]H4}=FindNucleation[V,{Tmin,Tmax},phases,
				Return->{"Tn","NucleationAction","NucleationDecayOverHubble","NucleationIntegralDecay"},
				"NucleationCriterion"->"IntegralDecay",
				"NucleationMethod"->{"ActionFunction"->actionFun,"Tc"->Tc,"TnEstimate"->Tn0},
				optFindTnuc,
				Print->True
				];
			(* nucleation temperature Tn *)
			If[!NumericQ[Tn],Throw@out];
			If[Abs[Log10@Int\[CapitalGamma]H4]>1,PT2GWPrint@msg["NoTnuc"];Throw@out];
			Tn),
		"NucleationAction":>(
			PT2GWEcho[msg["Tn"][<|"Tn"->Quantity[Tn,$Unit],"STn"->STn,"GammaH4"->\[CapitalGamma]H4,"IntGammaH4"->Int\[CapitalGamma]H4|>],"\!\(\*SubscriptBox[\(T\), \(n\)]\)  \[Rule]"];
			STn),
		"NucleationDecayOverHubble":>\[CapitalGamma]H4,
		"NucleationIntegralDecay":>Int\[CapitalGamma]H4,
		"Tp":>(
			PT2GWPrint["Computing phase transition parameters..."];
			actionFun=If[MemberQ[analyticApply,"Tp"],analyticAction,actionFit["Function"]];
			{Tp,Ip,STp}=FindPercolation[actionFun,{Tn,Tc},vw,V,phases,Return->{"Tp","Value","Action"},optFindTperc];
			If[!NumericQ[Tp],Throw@out];
			PT2GWEcho[Tp,"\!\(\*SubscriptBox[\(T\), \(p\)]\)  \[Rule]",Quantity[#,$Unit]&]
			),
		"PercolationAction":>STp,
		"PercolationIntegralValue":>Ip,
		"TpCondition":>(TpCond=PercolationCheck[actionFun,Tp,Tc,vw,V,phases,Scale->10.^-3 (Tp-Tmin)];
			PT2GWEcho[TpCond,"Percolation condition:",Row@{If[#<0,Style["satisfied",Darker@Green],Style["violated",Red]]," (",#,")"}&]
			),
		"TransitionTemperature":>PT2GWEcho[Ttrans,"\!\(\*SubscriptBox[\(T\), \(transition\)]\) \[Rule]"],
		(* phase transition parameters *)
		"\[Alpha]":>(
			Ttrans=Ttrans/.TtransRules; (* numerical value *)
			(* optional plot *)
			If[plotAction,Print@PlotAction[Transition[out]]];
			\[Alpha]=PT2GWEcho[Alpha[Ttrans,V,phases],"\[Alpha] \[Rule] "]
			),
		"\[Beta]/H":>(\[Beta]H=PT2GWEcho[BetaHubble[Ttrans,actionFun],"\[Beta]/H \[Rule] "]),
		(* compute and append GW data *)
		"fPeak":>(
			If[MatchQ[colData,_Association|_List],AppendTo[colData,"Potential"->V]];
			AssociateTo[out,ComputeGW[out,"CollisionData"->colData,optGW]];
			h2\[CapitalOmega]=out["h2Omega"]; (* h2Omega association must be defined! *)
			If[plotGW,Print@PlotGW[h2\[CapitalOmega]]];
			(* GW peak *)
			{fPeak,\[CapitalOmega]Peak}=Module[{logf},E^{logf/.#[[2]],#[[1]]}&@NMaximize[{Log[h2\[CapitalOmega]["Combined"][E^logf]]},logf,Method->"NelderMead"]];
			PT2GWEcho[fPeak,"\!\(\*SuperscriptBox[\(f\), \(peak\)]\)  \[Rule]",Quantity[#,$FrequencyUnit]&]
			),
		"h2OmegaPeak":>PT2GWEcho[\[CapitalOmega]Peak,"\!\(\*SuperscriptBox[\(h\), \(2\)]\)\!\(\*SubsuperscriptBox[\(\[CapitalOmega]\), \(GW\), \(peak\)]\) \[Rule] "]
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
					PT2GWPrint[msg["computations"][comp,out[comp],comp/.targetForms]];
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
AutoComplete[SearchPhases,$dependencies[SearchPhases]];


(* ::Subsubsection::Closed:: *)
(*SearchPotential*)


(* ::Text:: *)
(*PT and GW parameters (full potential)*)


SearchPotential::usage="SearchPotential[V,\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\)] runs the full BSM\[Rule]GW pipeline for a given temperature-dependent scalar potential and bubble wall velocity \!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\).
SearchPotential[V] sets the wall velocity to \!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\)=0.9.";
Options[SearchPotential]={"PrintNoCritical"->False,Dataset->False,"Plots"->None};
$dependencies[SearchPotential]={SearchPotential,SearchPhases,TracePhases,ActionFit,FindNucleation,Bisection,ComputeGW};
SearchPotential[V_,Optional[vw_/;NumericQ[vw],.9],opt:OptionsPattern[$dependencies[SearchPotential]]]:=Module[{
	transitions,trans,xm,xmFuns,phases,
	optTrace,optSearchPh,
	plotPhaseDiagram,
	plots=OptionValue["Plots"],
	dataset=OptionValue[Dataset]
	},
	(* initialize transitions *)
	transitions=If[dataset,Dataset[{}],{}];
	(* extract options *)
	optTrace=Evaluate@FilterRules[{opt}~Join~Options[TracePhases],Options[TracePhases]]//DeleteDuplicatesBy[#,First]&;
	optSearchPh=Evaluate@FilterRules[{opt}~Join~Options[SearchPhases],Union@@Options/@$dependencies[SearchPhases]]//DeleteDuplicatesBy[#,First]&;
	plotPhaseDiagram=plots/.{None->False,All->True,{__}->MemberQ[plots,"PhaseDiagram"],_->OptionValue["PlotPhaseDiagram"]};
	(* phase tracing *)
	phases=TracePhases[V,"PlotPhaseDiagram"->plotPhaseDiagram,optTrace]//Simplify;
	(* only one phase found *)
	If[Length[phases]<2,PT2GWPrint@msg["SinglePhase"];Return[If[dataset,Dataset[{}],{}]]];
	If[\[Not]OptionValue["PrintNoCritical"],Off[FindCritical::empty,FindCritical::noOverlap]];
	(* loop over pairs of phases *)
	PT2GWPrint@msg["loopPhases"];
	Do[
		trans=SearchPhases[V,phaseCouple,vw,optSearchPh];
		If[MatchQ[trans,_Transition],
			PT2GWEcho[trans,"Transition \[Rule]"];
			transitions=AppendTo[transitions,If[dataset,trans[Dataset],trans]]
			],
		{phaseCouple,Subsets[phases,{2}]}
		];
	If[Normal@transitions=={},PT2GWPrint@msg["NoTran"]];
	transitions
	]
AutoComplete[SearchPotential,$dependencies[SearchPotential]];


(* ::Section::Closed:: *)
(*End Package*)


End[]; (*"`Private`"*)


(* ReadProtected attribute on public symbols prevents rendering of huge box with all 
definitions (DownValues) when they are called in Information or with shortcut ?PT2GW. *)
SetAttributes[Evaluate@Names["`*"],{ReadProtected}]; (* from FindBounce.m *) 


EndPackage[]
