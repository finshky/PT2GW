(* ::Package:: *)

(* ::Title:: *)
(*Coupled fluid-field model*)


(* ::Input::Initialization::Plain:: *)
Quit


(* ::Code::Initialization::Plain:: *)
(* !for pre-release only! *)
pacletDir="/home/marco/Aveiro/Nerdy/Mathematica nbs/PTFB/PT2GW/";
(* pacletDir="/home/marco/Aveiro/Nerdy/Mathematica nbs/PTFB/Backup/TBounce_24-12-20" *)
PacletDirectoryLoad[pacletDir]


(* ::Chapter:: *)
(*Initialize*)


(* ::Text:: *)
(*Load PT2GW package and Example notebook:*)


(* ::Input::Initialization::Plain:: *)
<<PT2GW`
<<PT2GW/Examples.m


(* ::Text:: *)
(*The  coupled fluid-field model is defined by the potential*)


(* ::Input::Initialization::Plain:: *)
CFFModel[][\[Phi],T]


(* ::Text:: *)
(*Load the nth benchmark*)


(* ::Input::Initialization::Plain:: *)
idx=3;
V=CFFModel[idx];
V[\[Phi],T]


(* ::Section::Closed:: *)
(*Define parameters*)


(* ::Text:: *)
(*Define bubble wall velocity*)


(* ::Input::Initialization::Plain:: *)
vw=.9; (* units of c *)


(* ::Subsection::Closed:: *)
(*Auxiliary parameters*)


(* ::Text:: *)
(*Define energy units (defaults to GeV), belonging to the*)
(*- field*)
(*- temperature*)
(*- Planck mass (entering the Hubble parameter)*)


(* ::Input::Initialization::Plain:: *)
DefineUnits["GeV"]


(* ::Text:: *)
(*Number of relativistic degrees of freedom (defaults to SM value at the electroweak scale)*)


(* ::Input::Initialization::Plain:: *)
RelativisticDOF=106.75; (* RelativisticDOF is a PT2GW package variable *)


(* ::Text:: *)
(*Optional: define data required to compute the bubble wall collision efficiency \[Kappa]col*)


(* ::Input::Initialization::Plain:: *)
(* this is the minimal data necessary to compute Subscript[\[Kappa], col] *)
colData={"GaugeCouplings"->{0.1},"MassFunctions"->{"Bosons"->{MassFunction[V]}},"T0"->"Tn","T"->"Tp"};


(* ::Input::Initialization::Plain:: *)
(* alternatively, one may define directly Subscript[\[Kappa], col] *)
(* colData={"\[Kappa]col"->0.1} *)


(* ::Chapter:: *)
(*SearchPotential*)


transitions=SearchPotential[V,vw,
	"TracingMethod"->NSolve,
	"CollisionData"->None (* colData *),
	"Metadata"->CFFModel[idx,"Benchmark"]//Normal (* optional: include metadata. Here we include the benchmark parameters. *),
	"Plots"->{"PhaseDiagram"}
	];//EchoTiming


(* ::Text:: *)
(*To run a search for phase transitions, it is sufficient to provide the potential and bubble wall velocity*)


(* ::Section:: *)
(*Inspect transition*)


(* ::Text:: *)
(*Extract  transition  from  list*)


(* ::Input::Initialization::Plain:: *)
tr=transitions[[1]]
AutoComplete[tr]; (* optional: quickly search keys from a drop-down menu *)


(* ::Text:: *)
(*Inspect transition elements*)


(* ::Input::Initialization::Plain:: *)
tr["ActionFunction"]


(* ::Text:: *)
(*The  key - search  for  Transitions  behaves  like the built-in  Lookup*)


(* ::Input::Initialization::Plain:: *)
tr[{"\[Alpha]","\[Beta]/H","not a key"},"missing",Log10]


(* ::Text:: *)
(*Display all data as Dataset*)


(* ::Input::Initialization::Plain:: *)
tr//Dataset


(* ::Text:: *)
(*Get the underlying Association*)


(* ::Input::Initialization::Plain:: *)
tr//Normal


(* ::Section:: *)
(*Plots*)


(* ::Text:: *)
(*Plot a phase diagram for the transition*)


(* ::Input::Initialization::Plain:: *)
PlotTransition[tr]


(* ::Text:: *)
(*The action vs temperature, and the transition milestones*)


(* ::Input::Initialization::Plain:: *)
PlotAction[tr]


(* ::Text:: *)
(*The gravitational wave spectra*)


(* ::Input::Initialization::Plain:: *)
PlotGW[tr]


(* ::Chapter:: *)
(*PT2GW Internal functions*)


(* ::Text:: *)
(*All PT2GW functions feature usage messages*)


(* ::Input::Initialization::Plain:: *)
?SearchPhases


(* ::Text:: *)
(*A   template  for  the  function  arguments  can  be  pasted  from  the  popup  menu  (Ctrl + Shift + K, Cmd + Shift + K  on  Mac)*)


(* ::Input::Initialization::Plain:: *)
SearchPhases[\!\(\*
TagBox[
FrameBox["V"],
"Placeholder"]\),{\!\(\*
TagBox[
FrameBox[
SubscriptBox["\[Phi]", "1"]],
"Placeholder"]\),\!\(\*
TagBox[
FrameBox[
SubscriptBox["\[Phi]", "2"]],
"Placeholder"]\)},\!\(\*
TagBox[
FrameBox[
SubscriptBox["\[Xi]", "w"]],
"Placeholder"]\)]


(* ::Text:: *)
(*To  list  all  PT2GW  functions , use*)


(* ::Input::Initialization::Plain:: *)
?PT2GW`*


(* ::Text:: *)
(*In the following, we may either use the transition object computed in SearchPotential, or load it from the examples:*)


(* ::Code::Initialization::Plain:: *)
tr=CFFModel[idx,"Transition"]
AutoComplete[tr];


(* ::Section:: *)
(*Potential*)


(* ::Text:: *)
(*Plot the potential, with dynamic temperature and field ranges*)


(* ::Input::Initialization::Plain:: *)
PlotPotential[V,5 10.^2{-1,4},100.,"LogTRange"->2.5,"Log\[Phi]Range"->1.]


(* ::Text:: *)
(*Add sliders for the potential parameters*)


(* ::Input::Initialization::Plain:: *)
pot[\[Phi]_,T_,{\[Gamma]_,A_,\[Lambda]_,T0_}]:=CFFModel[][\[Phi],T]/.{"\[Gamma]"->\[Gamma],"A"->A,"\[Lambda]"->\[Lambda],"T0"->T0}


(* ::Input::Initialization::Plain:: *)
Manipulate[Plot[pot[\[Phi],T,{\[Gamma],A,\[Lambda],T0}],{\[Phi],-\[Phi]range,2\[Phi]range}],
{{\[Phi]range,10^3},10,10^4},
{{T,200.},100.,300.},
{{\[Gamma],"\[Gamma]"},0.,.5},
{{A,"A"},0.,.1},
{{\[Lambda],"\[Lambda]"},0.,.1},
{{T0,"T0"},0.,200.}
]/.Normal[CFFModel[3,"Benchmark"]]//N


(* ::Section:: *)
(*Phase tracing*)


(* ::Text:: *)
(*Trace the phases semi-analytically with NSolve:*)


(* ::Input::Initialization::Plain:: *)
Phases=TracePhases[V,"TracingMethod"->NSolve]//
Echo[#,"Phases \[Rule]",TableForm]&;


(* ::Code::Initialization::Plain:: *)
(* alternatively, use numerical methods *)
(*Phases=TracePhases[V,"TracingMethod"->"Numeric","TRange"->{0,300}]//
Echo[#,"Phases \[Rule]",TableForm]&;*)


(* ::Text:: *)
(*Only the  1st and 3rd phase feature a transition with a temperature overlap*)


(* ::Code::Initialization::Plain:: *)
With[{phaseCouples=Subsets[Phases,{2}]},
	FindCritical[V,#]&/@phaseCouples//
	TableForm[#,TableHeadings->{Subsets[Range[Length[Phases]],{2}]}]&
	]


(* ::Code::Initialization::Plain:: *)
PlotPhases[tr["Phases"]]


(* ::Section:: *)
(*ActionFit*)


(* ::Text:: *)
(*Compute  action (calling FindBounce)*)


(* ::Input::Initialization::Plain:: *)
Action[tr["Tn"],V,tr["Phases"]]


(* ::Text:: *)
(*Compute  Euclidean  action  (calling  FindBounce)  and  fit  the  action - temperature  functionS_ 3/T  (T)*)


(* ::Input::Initialization::Plain:: *)
actionFunction=ActionFit[V,tr["Phases"],{145.,180.},tr["Tc"],
	"Data"->None (*actionFunction["Data"] optional: pass action data if already computed *),
	"Orders"->Range[-2,1] (* set the orders used in the Laurent polynomial fit function: \[Sum]_n c_n(Tc-T)^n *)
	]


(* ::Text:: *)
(*Compare fit functions*)


(* ::Code::Initialization::Plain:: *)
fits=Table[ActionFit[V,tr["Phases"],actionFunction["Domain"],tr["Tc"],"Data"->actionFunction["Data"],options],{options,{
	{},
	{"Orders"->Range[-3,0]},
	{"ActionMethod"->Interpolation}
	}}];
LogPlot[Through[Through[fits["Function"]][T]]//Evaluate,{T,149.,198.},
	PlotStyle->{Dashing[None],Dashed,Dashed},
	PlotLegends->{"default","orders\[Rule](-3,0)","\"ActionMethod\"\[Rule]Interpolation"},
	GridLines->Automatic
	]


(* ::Section:: *)
(*Nucleation temperature*)


(* ::Text:: *)
(*Compute the nucleation rate*)


(* ::Input::Initialization::Plain:: *)
DecayRate[tr["Tn"],V,tr["Phases"]]


(* ::Text:: *)
(*Plot  \[CapitalGamma]/H^4 (T)*)


(* ::Input::Initialization::Plain:: *)
DiscretePlot[DecayOverHubble[T,V,tr["Phases"]],{T,Subdivide[tr["TnEstimate"]-1,tr["TnEstimate"]+1,6]},
	AxesLabel->{"T","\[CapitalGamma]/\!\(\*SuperscriptBox[\(H\), \(4\)]\)"},PlotLabel->"Nucleation estimate",
	"ScalingFunctions"->"Log",Joined->True,
	Epilog->{Dashed,InfiniteLine[{0,Log@1},{1,0}],Darker@Orange,InfiniteLine[{tr["TnEstimate"],1},{0,1}]}
	]


(* ::Text:: *)
(*Plot the nucleation criterion integral (requires the action function)*)


(* ::Input::Initialization::Plain:: *)
actionFunction=tr["ActionFunction"]


(* ::Input::Initialization::Plain:: *)
DiscretePlot[IntegralDecay[T,tr["Tc"],actionFunction["Function"],V,tr["Phases"]],{T,Subdivide[tr["Tn"]-1,tr["Tn"]+1,6]},
AxesLabel->{"T","\[Integral]dT/T \[CapitalGamma]/\!\(\*SuperscriptBox[\(H\), \(4\)]\)"},PlotLabel->"Nucleation criterion",
"ScalingFunctions"->"Log",Joined->True,Epilog->{Dashed,InfiniteLine[{0,Log@1},{1,0}],Orange,InfiniteLine[{tr["Tn"],1},{0,1}]}]


(* ::Text:: *)
(*Estimate nucleation temperature*)


(* ::Input::Initialization::Plain:: *)
(* bisection method on \[CapitalGamma]/H^4\[TildeTilde]1 criterion (default) *)
FindNucleation[V,{140.,180.},tr["Phases"],"PrintIterations"->False,
	Return->{"Tn","NucleationDecayOverHubble"}
	(*"NucleationCriterion"->{"ActionValue"->140.}*)
	]//EchoTiming


(* ::Text:: *)
(*For the CFF model, the nucleation can be estimated analytically, by expanding the action around*)
(*- the critical temperature Tc, and*)
(*- and the inflection point T0*)


NucleationCFF[V,140.] (* S_3/T\[TildeTilde]140. for a transition at the EW scale *)


(* ::Text:: *)
(*Compute precise value with integral nucleation criterion*)


(* ::Input::Initialization::Plain:: *)
(* integral decay criterion, requiring action function *)
FindNucleation[V,{140.,180.},tr["Phases"],"PrintIterations"->True,
	"NucleationCriterion"->"IntegralDecay",
	"NucleationMethod"->{"ActionFunction"->tr["ActionFunction"]["Function"],"Tc"->tr["Tc"],"TnEstimate"->tr["TnEstimate"]},
	Return->"Tn"
	(*"NucleationCriterion"->{"ActionValue"->140.}*)
	]//EchoTiming


(* ::Section:: *)
(*Percolation temperature*)


(* ::Text:: *)
(*Plot I_F=-log(P_F), where P_F is the false-vacuum fractional volume (requires the action function).*)
(*The percolation temperature corresponds to I_F(T_p)\[TildeEqual]0.34.*)


(* ::Input::Initialization::Plain:: *)
actionFunction=tr["ActionFunction"]


(* ::Input::Initialization::Plain:: *)
DiscretePlot[IntegralFalseVacuum[actionFunction["Function"],T,tr["Tc"],vw,V,tr["Phases"]],
	{T,Subdivide[tr["Tp"]-1,tr["Tp"]+1,6]},
	AxesLabel->{"T","\!\(\*SubscriptBox[\(I\), \(FV\)]\)"},PlotLabel->"False vacuum decay integral",
	"ScalingFunctions"->"Log",Joined->True,
	Epilog->{Dashed,InfiniteLine[{0,Log@.34},{1,0}],Darker@Yellow,InfiniteLine[{tr["Tp"],1},{0,1}]}
	]


(* ::Text:: *)
(*Determine  the  percolation  temperature  by  solving  I_F(T_p) = 0.34*)


(* ::Input::Initialization::Plain:: *)
FindPercolation[actionFunction["Function"],{tr["Tn"],tr["Tc"]},vw,V,tr["Phases"]]


(* ::Section:: *)
(*Transition parameters*)


(* ::Text:: *)
(*Compute the transition strength \[Alpha]*)


(* ::Input::Initialization::Plain:: *)
Alpha[tr["Tp"],V,tr["Phases"]]
Echo[tr["\[Alpha]"],"precomputed \[Rule]"];


(* ::Text:: *)
(*Compute the inverse duration in Hubble units \[Beta]/H(T*)*)


(* ::Input::Initialization::Plain:: *)
BetaHubble[tr["Tp"],tr["ActionFunction"]["Function"]]
Echo[tr["\[Beta]/H"],"precomputed \[Rule]"];


(* ::Section:: *)
(*SearchPhases*)


(* ::Text:: *)
(*Search for phase transitions given a potential and two of its phases*)


Phases=TracePhases[V,"TracingMethod"->NSolve]
phases=Phases[[{1,3}]];


(* ::Text:: *)
(*SearchPhases wraps the functions above:*)
(*- FindCritical*)
(*- ActionFit*)
(*- FindNucleation*)
(*- FindPercolation*)
(*and computes the GW spectr*)


(* ::Input::Initialization::Plain:: *)
tr=SearchPhases[V,phases,vw,
	"NActionPoints"->31, (* set the number of computations of the action *)
	"CollisionData"->None (* colData *),
	"Metadata"->CFFModel[idx,"Benchmark"]//Normal, (* include meta data: here, we save the benchmark parameters *)
	"Plots"->{"Action"},
	ProgressIndicator->True
];//EchoTiming


(* ::Input::Initialization::Plain:: *)
AutoComplete[tr]; (* optional: quickly access Transition keys from a drop-down menu *)


(* ::Section:: *)
(*Gravitational waves*)


(* ::Text:: *)
(*To compute the GW spectra from a set of phase transition parameters, we first load a Transition object*)
(*(this already contains GW spectral parameters, but we will recompute them)*)


(* ::Input::Initialization::Plain:: *)
tr=CFFModel[idx,"Transition"]
AutoComplete[tr];


(* ::Subsection:: *)
(*Compute GW spectra*)


(* ::Text:: *)
(*ComputeGW is defined in the GW` sub-package. It computes  GW spectra, requiring the following transition parameters:*)


ComputeGW[\!\(\*
TagBox[
FrameBox["T"],
"Placeholder"]\),\!\(\*
TagBox[
FrameBox["\[Alpha]"],
"Placeholder"]\),\!\(\*
TagBox[
FrameBox["\<\"\[Beta]/H\"\>"],
"Placeholder"]\),\!\(\*
TagBox[
FrameBox[
SubscriptBox["\[Xi]", "w"]],
"Placeholder"]\),\!\(\*
TagBox[
FrameBox["g"],
"Placeholder"]\),\!\(\*
TagBox[
FrameBox["$Unit"],
"Placeholder"]\)]


(* ::Text:: *)
(*When a Transition (or Association) object is passed as argument, the parameters are automatically extracted.*)
(*NB Missing arguments given an error:*)


ComputeGW[KeyDrop[tr//Normal,"\[Alpha]"]]//Dataset


(* ::Text:: *)
(*ComputeGW  returns  an  Association  of  GW  parameters, and  stores  it  in  the  GWData symbol, from the GW` sub-package*)


(* ::Input::Initialization::Plain:: *)
ComputeGW[tr//Normal]//Dataset


(* ::Text:: *)
(*The  spectra, corresponding to the key "h2Omega", can be plotted with PlotGW*)


(* ::Input::Initialization::Plain:: *)
PlotGW[GWData["h2Omega"]]


(* ::Subsection:: *)
(*Collision efficiency \[Kappa]col*)


(* ::Text:: *)
(*To add the contribution from bubble wall collisions, we must include, to the least, the following information:*)
(*- a bosonic mass function*)
(*- a gauge coupling value*)
(*- characteristic temperatures for bubble growth, T0 and T* (see 1903.09642)*)


(* ::Input::Initialization::Plain:: *)
(* MassFunction is just the square root of the second field-derivative: \[PartialD]_\[Phi]^2 V(\[Phi],T) *)
m\[Phi]=MassFunction[V];
m\[Phi][\[Phi],T]


(* ::Input::Initialization::Plain:: *)
Plot[m\[Phi][#[T],T]&/@tr["Phases"]//Evaluate,{T,100,200}]


(* ::Text:: *)
(*The minimal collision data is then*)


(* ::Input::Initialization::Plain:: *)
colData={"GaugeCouplings"->{0.1},"MassFunctions"->{"Bosons"->{m\[Phi]}},"T0"->tr["Tn"],"T"->tr["Tp"]};


(* ::Text:: *)
(*Finally, to compute \[Kappa]col we must include the transition parameters and potential*)


data=Append[KeyDrop[tr[Association],"\[Kappa]col"],colData~Join~{"Potential"->V}];


(* ::Text:: *)
(*kappaCollision returns the collision efficiency and, optionally, the transition parameters computed along the way*)


kappaCollision[data,Return->{"\[Kappa]col","Data"}]//Dataset


(* ::Subsection:: *)
(*Detector sensitivities*)


(* ::Text:: *)
(*Several peak-integrated (PISCs) and power-law-integrated sensitivity curves (PLISCs) are built-in PT2GW:*)


PlotGWSensitivities[10^{-9,4},All]


(* ::Text:: *)
(*To add a user-defined detector sensitivity, simply  define the corresponding PISC (peak-integrated sensitivity curve), as a function of frequency in Hertz*)


(* ::Input::Initialization::Plain:: *)
PISC[f_,"myDetector"]:=10^-22 (10^3 f^2+10^-3 f^-2)


(* ::Input::Initialization::Plain:: *)
PlotGW[tr,"Detectors"->{"myDetector"}]


DownValues[GWSensitivities]


(* ::Section:: *)
(*Parameter-space scan*)


$PT2GWPrint=False;


(* ::Text:: *)
(*We may run simple parameter scans.*)
(*To construct a Dataset of transitions, we first define a function to run single a benchmark with option Dataset->True,*)
(*and append it to a dataset DS.*)


(* suppress the printing of computation status *)
$PT2GWPrint=False;


(* separation string *)
sep=StringRepeat["\[Placeholder]\[SelectionPlaceholder]",30];


(* run a benchmark and add the found transitions to DataFrame *)
run[benchmarkValues_,opt:OptionsPattern[]]:=Module[{},
	If[!MatchQ[DS,_Dataset],DS=Dataset[{}]];
	Print@sep;
	Echo[KeyTake[benchmarkValues,{"T0","\[Gamma]","A","\[Lambda]"}],"Running CFF model at \[Rule]"];
	V=CFFModel[]/.benchmarkValues;
	ds=SearchPotential[V,"WallVelocity"/.benchmarkValues,
		Dataset->True,
		opt,
		"TracingMethod"->NSolve,
		"CollisionData"->benchmarkValues["CollisionData"],
		"Metadata"->KeyTake[benchmarkValues,{"T0","\[Gamma]","A","\[Lambda]"}],
		"Plots"->None,
		ProgressIndicator->False
		]//EchoTiming;
	If[ds=!=Dataset[{}],DS=DS~Join~ds]
	]


(* ::Text:: *)
(*Here, we run the benchmark parameters stored in CFFModel[All,"Benchmark"]*)


DS=.;
Scan[run,
	CFFModel[All,"Benchmark"]//Normal
	]//EchoTiming


(* ::Text:: *)
(*With the Dataset format, it is easy to compare the parameters of different transitions*)


DS


(* ::Text:: *)
(*Alternatively, we may obtain a list of Transition objects with Table.*)
(*Here we recompute the transitions above, corresponding to the ones stored in CFFModel[All,"Transition"]*)
(*(the following command should take a few minutes to evaluate)*)


(* ::Input::Initialization::Plain:: *)
transitions=Table[
With[{benchmarkValues=CFFModel[n,"Benchmark"]//Normal,V=CFFModel[n]},
	Print[sep];
	Echo[KeyTake[benchmarkValues,{"T0","\[Gamma]","A","\[Lambda]"}],"Running benchmark \[Rule]"];
	RelativisticDOF="RelativisticDOF"/.benchmarkValues;
	SearchPotential[V,"WallVelocity"/.benchmarkValues,"TracingMethod"->NSolve,
	"CollisionData"->benchmarkValues["CollisionData"],
	"Metadata"->KeyTake[benchmarkValues,{"T0","\[Gamma]","A","\[Lambda]"}],
	"Plots"->None,
	ProgressIndicator->False
	]//Quiet//EchoTiming 
	],
{n,Length@CFFModel[All,"Benchmark"]}]//Flatten


(* ::Text:: *)
(*Similarly, we may inspect selected transition parameters*)


(* ::Input::Initialization::Plain:: *)
With[{labels={"Tc","Tn","Tp","\[Alpha]","\[Beta]/H","fPeak","h2OmegaPeak"}},
Through[transitions[ labels]]//TableForm[#,TableHeadings->{Automatic,labels}]&]


(* ::Subsection:: *)
(*Parallelization*)


(* ::Section:: *)
(*Load/Save data*)


(* ::Subsection:: *)
(*Load the coupled fluid-field model*)


(* ::Text:: *)
(*The CFFModel is defined as*)


(* ::Input::Initialization::Plain:: *)
CFFModel[][\[Phi],T]


(* ::Text:: *)
(*You  can load specific benchmarks by passing an index argument*)


(* ::Input::Initialization::Plain:: *)
idx=3;
V=CFFModel[idx];
V[\[Phi],T]


(* ::Text:: *)
(*To list the default benchmark parameters, use the "Benchmark" argument*)


(* ::Input::Initialization::Plain:: *)
CFFModel[All,"Benchmark"]


(* ::Text:: *)
(*Benchmark parameters can be defined as Associations*)


(* ::Input::Initialization::Plain:: *)
bp=<|"T0"->140,"\[Gamma]"->1/18,"A"->Sqrt[5/2]/36,"\[Lambda]"->5/324|>;
V=CFFModel[]/.bp


(* ::Subsection:: *)
(*Load a CFF transition*)


(* ::Text:: *)
(*The transitions corresponding to the benchmark parameters above can be loaded with the "Transition" argument*)


(* ::Input::Initialization::Plain:: *)
tr=CFFModel[idx,"Transition"]


(* ::Subsection:: *)
(*Save a transition*)


(* ::Text:: *)
(*As  an  example, here  we  save  a  Transition  object  to  a . m  file  in  the  notebook  directory*)


(* ::Input::Initialization::Plain:: *)
SetDirectory[NotebookDirectory[]];
tr>>transition_CFF.m


(* ::Text:: *)
(*The same transition can then be loaded with*)


(* ::Input::Initialization::Plain:: *)
tr=<<transition_CFF.m
