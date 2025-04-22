(* ::Package:: *)

(* ::Title:: *)
(*Dark Abelian Higgs model analysis:*)
(*Daisy resummation + PT2GWFinder*)


(* ::Text:: *)
(*The model consists of a decoupled dark sector: a complex scalar field charged under a U(1)' gauge symmetry.*)
(*In this notebook, we reproduce the analysis presented in the companion paper.*)
(*The thermal effective potential is constructed using daisy resummation.*)
(**)
(*This notebook is designed to reproduce with PT2GWFinder the analysis presented in:*)
(*	Dark, cold, and noisy: constraining secluded hidden sectors with gravitational waves*)
(*	by Moritz Breitbach, Joachim Kopp, Eric Madge, Toby Opferkuch and Pedro Schwaller*)
(*	https://iopscience.iop.org/article/10.1088/1475-7516/2019/07/007*)
(**)
(*Please cite XXXX if the analysis you perform with PT2GWFinder results in a publication.*)


(* ::Input:: *)
(*Quit*)


(* pre-release only: load paclet if not installed *)
pacletDir="/home/marco/Aveiro/Nerdy/Mathematica nbs/PTFB/PT2GW/";
PacletDirectoryLoad[pacletDir];


(* ::Section:: *)
(*Construction of the Effective Potential*)


(* ::Subsection:: *)
(*Tree-level*)


(* ::Text:: *)
(*Tree-level potential*)


Vtree[S_]=-(1/2)\[Mu]sq S^2+\[Lambda]/8 S^4


(* ::Text:: *)
(*Tree-level minimization condition*)


minRule=Solve[Vtree'[S]==0/.S->v,v]//Last


(* ::Subsection:: *)
(*1-loop corrections*)


(* ::Subsubsection::Closed:: *)
(*Masses*)


(* ::Text:: *)
(*Fix the mass parameter from the minimization condition*)


\[Mu]sqRule=Solve[v==(v/.minRule),\[Mu]sq]//First


(* ::Text:: *)
(*Field-dependent masses for the radial scalar mode, for the Goldstone mode, and for the gauge boson*)


mSsq=\!\(
\*SubscriptBox[\(\[PartialD]\), \({S, 2}\)]\(Vtree[S]\)\)
m\[Sigma]sq=-\[Mu]sq + 1/2 \[Lambda] S^2
mAsq=g^2 S^2


(* ::Text:: *)
(*Collect as a mass vector*)


msq[i_]:=Evaluate[{mSsq,m\[Sigma]sq,mAsq,mAsq,mAsq}/.\[Mu]sqRule//Simplify][[i]]


msq[All]


(* ::Subsubsection::Closed:: *)
(*Zero-temperature Coleman-Weinberg (CW) potential*)


(* ::Text:: *)
(*We define the "Coleman-Weinberg" logarithm that introduces a regulator epsilon to cure the IR divergence coming from the Goldstone boson mass going to zero as S goes to zero.*)
(*Whenever the log of a vanishing mass square is present, it is replaced by the log of the IR regulator.*)
(*Here, \[CapitalLambda] is the renormalization scale.*)


ClearAll[LogCW];
LogCW[msq_]:=\[Piecewise]{
 {Log[msq/\[CapitalLambda]^2], msq=!=0},
 {Log[\[CurlyEpsilon]/\[CapitalLambda]^2], True}
}


(* ::Text:: *)
(*For numerical purposes, the logarithm of zero is manually redefined to be the logarithm of the IR regulator.*)


Unprotect[Log];
Log[0.]=Log[\[CurlyEpsilon]/\[CapitalLambda]^2];
Log[0]=Log[\[CurlyEpsilon]/\[CapitalLambda]^2];
Protect[Log];


(* ::Text:: *)
(*Constants from the dimensional regularization of the CW potential*)


c[i_]:=Which[MemberQ[{1,2},i],3/2,MemberQ[{3,4,5},i],5/6,True,Print["Bad i=",i," in c[i]"];Abort[]]


(* ::Text:: *)
(*Counter-term potential*)


Vct[S_]=-(\[Delta]\[Mu]sq/2) S^2+\[Delta]\[Lambda]/8 S^4;


(* ::Text:: *)
(*Full CW potential*)


VCW[S_]=1/(64 \[Pi]^2) \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(Length[msq[All]]\)]\(
\*SuperscriptBox[\(msq[i]\), \(2\)] \((LogCW[msq[i]] - c[i])\)\)\)+Vct[S]//Simplify


(* ::Text:: *)
(*First and second derivatives of the CW potential*)


cwExpr=({VCW'[S],VCW''[S]}/.S->v)//Collect[#,{g,\[Lambda]},Simplify]&//
Echo[#,"",TableForm]&;


(* ::Text:: *)
(*Manual removal of the IR divergence*)


cwExpr=cwExpr/.\[CurlyEpsilon]->\[CapitalLambda]^2//
Echo[#,"",TableForm]&;


(* ::Text:: *)
(*Solution for the counterterms*)


cwSol=Solve[Thread[cwExpr==0],{\[Delta]\[Mu]sq,\[Delta]\[Lambda]}]//First//Simplify//Echo[#,"CT sol:",TableForm]&;


(* ::Subsubsection::Closed:: *)
(*Thermal potential*)


(* ::Text:: *)
(*Construct the thermal function 'Jb'*)
(*(see e.g. the review  2305.02357 by Athron et al.)*)


(*
Jb[\[Theta]_?NumericQ]:=Module[{f,f1},
	f[x_]:=x^2 Log[1-Exp[-Sqrt[x^2+\[Theta]]]];
	If[\[Theta]>=0,
	NIntegrate[f[x],{x,0,\[Infinity]}],
	f1[x_]:=x^2 Log[2Abs[Sin[Sqrt[-x^2-\[Theta]]/2]]];
	NIntegrate[f1[x],{x,0,Sqrt[Abs[\[Theta]]]}]+NIntegrate[f[x],{x,Sqrt[Abs[\[Theta]]],\[Infinity]}]
	]]
	
xRange={ArcSinh[-1.3*20],ArcSinh[-(1410./-3.724)20]}; (* range as implemented in CosmoTransitions *)
xlist=Subdivide[Sequence@@xRange,1000];
xlist=Abs[xRange[[1]]]Sinh[xlist]/20;
JbList=Re[Jb/@xlist];//EchoTiming

JbInterp=Interpolation[{xlist,JbList}\[Transpose],Method->"Spline"]

SetDirectory[NotebookDirectory[]]; (* export to a notebook for later use *)
JbInterp>>data/JbInterpolation.m
*)


(* ::Text:: *)
(*Import the precomputed thermal function Jb*)


SetDirectory[NotebookDirectory[]];
JbInterp=<<data/JbInterpolation.m


(* ::Text:: *)
(*Finite temperature part of the one-loop potential*)


VT[S_,T_]=T^4/(2\[Pi]^2) \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(Length[msq[All]]\)]\(JbInterp[
\*FractionBox[\(msq[i]\), 
SuperscriptBox[\(T\), \(2\)]]]\)\)


(* ::Text:: *)
(**)


(* ::Text:: *)
(*Alternatively, use the high-temperature approximation for the thermal correction V_T*)


(*VTHigh[S_,T_]=T^4/12 \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(Length[msq[All]]\)]\((
\*FractionBox[\(msq[i]\), \(2
\*SuperscriptBox[\(T\), \(2\)]\)] - 
\*FractionBox[\(1\), \(\[Pi]\)]Re[
\*SuperscriptBox[\((
\*FractionBox[\(msq[i]\), 
SuperscriptBox[\(T\), \(2\)]])\), 
FractionBox[\(3\), \(2\)]]])\)\)*)


(* ::Subsubsection::Closed:: *)
(*Daisy term*)


(* ::Text:: *)
(*Debye masses for the scalar modes and for the longitudinal gauge boson mode*)


\[CapitalPi]S[T_]:=(\[Lambda]/6+g^2/4)T^2
\[CapitalPi]A[T_]:=g^2/3 T^2
\[CapitalPi][i_]:={\[CapitalPi]S[T],\[CapitalPi]S[T],\[CapitalPi]A[T],0,0}[[i]] (* the two zeros correspond to the transverse gauge boson modes *)


Vdaisy[S_,T_]=-(T/(12 \[Pi]))\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(Length[msq[All]]\)]\(Re[
\*SuperscriptBox[\((msq[i] + \[CapitalPi][i])\), 
FractionBox[\(3\), \(2\)]] - 
\*SuperscriptBox[\(msq[i]\), 
FractionBox[\(3\), \(2\)]]]\)\)//Simplify


(* ::Subsection:: *)
(*Full effective potential*)


(* ::Text:: *)
(*Build the total potential V_tot = V_tree + V_CW + V_T + V_daisy*)


Vtot[S_,T_]=Vtree[S]+(VCW[S]/.cwSol)+VT[S,T]+Vdaisy[S,T]//Simplify;


Vtot[S_,T_]=Vtot[S,T]/.Log[x_]->Log[RealAbs[x]]; (* takes the real part of the effective potential *)


(* ::Text:: *)
(*The full effective potential can be stored in an external notebook, so it can be easily loaded later*)


(*SetDirectory[NotebookDirectory[]];
Compress[Vtot[S,T]]>>data/DarkPhoton_CW_Potential.m*)


(* ::Subsection:: *)
(*Benchmark points*)


(* ::Text:: *)
(*Load benchmark parameters from the Models sub-package*)


(* load the Models sub-package *)
<<PT2GW/Models.m;


(* load the Dataset of benchmark parameters *)
BPs=DPModel[All,"Benchmark"]
bps[i_]:=KeyMap[ToExpression,BPs[i]//Normal]


(* ::Text:: *)
(*The effective potential can be loaded from file.*)
(*NB The potential implemented above can also be loaded from the Models sub-package with DPModel[].*)


(*SetDirectory[NotebookDirectory[]];
Vtot[s_,t_]:=Evaluate[Uncompress[<<"data/DarkPhoton_CW_Potential.m"]/.{S->s,T->t}];*)


(* ::Text:: *)
(*Select the benchmark point and evaluate the potential*)


bp=bps[1]
V[S_,T_]=Vtot[S,T]/.\[Mu]sqRule/.\[CapitalLambda]->v/.bp//Simplify; (* the renormalization scale \[CapitalLambda] is set to the tree-level VEV *)


(* ::Text:: *)
(*Check the result is numerical*)


V[v,v/2]/.bp


(* ::Section:: *)
(*PT2GWFinder*)


(* ::Subsection:: *)
(*Initialize*)


(* ::Text:: *)
(*Load the package*)


<<PT2GW`


(* ::Text:: *)
(*Set the bubble wall velocity*)


vw=0.95;


(* ::Text:: *)
(*Define the energy units*)


DefineUnits["keV"]


(* ::Text:: *)
(*Compute the number of relativistic degrees of freedom*)


gstarSM=3.38; (* SM degrees of freedom at the keV scale *)
gstarHS=5;    (* dark degrees of freedom *)
\[Xi]=.48;        (* temperature ratio between dark sector and SM sector:  \[Xi] = T / T_\[Gamma] *)
RelativisticDOF=gstarSM/\[Xi]^4+gstarHS (* total number of degrees of freedom *)
(* NB RelativisticDOF is a PT2GW built-in symbol *)


(* ::Text:: *)
(*Plot the potential*)


PlotPotential[V,{-1,1}1.3v/.bp,v/4/.bp,"LogTRange"->1,"Log\[Phi]Range"->1.5]


(* ::Subsection::Closed:: *)
(*SearchPotential*)


(* ::Text:: *)
(*Run the SearchPotential module, which is the global wrapper in our package.*)


transitions=SearchPotential[V,vw, (* the minimal input: thermal potential and bubble wall velocity *)
	(* Additional options *)
	"TRange"->{0.01,.3}v/.bp,    (* temperature range for the phase tracing *)
	"BrokenPhaseScale"->10v/.bp, (* scale of the broken phase: must be close to or larger than the actual minimum *)
	"Metadata"->bp               (* optional metadata: here, we store the benchmark values *)
	]//EchoTiming


(* ::Subsubsection::Closed:: *)
(*Inspect the transition*)


(* ::Text:: *)
(*Extract one transition from the list of all transitions found*)


(* ::Input::Initialization::Plain:: *)
tr=transitions[[1]]
AutoComplete[tr]; (* optional: quickly search keys from a drop-down menu *)


(* ::Text:: *)
(*Inspect transition elements*)


(* ::Input::Initialization::Plain:: *)
tr["ActionFunction"]


(* ::Text:: *)
(*The key-search for Transitions behaves like the built-in Lookup*)


(* ::Input::Initialization::Plain:: *)
tr[{"fPeak","h2OmegaPeak","not a key"},"missing",Log10]


(* ::Text:: *)
(*Display all data as Dataset*)


(* ::Input::Initialization::Plain:: *)
tr//Dataset


(* ::Text:: *)
(*Get the underlying Association*)


(* ::Input::Initialization::Plain:: *)
tr//Normal


(* ::Subsubsection::Closed:: *)
(*Plots*)


(* ::Text:: *)
(*Plotting functions included in the package*)


(* ::Text:: *)
(*Plot the phase diagram*)


(* ::Input::Initialization::Plain:: *)
PlotTransition[tr]


(* ::Text:: *)
(*Plot the action vs temperature, with the relevant temperatures*)


(* ::Input::Initialization::Plain:: *)
PlotAction[tr]


(* ::Text:: *)
(*Check the available detector sensitivities*)


GWSensitivities["List"]


(* ::Text:: *)
(*Plot the GW power spectrum*)


(* ::Input::Initialization::Plain:: *)
PlotGW[tr,"Detectors"->{"PPTA PLISC","SKA PLISC","IPTA PLISC","NANOGrav PLISC"}]


(* ::Subsection::Closed:: *)
(*Internal Functions*)


(* ::Text:: *)
(*All PT2GW functions feature usage messages*)


(* ::Input::Initialization::Plain:: *)
?SearchPhases


(* ::Text:: *)
(*A  template for the function arguments can be pasted from the popup menu (Ctrl + Shift + K, Cmd + Shift + K on Mac)*)


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


(* ::Subsubsection:: *)
(*Phase tracing*)


(* ::Text:: *)
(*Trace the phases via the TracePhases module*)


Phases=TracePhases[V,
	"TRange"->{0.01,.3}v/.bp,
	"NTracingPoints"->300,
	"BrokenPhaseScale"->10v/.bp
	]//EchoTiming


(* ::Text:: *)
(*Study the overlap of the phases*)


overlap=Overlap[Phases]


(* ::Text:: *)
(*Find the critical temperature*)


Tc=FindCritical[V,Phases]


(* ::Subsubsection:: *)
(*Euclidean Action*)


(* ::Text:: *)
(*Compute  action (calling FindBounce)*)


(* ::Input::Initialization::Plain:: *)
Action[tr["Tn"],V,tr["Phases"]]


(* ::Text:: *)
(*Compute  Euclidean  action  (calling  FindBounce)  and  fit  the  action - temperature  function S_ 3/T  (T)*)


actionFunction=ActionFit[V,tr["Phases"],tr["Tn"]+{-1.,1.},tr["Tc"],"RefineInflection"->False(*{True,"NHits"->4,"MaxIterations"->30}*)]


(* ::Text:: *)
(*Compare fit functions*)


(* ::Code::Initialization::Plain:: *)
fits=Table[ActionFit[V,tr["Phases"],actionFunction["Domain"],tr["Tc"],"Data"->actionFunction["Data"],options],{options,{
	{},
	{"Orders"->Range[-3,0]},
	{"ActionMethod"->Interpolation}
	}}];
LogPlot[Through[Through[fits["Function"]][T]]//Evaluate,{T,actionFunction["Domain"][[1]],tr["Tc"]},
	PlotStyle->{Dashing[None],Dashed,Dashed},
	PlotLegends->{"default","orders\[Rule](-3,0)","\"ActionMethod\"\[Rule]Interpolation"},
	GridLines->Automatic
	]


(* ::Subsubsection:: *)
(*Nucleation temperature*)


(* ::Text:: *)
(*Estimation of nucleation temperature with default bisection method*)


FindNucleation[V,overlap,Phases,Return->"Tn"]


(* ::Text:: *)
(*Plot  \[CapitalGamma]/H^4 (T)*)


(* ::Input::Initialization::Plain:: *)
DiscretePlot[DecayOverHubble[T,V,Phases],{T,tr["TnEstimate"]+Subdivide[-1,1,10]},
	AxesLabel->{"T","\[CapitalGamma]/\!\(\*SuperscriptBox[\(H\), \(4\)]\)"},PlotLabel->"Nucleation estimate",
	"ScalingFunctions"->"Log",Joined->True,
	Epilog->{Dashed,InfiniteLine[{0,Log@1},{1,0}],Darker@Orange,InfiniteLine[{tr["TnEstimate"],1},{0,1}]}
	]


(* ::Text:: *)
(*Computation of nucleation temperature with integral criterion*)


Tn=FindNucleation[V,{7.5,10.8},Phases,
	"PrintIterations"->True,
	"NucleationCriterion"->"IntegralDecay",
	"NucleationMethod"->{"ActionFunction"->actionFunction["Function"],"Tc"->Tc,"TnEstimate"->tr["TnEstimate"]},
	Return->"Tn"
	]//EchoTiming


(* ::Text:: *)
(*Plot the nucleation criterion integral*)


(* ::Input::Initialization::Plain:: *)
actionFunction=tr["ActionFunction"]


(* ::Input::Initialization::Plain:: *)
DiscretePlot[IntegralDecay[T,tr["Tc"],actionFunction["Function"],V,tr["Phases"]],{T,tr["Tn"]+Subdivide[-1,1,10]},
AxesLabel->{"T","\[Integral]dT/T \[CapitalGamma]/\!\(\*SuperscriptBox[\(H\), \(4\)]\)"},PlotLabel->"Nucleation criterion",
"ScalingFunctions"->"Log",Joined->True,Epilog->{Dashed,InfiniteLine[{0,Log@1},{1,0}],Orange,InfiniteLine[{tr["Tn"],1},{0,1}]}]


(* ::Subsubsection:: *)
(*Percolation temperature*)


(* ::Text:: *)
(*Determine  the  percolation  temperature  by  solving  I_F(T_p) = 0.34*)


(* ::Input::Initialization::Plain:: *)
Tp=FindPercolation[actionFunction["Function"],{tr["Tn"],tr["Tc"]},tr["WallVelocity"],V,tr["Phases"]]


(* ::Input::Initialization::Plain:: *)
DiscretePlot[IntegralFalseVacuum[actionFunction["Function"],T,tr["Tc"],tr["WallVelocity"],V,tr["Phases"]],
	{T,tr["Tp"]+Subdivide[-1,1,10]},
	AxesLabel->{"T","\!\(\*SubscriptBox[\(I\), \(FV\)]\)"},PlotLabel->"False vacuum decay integral",
	"ScalingFunctions"->"Log",Joined->True,
	Epilog->{Dashed,InfiniteLine[{0,Log@.34},{1,0}],Darker@Yellow,InfiniteLine[{tr["Tp"],1},{0,1}]}
	]


(* ::Subsubsection:: *)
(*Phase transition parameters*)


(* ::Text:: *)
(*Compute the transition strength \[Alpha]*)


Alpha[tr["Tp"],V,tr["Phases"]]


(* ::Text:: *)
(*Compute the inverse duration in Hubble units \[Beta]/H(T*)*)


BetaHubble[tr["Tp"],actionFunction["Function"]]


(* ::Text:: *)
(*The user can decide at which temperature they are computed in SearchPotential (SearchPhases) with the "TransitionTemperature" option.*)


(* ::Subsubsection:: *)
(*SearchPhases*)


(* ::Text:: *)
(*Run the SearchPhases module to study the transition between the previously calculated phases*)


transition=SearchPhases[V,Phases,vw,"RefineInflection"->False]//EchoTiming


AutoComplete[transition];


(* ::Subsection::Closed:: *)
(*Gravitational waves*)


(* ::Subsubsection:: *)
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
(*ComputeGW  returns  an  Association  of  GW  parameters, and  stores  it  in  the  GWData symbol, from the GW` sub-package*)


(* ::Input::Initialization::Plain:: *)
ComputeGW[transition//Normal]//Dataset


(* ::Text:: *)
(*The  spectra, corresponding to the key "h2Omega", can be plotted with PlotGW*)


(* ::Input::Initialization::Plain:: *)
PlotGW[GWData["h2Omega"],"Detectors"->{"PPTA PLISC","SKA PLISC","IPTA PLISC","NANOGrav PLISC"}]


(* ::Subsubsection:: *)
(*Detector sensitivities*)


(* ::Text:: *)
(*Several peak-integrated (PISCs) and power-law-integrated sensitivity curves (PLISCs) are built-in PT2GW:*)


PlotGWSensitivities[10^{-9,4},All]


(* ::Text:: *)
(*To add a user-defined detector sensitivity, simply  define the corresponding PISC (peak-integrated sensitivity curve), as a function of frequency in Hertz*)


(* ::Input::Initialization::Plain:: *)
GWSensitivities[f_,"myDetector",OptionsPattern[]]:=10^-23 (10^3 f^3+10^-3f^-3)


PlotGW[tr,"Detectors"->{"NANOGrav PLISC","myDetector"}]


(* ::Subsection:: *)
(*Parameter scan*)


(* ::Text:: *)
(*We may run simple parameter scans, optionally exploiting parallelization (see sub-section below).*)


(* ::Text:: *)
(*To  construct  a  Dataset  of  transitions, we  first  define  a  function  to  run  single  a  benchmark  with  option  Dataset -> True,*)
(*and  append  it  to  a  dataset  DS .*)


(* ::Text:: *)
(*The user should take care of the options to run SearchPotential.*)
(*An example is the temperature range: for different benchmark points, the potential evolution and therefore the transition properties can be vastly different.*)
(*It is recommended to split the parameter space in different regions, within which the benchmark points are qualitatively similar.*)
(*Here, we do not follow this approach, for the sake of simplicity.*)


(* ::Subsubsection:: *)
(*Sequential scan*)


(* separator between consecutive runs *)
sep=StringRepeat["\[Placeholder]\[SelectionPlaceholder]",30];


(* run a benchmark and add the found transitions to DataFrame *)
Options[run]={Print->True,EchoTiming->True};
run[benchmarkValues_,opt:OptionsPattern[]]:=Module[{},
	If[!MatchQ[DS,_Dataset],DS=Dataset[{}]];
	If[OptionValue[Print],Print@sep];
	Echo[benchmarkValues,"Running AH at \[Rule]"];
	V[S_,T_]=Vtot[S,T]/.\[Mu]sqRule/.\[CapitalLambda]->v/.benchmarkValues//Simplify;
	dsRun=SearchPotential[V,vw,
		"TRange"->{0.01,1.}v/.benchmarkValues,
		"NTracingPoints"->600,
		"BrokenPhaseScale"->10v/.benchmarkValues,
		Dataset->True,
		"Metadata"->KeyMap[ToString,Association[benchmarkValues]],
		"Plots"->None,
		ProgressIndicator->False
		]//If[OptionValue[EchoTiming],EchoTiming,Identity];
	If[dsRun=!=Dataset[{}],DS=DS~Join~dsRun]
	]


(* ::Text:: *)
(*Turn off printing messages*)


$PT2GWPrint=False;


(* ::Text:: *)
(*Here we scan the benchmark points defined above (BPs)*)


Scan[Quiet[run[bps[#],Print->False,EchoTiming->False]]&,
	Range[Length[BPs]]
	]//EchoTiming


(* ::Text:: *)
(*View Dataset of found transitions *)


DS


(* plot the scanning and transition parameters *)
(* NB PointValuePlot was introduced in MMA 12.2 *)
With[{scanParams={"\[Lambda]","g"},tranParams={"\[Alpha]","\[Beta]/H"}}, 
	PointValuePlot[(scanParams->Log10[tranParams])/.Normal[DS],
		{1->"Color",2->"Size"},            (* \[Alpha] as color and \[Beta]/H as circle size *)
		ScalingFunctions->{"Log","Log"},
		PlotLegends->Automatic,
		FrameLabel->scanParams
		]/.BarLegend[x___]:>BarLegend[x,LegendLabel->StringTemplate["log(``)"]@tranParams[[1]]]
	]


(* ::Subsubsection:: *)
(*Parallel scan*)


(* ::Text:: *)
(*Use the built-in Parallelize function to launch multiple benchmarks on different kernels.*)
(*First, let's set up the parallel kernels*)


ParallelNeeds["PT2GW"]


(* load PT2GW and the Examples on all kernels *)
ParallelEvaluate[
	$PT2GWPrint=False;
	];


(* share between kernels the Dataset container for the transitions *)
SetSharedVariable[DS]


Parallelize@Scan[Quiet[run[#,Print->False,EchoTiming->False]]&,
	scanParameters
	]//EchoTiming;


(* ::Text:: *)
(*Define a grid of quartic parameters (\[Lambda],g), fixing the dark photon VEV to 40 keV*)


scanParameters=Table[{v->40.,\[Lambda]->\[Lambda]0,g->g0},
	{g0,.2,1.,.2},
	{\[Lambda]0,10^Subdivide[-4.,-1.,5]}
	];
ListPlot[Flatten[scanParameters,1][[All,2;;]]//Values,ScalingFunctions->{"Log"},AxesLabel->{"\[Lambda]","g"}]


(* ::Text:: *)
(*Run the scan*)


DS=.;
(*Scan[run,
	 Flatten[scanParameters,1]
	]//EchoTiming*)


(* ::Text:: *)
(*Visualize the scan results*)


DS


(* reproduce plot from the paper (PT2GW data only) *)
plotDS=PointValuePlot[DS[All,{"\[Lambda]","g"}]->DS[All,#,Log10]//Normal,"Color",
		PlotRange->{{10^-4,2*10^-1},{.15,1.05}},
		PlotStyle->AbsolutePointSize[10],
		GridLines->None,
		PlotLegends->Automatic,
		ColorFunction->ColorData[#/.{"\[Alpha]"->{"RedBlueTones","Reverse"},"\[Beta]/H"->"RedBlueTones"}],
		ScalingFunctions->{"Log"},
		FrameLabel->{"\[Lambda]","g"},
		ImageSize->Medium]&;
Labeled[plotDS[#],"\!\(\*SubscriptBox[\(Log\), \(10\)]\)"<>#,Right]&/@{"\[Alpha]","\[Beta]/H"}//Row[#,Spacer[20]]&
