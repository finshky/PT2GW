(* ::Package:: *)

(* ::Chapter:: *)
(*Dark Abelian Higgs model analysis:*)
(*Dimensional reduction + PT2GWFinder*)


(* ::Text:: *)
(*The model consists of a decoupled dark sector: a complex scalar field charged under a U(1)' gauge symmetry.*)
(*In this notebook, we reproduce the analysis presented in the companion paper.*)
(*The thermal effective potential is obtained using dimensional reduction, implemented with DRalgo (https://doi.org/10.1016/j.cpc.2023.108725).*)
(*The closed-form effective potential is then constructed with DRTools, a sub-package provided with PT2GWFinder.*)
(**)
(*This notebook is intended to compare dimensional reduction with PT2GWFinder to the analysis presented in:*)
(*	Dark, cold, and noisy: constraining secluded hidden sectors with gravitational waves*)
(*	by Moritz Breitbach, Joachim Kopp, Eric Madge, Toby Opferkuch and Pedro Schwaller*)
(*	https://iopscience.iop.org/article/10.1088/1475-7516/2019/07/007,*)
(*which is based on daisy resummation.*)
(**)
(*Please cite XXXX if the analysis you perform with PT2GWFinder results in a publication.*)


(* ::Code:: *)
(*Quit*)


(* pre-release only: load paclet if not installed *)
pacletDir="/home/marco/Aveiro/Nerdy/Mathematica nbs/PTFB/PT2GW/";
PacletDirectoryLoad[pacletDir];


(* ::Section::Closed:: *)
(*DRalgo: construction of the effective potential*)


(* ::Subsection::Closed:: *)
(*Initialize*)


(* ::Text:: *)
(*Load DRalgo*)


$LoadGroupMath=True;
<<DRalgo`


(* ::Subsection::Closed:: *)
(*Group structure*)


(* ::Text:: *)
(*Define the U(1)' group structure*)


Group={"U1"};
CouplingName={g};
RepAdjoint={0};
Higgs={{Y},"C"}; (* Y = 1 *)
RepScalar={Higgs};


RepFermion={}; (* no fermions included *)


(* ::Text:: *)
(*Define tensor parameters*)


{gvvv,gvff,gvss,\[Lambda]1,\[Lambda]3,\[Lambda]4,\[Mu]ij,\[Mu]IJC,\[Mu]IJ,Ysff,YsffC}=AllocateTensors[Group,RepAdjoint,CouplingName,RepFermion,RepScalar];


(* ::Subsection::Closed:: *)
(*Tree-level, 4D potential*)


(* ::Text:: *)
(*Construction of Lorentz invariants:*)
(*mass term*)


InputInv={{1,1},{True,False}}; (*This specifies that we want a \[Phi]^+\[Phi] term*)
MassTerm1=CreateInvariant[Group,RepScalar,InputInv]//Simplify//FullSimplify;


VMass=\[Mu]sq*MassTerm1[[1]];(*This is the \[Phi]^+\[Phi] term written in component form*)


\[Mu]ij=GradMass[VMass]//Simplify//SparseArray;


(* ::Text:: *)
(*Quartic term*)


QuarticTerm1=MassTerm1[[1]]^2; (*Because MassTerm1=\[Phi]^+\[Phi], we can write (\[Phi]^+\[Phi])^2=MassTerm1^2*)


VQuartic=\[Lambda]*QuarticTerm1;


\[Lambda]4=GradQuartic[VQuartic];


(* ::Subsection::Closed:: *)
(*Dimensional reduction*)


(* ::Text:: *)
(*Import the 4D model*)


ImportModelDRalgo[Group,gvvv,gvff,gvss,\[Lambda]1,\[Lambda]3,\[Lambda]4,\[Mu]ij,\[Mu]IJ,\[Mu]IJC,Ysff,YsffC,Verbose->False];


(* ::Subsubsection::Closed:: *)
(*DRHard*)


(* ::Text:: *)
(*Match to the 3D theory, to obtain a theory at the "soft" energy scale*)


PerformDRhard[]


(* ::Text:: *)
(*Print matching relations: 3D quantities in terms of 4D quantities*)


PrintCouplings[]


PrintConstants[]


PrintScalarMass["LO"]
PrintScalarMass["NLO"]
PrintDebyeMass["LO"]
PrintDebyeMass["NLO"]


PrintTensorDRalgo[];


PrintTemporalScalarCouplings[]//Simplify


PrintPressure["LO"]
PrintPressure["NLO"]
PrintPressure["NNLO"]


BetaFunctions4D[]


(* ::Text:: *)
(*Print anomalous dimensions*)


AnomDim4D["S",Table[PrintScalarRepPositions[][[1]],2]]
AnomDim4D["V",Table[PrintGaugeRepPositions[][[1]],2]]


(* ::Subsubsection::Closed:: *)
(*DRSoft*)


(* ::Text:: *)
(*Optionally, we may integrate out temporal scalars, to obtain a theory at the "ultrasoft" energy scale*)


PerformDRsoft[{}];


PrintCouplingsUS[]


PrintScalarMassUS["LO"]
PrintScalarMassUS["NLO"]


(* ::Subsubsection::Closed:: *)
(*Effective Potential*)


(* ::Text:: *)
(*Choose the energy scale ("soft" or "ultrasoft")*)


UseSoftTheory[]
(*UseUltraSoftTheory[]*)


(* ::Text:: *)
(*Define the VEV structure*)


DefineNewTensorsUS[\[Mu]ij,\[Lambda]4,\[Lambda]3,gvss,gvvv];
\[Phi]VeV={0,\[Phi]}//SparseArray; (* select a non-zero vacuum expectation value in the down-component *)
DefineVEVS[\[Phi]VeV];


(* ::Text:: *)
(*Print the tensor parameters in the effective theory*)


PrintTensorsVEV[]
PrintTensorsVEV/@Range[7]


(* ::Text:: *)
(*Compute the effective potential*)


CalculatePotential[];


(* ::Text:: *)
(*Print the effective potential at the leading and higher orders*)


PrintEffectivePotential["LO"]
PrintEffectivePotential["NLO"]
PrintEffectivePotential["NNLO"];


(* ::Section:: *)
(*DRTools: closed-form effective potential*)


(* ::Subsection:: *)
(*Benchmark points*)


(* ::Text:: *)
(*Load benchmark parameters from the Models sub-package*)


(* load the Models sub-package *)
<<PT2GW/Models.m;


(* load the Dataset of benchmark parameters *)
BPs=DPModel[All,"Benchmark"];


(* add square mass and square gauge parameters *)
BPs=BPs[All,<|#,"\[Mu]sq"->-#\[Lambda] #v^2, "gsq"->#g^2|>&][All,Sort]
bps[i_]:=KeyMap[ToExpression,BPs[i]//Normal]


(* ::Text:: *)
(*Select a benchmark and define the renormalization scale \[CapitalLambda]0*)


bp=BPs[1]//Normal
\[CapitalLambda]0="v"/.bp//Echo[#,"\!\(\*SubscriptBox[\(\[CapitalLambda]\), \(0\)]\) \[Rule]"]&;


(* ::Subsection:: *)
(*Effective potential*)


(* ::Text:: *)
(*Load PT2GW and the DRTools module*)


<<PT2GW`
<<PT2GW/DRTools.m


(* ::Text:: *)
(*To save to (load from) file, set the current directory*)


SetDirectory[NotebookDirectory[]];


(* ::Text:: *)
(*Define substitution rules*)
(*This is required to redefine the RG equations: \[Beta](g^2) -> \[Beta](gsq)*)


BetaFunctions4D[] (* inspect DRalgo's \[Beta] functions *)


subRules={g->Sqrt[gsq]};
SubRulesAppend[subRules] (* extend substitution rules to 3D and 3D ultrasoft couplings *)


(* ::Text:: *)
(*Solve RG equations & compute effective potential*)


DPFile="data/DarkPhoton_DR_Expressions.m";
V=ComputeDRPotential[{"gsq","\[Lambda]","\[Mu]sq"}/.bp,{\[CapitalLambda]0,\[CapitalLambda]0/10,100 \[CapitalLambda]0},
	(*"SubRules"->subRules,*)   (* substitution rules are required to solve the RG equations *)
	(*"File"->DPFile,*)         (* save analytical DR expressions to file *)
	"LoadDRFrom"->DPFile,       (* the user may load the DR expressions from a file, once defined *)
	"OrderDR"->"LO",            (* order of the dimensional reduction matching relations: "LO" or "NLO" *)
	"OrderVeff"->"NLO",         (* order of the effective potential: "LO", "NLO" or "NNLO" *)
	"US"->False,                (* ultrasoft scale: this should match the scale of DRalgo's effective potential *)
	"NumericRules"->{Y->1.},     (* any auxiliary parameter should have numerical values *)
	"PlotRG"->True              (* plot the RG solutions *)
	]


V["v","v"/2]/.bp


(* ::Subsubsection::Closed:: *)
(*Plot potential*)


PlotPotential[Re@*V,\[CapitalLambda]0{-1,1},3/8 \[CapitalLambda]0,"Log\[Phi]Range"->1,"LogTRange"->1]


(* ::Subsection::Closed:: *)
(*Breakdown of ComputeDRPotential:*)
(*DRTools internal functions*)


(* ::Subsubsection::Closed:: *)
(*Store/Load DRExpressions*)


(* ::Text:: *)
(*Look at \[Beta]-functions and define substitution rules*)


BetaFunctions4D[]


subRules={g->Sqrt[gsq]};
SubRulesAppend[subRules] (* extend substitution rules to 3D and 3D ultrasoft couplings *)


(* ::Text:: *)
(*After running DRalgo, store DR expressions in the DRExpressions symbol.*)
(*Optionally, save to file with the "File" option.*)


SetDirectory[NotebookDirectory[]]; 
StoreDRExpressions["SubRules"->subRules,"PrintDR"->False,"US"->False(*,"File"->"DarkPhoton_DR_Expressions.m"*)] 


(* ::Text:: *)
(*In the following sessions, load DR expressions from file, and store them as DRExpressions*)


(*SetDirectory[NotebookDirectory[]];
LoadDRExpressions["DarkPhoton_DR_Expressions.m"]; (* load expressions to the DRExpressions symbol *)*)


(* ::Text:: *)
(*Inspect expressions for the dimensional reduction*)


DRExpressions//Dataset


(* ::Subsubsection::Closed:: *)
(*RG Solver*)


(* ::Text:: *)
(*\[Beta] functions are readily saved with the appropriate substitution as "BetaFunctions"*)


DRExpressions["BetaFunctions"]


(* ::Text:: *)
(*Solve numerically the RG equations*)


RGSolutions=RGSolve[
	DRExpressions["BetaFunctions"], (* RG equations or equivalently rules for the \[Beta] functions *)
	{"gsq","\[Lambda]","\[Mu]sq"}/.bp,          (* boundary conditions: parameters at energy \[CapitalLambda]0 *)
	{\[CapitalLambda]0,\[CapitalLambda]0/10,10 \[CapitalLambda]0},               (* renormalization scale, minimum and maximum *)     
	"NumericRules"->{Y->1}           
	]//Echo[#,"RG solutions",TableForm]&;


(* ::Text:: *)
(*Plot the RG solutions *)


PlotRG[All,{\[CapitalLambda]0/2,4\[CapitalLambda]0},Epilog->{Dashed,Gray,InfiniteLine[{\[CapitalLambda]0,0},{0,1}]}]


(* ::Subsubsection::Closed:: *)
(*Dimensional Reduction*)


(* ::Text:: *)
(*Perform the dimensional reduction step*)


With[{\[CapitalLambda]=\[CapitalLambda]0,s=1},
	DRStep[\[CapitalLambda]/(\[Pi] s),s,RGSolutions/.((l_->r_):>(l->r[\[CapitalLambda]])),
		"OrderDR"->"NLO",
		"US"->False,        (* soft (False) or ultrasoft (True) theory. This should match the settings of DRalgo *)
		"NumericRules"->{Y->1.}
		]
	]


(* ::Text:: *)
(*Plot parameters in the dimensionally reduced theory*)


PlotDR[All,{\[CapitalLambda]0/3,2\[CapitalLambda]0},1.,Epilog->{Dashed,Gray,InfiniteLine[{\[CapitalLambda]0,0},{0,1}]},"US"->False,"NumericRules"->{Y->1}]


(* ::Text:: *)
(*Time the dimensional reduction step*)


Module[{\[CapitalLambda],n=10^3,\[CapitalLambda]Prefactor=1,parameters4D},
	Echo[n,"# DRStep evaluations"];
	Table[
		\[CapitalLambda]=\[CapitalLambda]Prefactor \[Pi] T;
		parameters4D=RGSolutions/.((l_->r_):>(l->r[\[CapitalLambda]]));
		DRStep[T,\[CapitalLambda]Prefactor,parameters4D,"US"->False,"NumericRules"->{Y->1}],
		{T,Subdivide[\[CapitalLambda]0/2,2 \[CapitalLambda]0,n]}
		];//EchoTiming
	]


(* ::Subsubsection::Closed:: *)
(*Effective potential*)


(* ::Text:: *)
(*Define effective potential in the dimensionally reduced theory*)


optionsDR={"OrderVeff"->"NLO","NumericRules"->{Y->1},"US"->False};
Vtest[\[Phi]_,T_]:=Check[DRPotentialN[\[Phi],T,RGSolutions,1,optionsDR],
	\!\(
\*SubscriptBox[\(\[Limit]\), \(\[CurlyPhi] -> \[Phi]\)]\(DRPotentialN[\[CurlyPhi], T, RGSolutions, 1, optionsDR]\)\) (* take the limit to avoid numerical divergences  *)
	]//Quiet


Vtest[\[Phi],\[CapitalLambda]0]


(* ::Text:: *)
(*Evaluation time of the effective potential*)


(* efficiency *)
With[{n=32},
	Echo[n^2,"# Veff evaluations"];
	Table[Vtest[\[Phi],T],{\[Phi],Subdivide[0,10 "v"/.bp,n]},{T,Subdivide[\[CapitalLambda]0/10,2 \[CapitalLambda]0,n]}]//EchoTiming;
	]


(* ::Text:: *)
(*Compare effective potential orders (LO, NLO, NNLO)*)


With[{orders={"LO","NLO"(*,"NNLO"*)}},
Manipulate[
	Plot[Table[Re@DRPotentialN[\[Phi],T,RGSolutions,1,"OrderVeff"->o,optionsDR], (* overwrite matching order *)
		{o,orders}
			]//Evaluate,
		{\[Phi],-\[Phi]scale "v",\[Phi]scale "v"}/.bp,
		PlotLegends->orders,PlotRange->(10^logVscale "v"^4/.bp)],
	{{T,3\[CapitalLambda]0/8},\[CapitalLambda]0/4,\[CapitalLambda]0/2,Appearance->"Labeled"},
	{{\[Phi]scale,2},1,3},
	{{logVscale,-4},-5,0},
	TrackedSymbols:>{T,\[Phi]scale,logVscale}
	]]


(* ::Subsubsection::Closed:: *)
(*Effective masses*)


(* ::Text:: *)
(*Inspect the scalar and vector mass matrices of the effective 3D theory*)


DRExpressions["SquareMassMatrixScalar"]//MatrixForm


DRExpressions["SquareMassMatrixVector"]//MatrixForm


(* ::Text:: *)
(*Compute eigenvalues at a given temperature*)


optionsDR={"US"->False,"NumericRules"->{Y->1.}}; (* options for the dimensional reduction *)
With[{T=\[CapitalLambda]0/\[Pi],\[Mu]4DPrefactor=1.},
	DRScalarSquareMasses[T,\[Mu]4DPrefactor,RGSolutions,optionsDR]
	]


With[{T=\[CapitalLambda]0/\[Pi],\[Mu]4DPrefactor=1.},
	DRVectorSquareMasses[T,\[Mu]4DPrefactor,RGSolutions,optionsDR]
	]


(* ::Text:: *)
(*Compute the scalar mass-over-energy scale ratio m_eff/\[CapitalLambda] at given temperature and field value.*)
(*m_eff is the highest mass in the effective theory.*)
(*This ratio should be roughly smaller than 1 to ensure the high-temperature perturbativity of the theory. *)


With[{T=\[CapitalLambda]0/\[Pi],\[Mu]4DPrefactor=1.,\[Phi]="v"/3/.bp},
	HighTRatio[T,\[Mu]4DPrefactor,RGSolutions,\[Phi],optionsDR]
	]


(* ::Section:: *)
(*Phase Transitions*)


(* ::Subsection::Closed:: *)
(*Initialize*)


(* ::Text:: *)
(*Set the bubble wall velocity*)


(* ::Input::Initialization::Plain:: *)
vw=.9; (* units of c *)


(* ::Text:: *)
(*Define the energy units (here set to keV)*)


(* ::Input::Initialization::Plain:: *)
DefineUnits["keV"]


(* ::Text:: *)
(*Compute the number of relativistic degrees of freedom*)


gstarSM=3.38; (* SM degrees of freedom at the keV scale *)
gstarHS=5;    (* dark degrees of freedom *)
\[Xi]=.48;        (* temperature ratio between dark sector and SM sector:  \[Xi] = T / T_\[Gamma] *)
RelativisticDOF=gstarSM/\[Xi]^4+gstarHS (* total number of degrees of freedom *)
(* NB RelativisticDOF is a PT2GW built-in symbol *)


(* ::Subsection::Closed:: *)
(*SearchPotential*)


(* ::Text:: *)
(*Run the SearchPotential module, which is the global wrapper in our package.*)


transitions=SearchPotential[V,vw, (* the minimal input: thermal potential and bubble wall velocity *)
	(* options *)
	"TRange"->{1/10,1}\[CapitalLambda]0,          (* temperature range for the phase tracing *)
	"BrokenPhaseScale"->10"v"/.bp, (* scale of the broken phase: must be close to or larger than the actual minimum *)
	"Metadata"->bp                 (* optional metadata: here, we store the benchmark values *)
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
(*Plot the phase diagram*)


(* ::Input::Initialization::Plain:: *)
PlotTransition[tr]


(* ::Text:: *)
(*Plot the action vs temperature, including transition temperatures*)


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


(* ::Subsubsection::Closed:: *)
(*Phase tracing*)


(* ::Text:: *)
(*Trace the phases via the TracePhases module*)


Phases=TracePhases[V,
	"TRange"->{1/4,1}"v"/.bp,
	"NTracingPoints"->300,
	"BrokenPhaseScale"->10"v"/.bp
	]//EchoTiming


(* ::Text:: *)
(*Study the overlap of the phases*)


overlap=Overlap[Phases]


(* ::Text:: *)
(*Find the critical temperature*)


Tc=FindCritical[V,Phases]


(* ::Subsubsection::Closed:: *)
(*Euclidean Action*)


(* ::Text:: *)
(*Compute  action (calling FindBounce)*)


(* ::Input::Initialization::Plain:: *)
Action[tr["Tn"],V,tr["Phases"]]


(* ::Text:: *)
(*Compute  Euclidean  action  (calling  FindBounce)  and  fit  the  action - temperature  function S_ 3/T  (T)*)


actionFunction=ActionFit[V,Phases,tr["Tn"]+{-.5,.3},tr["Tc"],"RefineInflection"->False(*{True,"NHits"->4,"MaxIterations"->30}*)]


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


(* ::Subsubsection::Closed:: *)
(*Nucleation temperature*)


(* ::Text:: *)
(*Estimation of nucleation temperature with default bisection method*)


FindNucleation[V,tr["Domain"],Phases,Return->"Tn"]


(* ::Text:: *)
(*Plot  \[CapitalGamma]/H^4 (T)*)


(* ::Input::Initialization::Plain:: *)
DiscretePlot[DecayOverHubble[T,V,tr["Phases"]],{T,Subdivide[tr["TnEstimate"]-.1,tr["TnEstimate"]+.1,10]},
	AxesLabel->{"T","\[CapitalGamma]/\!\(\*SuperscriptBox[\(H\), \(4\)]\)"},PlotLabel->"Nucleation estimate",
	"ScalingFunctions"->"Log",Joined->True,
	Epilog->{Dashed,InfiniteLine[{0,Log@1},{1,0}],Darker@Orange,InfiniteLine[{tr["TnEstimate"],1},{0,1}]}
	]


(* ::Text:: *)
(*Computation of nucleation temperature with integral criterion (requires action function)*)


Tn=FindNucleation[V,actionFunction["Domain"],Phases,
	"PrintIterations"->True,
	"NucleationCriterion"->"IntegralDecay",
	"NucleationMethod"->{"ActionFunction"->actionFunction["Function"],"Tc"->tr["Tc"],"TnEstimate"->tr["TnEstimate"]},
	Return->"Tn"
	]//EchoTiming


(* ::Text:: *)
(*Plot the nucleation criterion integral*)


(* ::Input::Initialization::Plain:: *)
actionFunction=tr["ActionFunction"]


(* ::Input::Initialization::Plain:: *)
DiscretePlot[IntegralDecay[T,tr["Tc"],actionFunction["Function"],V,tr["Phases"]],{T,Subdivide[tr["Tn"]-.1,tr["Tn"]+.1,10]},
AxesLabel->{"T","\[Integral]dT/T \[CapitalGamma]/\!\(\*SuperscriptBox[\(H\), \(4\)]\)"},PlotLabel->"Nucleation criterion",
"ScalingFunctions"->"Log",Joined->True,Epilog->{Dashed,InfiniteLine[{0,Log@1},{1,0}],Orange,InfiniteLine[{tr["Tn"],1},{0,1}]}]


(* ::Subsubsection::Closed:: *)
(*Percolation temperature*)


(* ::Text:: *)
(*Determine  the  percolation  temperature  by  solving  I_F(T_p) = 0.34, , where P_F=e^-I_F is the false-vacuum fractional volume (requires the action function).*)


(* ::Input::Initialization::Plain:: *)
Tp=FindPercolation[actionFunction["Function"],{tr["Tn"],tr["Tc"]},tr["WallVelocity"],V,tr["Phases"]]


(* ::Text:: *)
(*Plot I_F(T)*)


(* ::Input::Initialization::Plain:: *)
DiscretePlot[IntegralFalseVacuum[actionFunction["Function"],T,tr["Tc"],vw,V,tr["Phases"]],
	{T,Subdivide[tr["Tp"]-.05,tr["Tp"]+.05,6]},
	AxesLabel->{"T","\!\(\*SubscriptBox[\(I\), \(FV\)]\)"},PlotLabel->"False vacuum decay integral",
	"ScalingFunctions"->"Log",Joined->True,
	Epilog->{Dashed,InfiniteLine[{0,Log@.34},{1,0}],Darker@Yellow,InfiniteLine[{tr["Tp"],1},{0,1}]}
	]


(* ::Subsubsection::Closed:: *)
(*Phase transition parameters*)


(* ::Text:: *)
(*Compute the transition strength \[Alpha]*)


Alpha[tr["Tp"],V,tr["Phases"]]


(* ::Text:: *)
(*Compute the inverse duration in Hubble units \[Beta]/H(T*)*)


BetaHubble[tr["Tp"],actionFunction["Function"]]


(* ::Text:: *)
(*The user can decide at which temperature they are computed in SearchPotential (SearchPhases) with the "TransitionTemperature" option.*)


(* ::Subsubsection::Closed:: *)
(*SearchPhases*)


(* ::Text:: *)
(*Run the SearchPhases module to study the transition between the previously calculated phases*)


transition=SearchPhases[V,tr["Phases"],vw,"RefineInflection"->False]//EchoTiming


AutoComplete[transition];


(* ::Subsubsection::Closed:: *)
(*Mass-over-energy scale ratio*)


(* ::Text:: *)
(*For potentials computed with DRTools, the function HighTRatio  computes the mass-over-energy scale ratio m_eff/\[CapitalLambda] at the transition temperature, for the given phases*)


HighTRatio[tr["Tn"],1.,RGSolutions,tr["Phases"],optionsDR]


(* ::Text:: *)
(*Plot the mass-over-energy scale ratio*)


PlotHighTRatio[tr,1.,RGSolutions,
	"US"->False,"NumericRules"->{Y->1.},
	Epilog->{Dashed,Darker@Yellow,
		InfiniteLine[{tr["Tp"],1},{0,1}],
		Orange,InfiniteLine[{tr["Tn"],1},{0,1}],
		Red,InfiniteLine[{tr["Tc"],1},{0,1}]}
		]


(* ::Subsection::Closed:: *)
(*Gravitational waves*)


(* ::Text:: *)
(*To compute the GW spectra from a set of phase transition parameters, we first load a Transition object*)
(*(this already contains GW spectral parameters, but we will recompute them)*)


(* ::Input::Initialization::Plain:: *)
tr=CFFModel[idx,"Transition"]
AutoComplete[tr];


(* ::Subsubsection::Closed:: *)
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
(*Missing arguments give an error*)


ComputeGW[KeyDrop[transition//Normal,"\[Alpha]"]]//Dataset


(* ::Text:: *)
(*ComputeGW  returns  an  Association  of  GW  parameters, and  stores  it  in  the  GWData symbol, from the GW` sub-package*)


(* ::Input::Initialization::Plain:: *)
ComputeGW[transition//Normal]//Dataset


(* ::Text:: *)
(*The  spectra, corresponding to the key "h2Omega", can be plotted with PlotGW*)


(* ::Input::Initialization::Plain:: *)
PlotGW[GWData["h2Omega"],"Detectors"->{"PPTA PLISC","SKA PLISC","IPTA PLISC","NANOGrav PLISC"}]


(* ::Subsubsection::Closed:: *)
(*Collision efficiency \[Kappa]col*)


(* ::Text:: *)
(*To add the contribution from bubble wall collisions, we must include, to the least, the following information:*)
(*- a bosonic mass function*)
(*- a gauge coupling value*)
(*- characteristic temperatures for bubble growth, T0 and T* (see 1903.09642)*)
(**)
(*For an effective potential derived with DRalgo, we may extract mass functions in the following way*)


(* ::Input::Initialization::Plain:: *)
optionsDR={"US"->False,"NumericRules"->{Y->1.}};
scalarMasses[\[CurlyPhi]_,T_]:=Sqrt@DRScalarSquareMasses[T,1.,RGSolutions,optionsDR]/.DRExpressions["VEV"][[1]]->\[CurlyPhi]
vectorMasses[\[CurlyPhi]_,T_]:=Sqrt@DRVectorSquareMasses[T,1.,RGSolutions,optionsDR]/.DRExpressions["VEV"][[1]]->\[CurlyPhi]


scalarMasses[f,15.]


(* ::Input::Initialization::Plain:: *)
(*plot the scalar and vector mass functions *)
Row@{
Plot[scalarMasses[#[T],T]&/@tr["Phases"]//Evaluate,
{T,Sequence@@tr["Domain"]},
AxesLabel->{"T","\!\(\*SubsuperscriptBox[\(m\), \(3  D\), \(scalar\)]\)"},ImageSize->Medium],
Plot[vectorMasses[#[T],T]&/@tr["Phases"]//Evaluate,
{T,Sequence@@tr["Domain"]},
AxesLabel->{"T","\!\(\*SubsuperscriptBox[\(m\), \(3  D\), \(vector\)]\)"},ImageSize->Medium]
	}


(* ::Text:: *)
(*The minimal collision data is then*)


(* ::Input::Initialization::Plain:: *)
colData={"GaugeCouplings"->0.1,"MassFunctions"->{"Bosons"->{
	scalarMasses[#1,#2][[1]]&,
	scalarMasses[#1,#2][[2]]&,
	vectorMasses[#1,#2][[1]]&
}},"T0"->tr["Tn"],"T"->tr["Tp"]};


(* ::Text:: *)
(*Finally, to compute \[Kappa]col we must include the transition parameters and potential*)


data=Append[KeyDrop[tr[Association],"\[Kappa]col"],colData~Join~{"Potential"->V}];


(* ::Text:: *)
(*kappaCollision returns the collision efficiency and, optionally, the transition parameters computed along the way*)


kappaCollision[data,Return->{"\[Kappa]col","Data"}]//Dataset


(* ::Subsubsection::Closed:: *)
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


(* ::Subsubsection::Closed:: *)
(*Sequential scan*)


(* separator between consecutive runs *)
sep=StringRepeat["\[Placeholder]\[SelectionPlaceholder]",30];


(* file to load DR expressions from *)
DPFile="data/DarkPhoton_DR_Expressions.m";


(* run a benchmark and add the transitions found to a DataFrame *)
Options[run]={Print->True,EchoTiming->True};
run[benchmarkValues_,opt:OptionsPattern[]]:=Module[{print=OptionValue[Print]},
	If[!MatchQ[DS,_Dataset],DS=Dataset[{}]];
	V=ComputeDRPotential[{"gsq","\[Lambda]","\[Mu]sq"}/.benchmarkValues,{1,.01,10}"v"/.benchmarkValues,
		"LoadDRFrom"->DPFile,
		"OrderDR"->"LO",
		"OrderVeff"->"NLO",
		"US"->False,
		"NumericRules"->{Y->1},
		"PlotRG"->False
		];
	If[print,(*Print@sep;*)Echo[Row@{benchmarkValues,
		"\nNumeric potential check: V(v,v/2) \[Rule] ",V["v","v"/2]/.benchmarkValues},"Running AH at \[Rule]"]];
	dsRun=SearchPotential[V,vw,   (* wall velocity must be pre-defined *)
		"TRange"->{0.01,1.3}"v"/.benchmarkValues,"NTracingPoints"->1500,
		"BrokenPhaseScale"->10 "v"/.benchmarkValues,Dataset->True,
		"Metadata"->benchmarkValues,
		MaxIterations->20,
		ProgressIndicator->False,"PlotAction"->False,"PlotPhaseDiagram"->False,Print->False
		]//If[OptionValue[EchoTiming],EchoTiming,Identity];
	If[dsRun=!=Dataset[{}],DS=DS~Join~dsRun]
	]


(* ::Text:: *)
(*Turn off printing messages*)


$PT2GWPrint=False;


(* ::Text:: *)
(*Here we scan the benchmark points defined above (BPs)*)


Scan[run[#,Print->False,EchoTiming->False]&,
	BPs//Normal
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


(* load PT2GW and the Examples on all kernels *)
ParallelEvaluate[
	<<PT2GW`;//Quiet;   (* load paclet. ParallelNeeds will not work properly *)
	<<PT2GW/DRTools.m;  (* load DRTools *)
	$PT2GWPrint=False;  (* turn off printing messages *)
	DefineUnits["keV"]; (* set energy unit *)
	];


(* share between kernels the Dataset container for the transitions *)
SetSharedVariable[DS]


(* ::Text:: *)
(*Define a grid of quartic parameters (\[Lambda],g), fixing the dark photon VEV to 40 keV*)


(* define a grid of scanning parameters *)
scanParameters=With[{n=9},Table[<|"v"->40.,"\[Lambda]"->\[Lambda]0,"g"->g0|>,
	{g0,.2,1.,.1},
	{\[Lambda]0,10^Subdivide[-4.,-1.,n]//Rest}
	]//Flatten//Dataset];
ListPlot[scanParameters[[All,2;;]]//Values,ScalingFunctions->{"Log"},AxesLabel->{"\[Lambda]","g"}]
scanParameters=scanParameters[All,<|#,"\[Mu]sq"->-#\[Lambda] #v^2, "gsq"->#g^2|>&];


(* ::Text:: *)
(*Run the parallel scan, with the 'run' function defined above (this should take a few minutes).*)
(*NB Debugging a parallelized computation might be challenging. It's good practice to first test a couple of benchmarks without parallelization.*)


DS=. (* clear the Dataset if necessary *)


(* run parameters in parallel *)
Parallelize@Scan[Quiet[run[#,Print->False,EchoTiming->False]]&,
	scanParameters//Normal
	];//EchoTiming


(* save/load Dataset *)
(*SetDirectory[NotebookDirectory[]];
Compress[DS]>>data/DarkPhoton_DR_Dataset.m*)
(*Uncompress[<<data/DarkPhoton_DR_Dataset.m]*)


(* ::Text:: *)
(*Visualize scan results *)


(* view Dataset *)
DS


(* plot the scanning and transition parameters *)
With[{scanParams={"\[Lambda]","g"},tranParams={"\[Alpha]","\[Beta]/H"}},
	PointValuePlot[(scanParams->Log10[tranParams])/.Normal[DS],
		{1->"Color",2->"Size"},      (* \[Alpha] <-> color, \[Beta]/H <-> size*)
		ScalingFunctions->{"Log"},
		PlotLegends->Automatic,
		FrameLabel->scanParams
		]/.BarLegend[x___]:>BarLegend[x,LegendLabel->StringTemplate["\!\(\*SubscriptBox[\(log\), \(10\)]\)(``)"]@tranParams[[1]]]
	]


(* reproduce plot from the paper (DR data only) *)
(*Module[{scanParams={"\[Lambda]","g"},tranParams={"\[Alpha]","\[Beta]/H"},grid=Automatic,font={FontSize->8,FontFamily->"Latin Modern Math"}},
	grid=DeleteDuplicates[DS[All,#]//Normal]&/@scanParams;
	Row[PointValuePlot[(scanParams->Log10[#])/.Normal[DS],
		{1->"Color",2->"Size"},
		PlotRangePadding->{{.3,.6},{.05,.1}},
		PlotStyle->PointSize[.04],
		ScalingFunctions->{"Log"},
		ColorFunction->(#/.{"\[Alpha]"->ColorData[{"RedBlueTones","Reverse"}],_->"RedBlueTones"}),
		PlotLegends->Automatic,
		ImageSize->300,
		GridLines->grid,
		Frame->True,
		AspectRatio->1,
		FrameLabel->scanParams
		]/.BarLegend[x___]:>BarLegend[x,LegendLabel->
		StringTemplate["\!\(\*SubscriptBox[\(log\), \(10\)]\) ``"][#]]&/@tranParams[[All]],
		Spacer[0]]]*)
