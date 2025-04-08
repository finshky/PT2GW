(* ::Package:: *)

Quit


(* ::Chapter:: *)
(*Abelian Higgs*)


(* ::Section::Closed:: *)
(*Model*)


(* ::Subsection::Closed:: *)
(*Init*)


SetDirectory[NotebookDirectory[]];
$LoadGroupMath=True;
<<DRalgo`


(* remove Context` from all information messages *)
Internal`$ContextMarks = False;


(* ::Subsection::Closed:: *)
(*Group structure*)


Group={"U1"};
CouplingName={g1};
RepAdjoint={0};
Higgs={{Y\[Phi]},"C"}; (* Y\[Phi] = 1 *)
RepScalar={Higgs};


RepFermion={};


(* ::Text:: *)
(*The first element is the vector self-interaction matrix:*)


{gvvv,gvff,gvss,\[Lambda]1,\[Lambda]3,\[Lambda]4,\[Mu]ij,\[Mu]IJC,\[Mu]IJ,Ysff,YsffC}=AllocateTensors[Group,RepAdjoint,CouplingName,RepFermion,RepScalar];


(* ::Subsection::Closed:: *)
(*Potential*)


InputInv={{1,1},{True,False}}; (*This specifies that we want a \[Phi]^+\[Phi] term*)
MassTerm1=CreateInvariant[Group,RepScalar,InputInv]//Simplify//FullSimplify;


VMass=msq*MassTerm1[[1]];(*This is the \[Phi]^+\[Phi] term written in component form*)


\[Mu]ij=GradMass[VMass]//Simplify//SparseArray;


QuarticTerm1=MassTerm1[[1]]^2; (*Because MassTerm1=\[Phi]^+\[Phi], we can write (\[Phi]^+\[Phi])^2=MassTerm1^2*)


VQuartic=\[Lambda]*QuarticTerm1;


\[Lambda]4=GradQuartic[VQuartic];


(* ::Subsection::Closed:: *)
(*Dimensional Reduction*)


ImportModelDRalgo[Group,gvvv,gvff,gvss,\[Lambda]1,\[Lambda]3,\[Lambda]4,\[Mu]ij,\[Mu]IJ,\[Mu]IJC,Ysff,YsffC,Verbose->False];


(* ::Subsubsection::Closed:: *)
(*DRHard*)


PerformDRhard[]
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


posScalar=PrintScalarRepPositions[];
posVector=PrintGaugeRepPositions[];


AnomDim4D["S",{posScalar[[1]],posScalar[[1]]}]
AnomDim4D["V",{posVector[[1]],posVector[[1]]}]


(* ::Subsubsection::Closed:: *)
(*DRSoft*)


(* ::Text:: *)
(*Integrating out temporal scalars:*)


PerformDRsoft[{}];


PrintCouplingsUS[]


PrintScalarMassUS["LO"]
PrintScalarMassUS["NLO"]


(* ::Subsubsection::Closed:: *)
(*Effective Potential*)


(*UseSoftTheory[]*)
(*UseUltraSoftTheory[]*)


DefineNewTensorsUS[\[Mu]ij,\[Lambda]4,\[Lambda]3,gvss,gvvv];
\[Phi]VeV={0,\[Phi]}//SparseArray;
DefineVEVS[\[Phi]VeV];
FieldMasses = PrintTensorsVEV[];


CalculatePotential[];


PrintEffectivePotential["LO"]
PrintEffectivePotential["NLO"]
PrintEffectivePotential["NNLO"]


PrintScalarMass["NLO"]


(* ::Section:: *)
(*Thermal Effective Potential*)


(* ::Subsection:: *)
(*Benchmark point*)


(* ::Text:: *)
(*DRalgo benchmark*)


BPs=Dataset[{
	<|"v"->707.107,"g"->Sqrt[0.42],"\[Lambda]"->0.01,"Unit"->"keV"|>,
	<|"v"->40.,"g"->0.5,"\[Lambda]"->0.001,"Unit"->"keV"|>, (* no critical temperature *)
	<|"v"->40.,"g"->0.4,"\[Lambda]"->0.001,"Unit"->"keV"|>, (* critical but no nucleation reached *)
	<|"v"->40.,"g"->0.35,"\[Lambda]"->0.001,"Unit"->"keV"|>,
	<|"v"->40.,"g"->1.,"\[Lambda]"->0.1,"Unit"->"keV"|>,
	<|"v"->40,"g"->0.71,"\[Lambda]"->0.01,"Unit"->"keV"|> (* strong *)
	}]; (* ?? nucleation but percolation condition violated *)
BPs=BPs[All,<|#,"msq"->-#\[Lambda] #v^2, "gsq"->#g^2|>&][All,Sort]
bps[i_]:=KeyMap[ToExpression,BPs[i]//Normal]


bp=BPs[-1]//Normal
\[Mu]0="v"/.bp//Echo[#,"\!\(\*SubscriptBox[\(\[Mu]\), \(0\)]\) \[Rule]"]&;


(* ::Subsubsection::Closed:: *)
(*Old*)


M0=100.; (* We fix Higgs field mass to some arbitrary number (understood to be in some units of mass). *)
gsq0=0.42; (* We fix the gauge coupling, again, this choice is arbitrary. *)
\[Lambda]0=0.01; (* Finally, we fine tune scalar self coupling to find strong first order phase transition, in perturbation theory. *)
msq0=-M0^2/2;
v=Sqrt[-msq0/\[Lambda]0];
Y\[Phi]=1;
\[Mu]0=100.;


(* ::Text:: *)
(*Dark photon benchmarks*)


v=40. (*keV*);
gsq0=0.39^2;
\[Lambda]0=0.001;
msq0=-\[Lambda]0 v^2;
Y\[Phi]=1;
\[Mu]0=v;


v=40. (*keV*);
gsq0=0.42;
\[Lambda]0=0.01;
msq0=-\[Lambda]0 v^2;
Y\[Phi]=1;
\[Mu]0=0.48v;


v=40. (*GeV*);
gsq0=0.46;
\[Lambda]0=0.01;
msq0=-\[Lambda]0 v^2;
Y\[Phi]=1;
\[Mu]0=0.4v;
bp={"v"->v,"\[Lambda]"->\[Lambda]0,"gsq"->gsq0,"msq"->msq0,"\[Mu]4DRef"->\[Mu]0};


v=40. (*keV*);
gsq0=.35^2;
\[Lambda]0=0.001;
msq0=-\[Lambda]0 v^2;
Y\[Phi]=1;
\[Mu]0=v;
bp={"v"->v,"\[Lambda]"->\[Lambda]0,"gsq"->gsq0,"msq"->msq0,"\[Mu]4DRef"->\[Mu]0};


(* check the unphysical mass m^2=-\[Lambda]v^2 *)
Echo[msq0,"Unphysical \!\(\*SuperscriptBox[\(m\), \(2\)]\) \[Rule]"];
Echo[-\[Lambda]0 v^2,"-\[Lambda]0 \!\(\*SuperscriptBox[\(v\), \(2\)]\) \[Rule]"];
(* physical, tree-level mass Subsuperscript[m, phys, 2]=2\[Lambda]v^2 *)
Echo[2\[Lambda]0 v^2,"\!\(\*SubsuperscriptBox[\(m\), \(phys\), \(2\)]\) \[Rule]"];


(* ::Subsection:: *)
(*DRExport*)


Quit


(*pacletDir="C:\\Users\\Marco\\OneDrive\\Aveiro\\Nerdy\\Mathematica nbs\\PTFB\\TBounce";*)
(*pacletDir="/home/marco/Aveiro/Nerdy/Mathematica nbs/PTFB/Backup/TBounce_24-10-15/"; (* deprecated *)*)
pacletDir="/home/marco/Aveiro/Nerdy/Mathematica nbs/PTFB/TBounce/";
PacletDirectoryLoad[pacletDir];
<<TBounce`
<<TBounce/DRTools.m


(* ::Text:: *)
(*Solve RG & Compute Veff*)


SetDirectory[NotebookDirectory[]];


V=ComputeDRPotential[{"gsq","\[Lambda]","msq"}/.bp,{\[Mu]0,\[Mu]0/10,100 \[Mu]0},
	"LoadDRFrom"->"ahDR_soft.m",
	(*"SubRules"->subRules,"File"->"ahDR_soft.m",*)
	"OrderDR"->"LO",
	"OrderVeff"->"NLO",
	"US"->False,
	"NumericRules"->{Y\[Phi]->1},
	"PlotRG"->True
	]


V[\[Phi],10.]


PlotPotential[Re@*V,10{-\[Mu]0,\[Mu]0},\[Mu]0/10,"Log\[Phi]Range"->2,"LogTRange"->1.5]


\[Beta]ToEquations[DRExpressions["BetaFunctions"],m]


Options@RGSolve


(* ::Subsection:: *)
(*Check/test DR functions*)


(* ::Text:: *)
(*Look at \[Beta] functions and define substitution rules*)


\[Beta]4D=BetaFunctions4D[]


subRules={g1->Sqrt[gsq]};
SubRulesAppend[subRules]


(* ::Text:: *)
(*Store/load DR expressions*)


StoreDRExpressions->


StoreDRExpressions["SubRules"->subRules,"PrintDR"->False,"US"->False(*,"File"->"ahDR_soft.m"*)]


SetDirectory[NotebookDirectory[]];
LoadDRExpressions["ahDR.m"];


DRExpressions//Dataset


(* ::Subsubsection:: *)
(*RG Solver*)


(* ::Text:: *)
(*NB \[Beta] functions are readily saved with the appropriate substitution as BetaFunctions*)


DRExpressions["BetaFunctions"]


RGSolutions=RGSolve[DRExpressions["BetaFunctions"],{"gsq","\[Lambda]","msq"}/.bp,{\[Mu]0,\[Mu]0/10,10 \[Mu]0},"NumericRules"->{Y\[Phi]->1}]//
	Echo[#,"RG solutions",TableForm]&;


PlotRG[All,{\[Mu]0/2,4\[Mu]0},Epilog->{Dashed,Gray,InfiniteLine[{\[Mu]0,0},{0,1}]}]


PlotRG[#,{\[Mu]0/2,4\[Mu]0},
	Epilog->{Dashed,Gray,InfiniteLine[{\[Mu]0,0},{0,1}]},ImageSize->300
	]&/@RGSolutions[[All,1]]


(* ::Subsubsection:: *)
(*Dimensional Reduction*)


(* ::Text:: *)
(*Perform DR step*)


drstep[\[Mu]_,s_:1]:=DRStep[\[Mu]/(\[Pi] s),s,RGSolutions/.((l_->r_):>(l->r[\[Mu]])),"OrderDR"->"NLO","US"->False,"NumericRules"->{Y\[Phi]->1.}]
drstep[\[Mu]0]


(* ::Text:: *)
(*Plot DR quantities*)


PlotDR[gsq3d,{\[Mu]0/3,2\[Mu]0},1.,Epilog->{Dashed,Gray,InfiniteLine[{\[Mu]0,0},{0,1}]},"US"->False,"NumericRules"->{Y\[Phi]->1}]


PlotHighTRatio[msq3d,{\[Mu]0,4\[Mu]0},1.,PlotRange->All,"US"->False]


(* ::Text:: *)
(*Timing*)


(* timing DRStep *)
Module[{\[Mu]4d,n=10^3,\[Mu]4DPrefactor = 1},
Echo[n,"# DRStep evaluations"];
pList=Table[
	\[Mu]4d=\[Mu]4DPrefactor \[Pi] T;
	p4d=RGSolutions/.((l_->r_):>(l->r[\[Mu]4d]));
	DRStep[T,\[Mu]4DPrefactor,p4d,"US"->False,"NumericRules"->{Y\[Phi]->1}],
	{T,Subdivide[\[Mu]0/2,2 \[Mu]0,n]}];//EchoTiming
]


(* ::Subsubsection::Closed:: *)
(*Effective potential*)


Vtest[\[Phi]_,T_]:=Check[EffectivePotentialN[\[Phi],T,RGSolutions,1,"OrderVeff"->"NLO","NumericRules"->{Y\[Phi]->1},"US"->False],
	\!\(
\*SubscriptBox[\(\[Limit]\), \(\[CurlyPhi] -> \[Phi]\)]\(EffectivePotentialN[\[CurlyPhi], T, RGSolutions, 1, "\<OrderVeff\>" -> "\<NLO\>", "\<NumericRules\>" -> {Y\[Phi] -> 1}]\)\)
	]//Quiet


Vtest[\[CurlyPhi],\[Mu]0]


(* efficiency *)
With[{n=32},
	Echo[n^2,"# Veff evaluations"];
	Table[V[\[Phi],T],{\[Phi],Subdivide[0,10 v,n]},{T,Subdivide[\[Mu]0/10,2 \[Mu]0,n]}]//EchoTiming;
	]


(* ::Text:: *)
(*Compare effective potential orders (LO, NLO, NNLO)*)


v="v"/.bp


With[{orders={"LO","NLO","NNLO"}},
Manipulate[
	Plot[Table[Re@EffectivePotentialN[\[Phi],T,RGSolutions,1,"OrderVeff"->o,
			"NumericRules"->{Y\[Phi]->1},"US"->False],{o,orders}
			]//Evaluate,
		{\[Phi],-\[Phi]scale v,\[Phi]scale v},
		PlotLegends->orders,PlotRange->10^logVscale v^4,ImageSize->Large],
	{{T,1.2\[Mu]0},\[Mu]0/10,4\[Mu]0,Appearance->"Labeled"},
	{{\[Phi]scale,10},1,20},
	{{logVscale,-1},-5,2},
	TrackedSymbols:>{T,\[Phi]scale,logVscale}
	]]


(* ::Text:: *)
(*Making an icon*)


(*Module[{T=23.},
	Plot[Table[VeffFun[\[Phi],T,RGSolutions,1,"OrderVeff"->o],{o,{"LO","NLO","NNLO"}}]//Evaluate,{\[Phi],-4v,4v},PlotRange->{-0.007v^4,.01v^4},AspectRatio\[Rule]1,ImageSize->Medium,Axes\[Rule]False,
	PlotStyle\[Rule]Thickness[.02]]]*)


(* ::Subsubsection::Closed:: *)
(*Effective masses*)


Msqeff=Echo[PrintTensorsVEV[1]//Normal,"\!\(\*SubsuperscriptBox[\(m\), \(eff\), \(2\)]\) \[Rule]",MatrixForm];


msqeff=Eigenvalues@Msqeff


drstep[T_,s_:1]:=DRStep[T,s,RGSolutions/.((l_->r_):>(l->r[s \[Pi] T])),"OrderDR"->"NLO"]
msqeff/.(drstep[\[Mu]0]/.{msq3dUS->msq,\[Lambda]3dUS->\[Lambda],gsq3dUS->gsq})


Tlist=Subdivide[\[Mu]0/2,2\[Mu]0,100];
list=Table[msqeff/.(drstep[T]/.{msq3dUS->msq,\[Lambda]3dUS->\[Lambda],gsq3dUS->gsq}),{T,Tlist}]\[Transpose];


(* ::Text:: *)
(*Doesn't seem to work.*)
(*Perhaps there's a problem evaluating msq[phases[[i]][T],T]. *)


msq1=Function[{\[CurlyPhi],T},Evaluate[Interpolation[{Tlist,list[[1]]}\[Transpose]][T]/.\[Phi]->\[CurlyPhi]]]
msq2=Function[{\[CurlyPhi],T},Evaluate[Interpolation[{Tlist,list[[2]]}\[Transpose]][T]/.\[Phi]->\[CurlyPhi]]]


msq1[phases[[1]][T],T]/.T->\[Mu]0


colData={"MassFunctions"->{"Bosons"->{msq1,msq2}},"GaugeCouplings"->{gsq0},"T0"->"Tn","T"->"Tn"};


PrintTensorsVEV[2]//MatrixForm


(* ::Section:: *)
(*Phase Transitions*)


(* ::Subsection::Closed:: *)
(*Parameters*)


DefineUnits["keV"]


$TBouncePrint=True


vw=0.99


gstarSM=3.38;
\[Xi]=.48;
gstarHS=5;
gstar=gstarSM/\[Xi]^4+gstarHS


(* ::Subsection::Closed:: *)
(*SearchPotential*)


trs=SearchPotential[V,vw,
	"TRange"->{\[Mu]0/10,\[Mu]0},
	"PlotPhaseDiagram"->False,
	"PlotAction"->False,"PlotGW"->False,Print->False,
	ProgressIndicator->False
	]//EchoTiming


tr=trs[[1]]


PlotGW[tr]


(* ::Subsubsection::Closed:: *)
(*Analysis*)


tr>>"darkPhoton_transition.m"


ds=TBounce[V,vw,"TRange"->{\[Mu]0/2,1.5\[Mu]0},
	"NTracingPoints"->30,"SymmetricPhaseThreshold"->v/100,
	Dataset->True,
	"PlotAction"->True,"PlotGWSpectrum"->True]//EchoTiming


SetDirectory[NotebookDirectory[]]
tr=<<"darkPhoton_transition.m"


tr=trs[[1]]


h2\[CapitalOmega]Fun[f,"soundwaves"]


{f/.#[[2]],#[[1]]}&@NMaximize[{h2\[CapitalOmega]Fun[f,"soundwaves"],f>0},f]


(* extract results *)
{Tc,Tn,Tp,actionFunction}=tr[{"Tc","Tn","Tp","ActionFunction"}]
{STfun,TvsST}=actionFunction[{"Function","Data"}];
{\[Alpha],\[Beta]H,\[Kappa]}=tr[{"\[Alpha]","\[Beta]/H","\[Kappa]"}];
{fPeak,\[CapitalOmega]Peak}=tr[{"fPeak","h2OmegaPeak"}];


tr[Dataset]


h2\[CapitalOmega]sw[f,Tp,]


PlotGW[tr]


h2Omega[f,Tp,\[Alpha],\[Beta]H,vw,"Sources"->#]&/@{"strong","soundwaves","turbulence"}


%/.f->10^-9


NMaximize[{Log@h2Omega[f,Tp,\[Alpha],\[Beta]H,vw,"Sources"->"turbulence"],f>0},f]
NMaximize[{Log@h2Omega[f,Tp,\[Alpha],\[Beta]H,vw,"Sources"->"soundwaves"],f>0},f]
NMaximize[{Log@h2Omega[f,Tp,\[Alpha],\[Beta]H,vw,"Sources"->"strong"],f>0},f]


(* ::Subsubsection::Closed:: *)
(*Plot GW*)


h2\[CapitalOmega]=Function[{f,s},h2Omega[f,Tp,\[Alpha],\[Beta]H,vw,"Sources"->s]]


h2\[CapitalOmega][1.,"soundwaves"]


PlotGW[h2\[CapitalOmega],"Detectors"->{"LISA","DECIGO"}]


PlotGW[tr]


(* ::Subsection::Closed:: *)
(*Test internal functions*)


(* ::Subsubsection::Closed:: *)
(*Tc*)


Tc=FindTcrit[V,phases]


(* ::Subsubsection::Closed:: *)
(*Tn estimate*)


Options@FindTnuc


{Tn0,STn0,\[CapitalGamma]H40}=FindTnuc[V,{Overlap[phases]//First,Tc},phases,
	Return->{"Tn","NucleationAction","NucleationGammaOverH4"},
	Method->"Bisection",
	"PrintIterations"->True,
	Direction->Up
	]//EchoTiming


(* ::Text:: *)
(*TV shift vs lower ActionTolerance*)


Direction/.Options[Bisection]


ST[14.7,V,phases,"ActionTolerance"->10^-16,"FieldPoints"->31,"CheckProfile"->{True,"ShiftToExitPoint"->True,"LowerActionTolerance"->True},"PrintBounceInfo"->True,BouncePlot->True,PlotRange->All]//EchoTiming


Module[{T=11.,minima},
	minima=Through[phases[T]];
	FindBounce[V[\[Phi],T],\[Phi],minima,
		"FieldPoints"->51,
		"Gradient"->None,
		"MidFieldPoint"->(\[Phi]/.NMaximize[{V[\[Phi],T],\[Phi]~Between~minima},\[Phi]][[2]]),
		"Dimension"->3
		]]


DiscretePlot[\[CapitalGamma]OverH4[T,V,phases],{T,Subdivide[10,12,10]},"ScalingFunctions"->"Log"]


Module[{\[CapitalDelta]T,\[CapitalDelta]Tfrac=0.9,Tmax,Tlow},
		\[CapitalDelta]T=Echo[\[CapitalDelta]Tfrac(Tc-Tn0),"\[CapitalDelta]T"];
		Tmin=Tn0-\[CapitalDelta]T;
		Tmax=Tn0+\[CapitalDelta]T;
		(* refine T range *)
		Tlow=Overlap[phases]//First;
		Tlow+=10^-6 Min[\[CapitalDelta]T,Tn0-Tlow]; (* phases are undetermined at Tlow: numerical shift  *)
		Off[Reduce::ratnz];                  (* Reduce complains with Real number input *)
		{Tmin,Tmax}=RegionBounds[ImplicitRegion[Reduce[{Tmin<t<Tmax,t>Tlow},t],t]]//First;
		On[Reduce::ratnz];
		af=ActionFit[V,phases,{Tmin,Tmax},Tc,
			"NPoints"->101,
			"CheckProfile"->{True,"ShiftToExitPoint"->True,"LowerActionTolerance"->True},
			"PlotAction"->True,Refine->False,
			"StopAtFailure"->True,
			"PrintAction"->False
			]//EchoTiming
]


Tmin


af


af=tr["ActionFunction"]


af["Data"]


With[{Tmin=overlapFun[phases][[1]]},
	FindRoot[Log10[\[CapitalGamma]OverH4[T,V,phases]],
		{T,Mean[{Tmin,Tc}],Tmin,Tc},
		AccuracyGoal->5,PrecisionGoal->\[Infinity]]//EchoTiming
	]


bisection[Log10@\[CapitalGamma]OverH4[#,V,phases]&,{20.,Tc},1.,1.,"Ascending"->False,Direction->Up,"PrintIterations"->True]


af["Function"]


Int\[CapitalGamma]OverH4[af["Domain"]//First//Echo[#,"T \[Rule]"]&,Tc,af["Function"],V,phases]


FindTnuc[V,{Tmin,Tc},phases,
	"NucleationMethod"->{"ActionFunction"->af["Function"]},"NucleationCriterion"->"IntegralGammaOverH4",
	Return->{"Tn","NucleationAction","NucleationGammaOverH4","NucleationIntegralGammaOverH4"},
	Print->True]


Module[{nucFun,s=0,e=0},
	nucFun=Log10@Int\[CapitalGamma]OverH4[#,Tc,af["Function"],V,phases]&;
	res=Reap[FindRoot[nucFun[T]==0,{T,Mean[{Tmin,Tc}],Tmin,Tc},StepMonitor:>s++,EvaluationMonitor:>(e++;Sow[T]),Method->"Secant"]];
	res~Join~{Int\[CapitalGamma]OverH4[T/.res[[1]],Tc,af["Function"],V,phases],"Steps"->s,"Evaluations"->e}
	]


res[[2,1]]


Subdivide[11.,14,10]


ListLinePlot[{#,Log10@Int\[CapitalGamma]OverH4[#,Tc,af["Function"],V,phases]}&/@Subdivide[11.,14,5]]


Tn0


ListPlot[{#,Log10@Int\[CapitalGamma]OverH4[#,Tc,af["Function"],V,phases]}&/@res[[2,1]]]


af["Function"]


fb[T_,fp_:31]:=Module[{max},
	max=\[Phi]/.NMaximize[{Re@V[\[Phi],T],\[Phi]~Between~Through[phases[T]]},\[Phi]][[2]];
	bf=FindBounce[Re@V[\[Phi],T],\[Phi],Through[phases[T]],"MidFieldPoint"->max,
				"FieldPoints"->fp,"ActionTolerance"->10^-6,"Dimension"->3,"Gradient"->None];
	Echo[fp,"FieldPoints"];
	Echo[bf["Action"],"Action"];
	Echo[BounceError[bf],"Error"];
	Print@BouncePlot[bf];
	bf
	]


fbList=Table[fb[fp],{fp,50,150,5}];


ST[15.,V,phases,BouncePlot->True,"ShiftToExitPoint"->False]


Tlist=Subdivide[20,40,10]
ST[V,Tlist,phases]


With[{T=252},bf=FindBounce[V[\[Phi],T],\[Phi],Through[phases[T]],"Dimension"->3,"FieldPoints"->101]];
BouncePlot[bf,PlotRange->All]


BounceError[bf]


Table[bf=FindBounce[V[\[Phi],T],\[Phi],Through[phases[T]],"Dimension"->3,"FieldPoints"->101];
	{T,BounceError[bf],BouncePlot[bf,PlotStyle->Small,PlotRange->All](*,NIntegrate[\!\(
\*SubscriptBox[\(\[PartialD]\), \({\[Rho], 2}\)]\(\(\(bf["\<Bounce\>"]\)[\([1]\)]\)[\[Rho]]\)\),{\[Rho],0.,Max[bf["Radii"]]}]*)},
	{T,Subdivide[2.5\[Mu]0,2.65\[Mu]0,5]}]//
TableForm[#,TableHeadings->{None,{"T","err","Plot"}}]&


Tc=T/.findTcrit[V,phases]


Tlist=Subdivide[16.5,18,5];
STlist=ST[Tlist,V,phases];//EchoTiming


ListPlot[{Tlist,#}\[Transpose]&/@{STlist}]


T=248.;
max=\[Phi]/.NSolve[{\!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Phi]\)]\(V[\[Phi], T]\)\)==0,\!\(
\*SubscriptBox[\(\[PartialD]\), \({\[Phi], 2}\)]\(V[\[Phi], T]\)\)<0,\[Phi]~Between~Through[phases[T]]},\[Phi]]//Mean;
Check[
	fb=FindBounce[V[\[Phi],T],\[Phi],Through[phases[T]],"Dimension"->3,"MidFieldPoint"->max,"Gradient"->None,"FieldPoints"->101];
	Echo[fb//BouncePlot],s
	(* refine the exit point if messages are encountered *)
		{tv,fv}=SortBy[Through[phases[T]],V[#,T]&];
		\[Phi]eq=\[Phi]/.FindRoot[V[\[Phi],T]==V[fv,T],{\[Phi],fv,fv,max}];
		\[Phi]0=\[Phi]eq+0.2(tv-\[Phi]eq);
		fb=FindBounce[V[\[Phi],T],\[Phi],{\[Phi]0,fv},"Dimension"->3,"MidFieldPoint"->max,"ActionTolerance"->10^-12];
		Echo@BouncePlot[fb];
		Echo[fb["Action"]/T,"\!\(\*FractionBox[SubscriptBox[\(S\), \(3\)], \(T\)]\)"];
		fb,
	{FindBounce::cvmit,\!\(TraditionalForm\`FindBounce::errbc\)}
]


(* ::Subsubsection::Closed:: *)
(*S/T[V4] vs S[V3]*)


SetDirectory[NotebookDirectory[]];


V3=ComputeEffectivePotential[{gsq0,\[Lambda]0,msq0},{\[Mu]0,\[Mu]0/10,100 \[Mu]0},
	"LoadDRFrom"->"ahDR.m","OrderDR"->"NLO","OrderVeff"->"NLO","RescaleTo4D"->False]//N


SetOptions[Action,"Action/T"->True];


phases=TracePhases[V,"TRange"->{\[Mu]0/2,2\[Mu]0},
	"BrokenPhaseScale"->10v,"TracingMethod"->"Numeric"]//EchoTiming;


vw=0.95


(* ::Subsubsection::Closed:: *)
(*Phase tracing*)


(* ::Text:: *)
(*Problem!!*)


PlotPotential[V,10{-\[Mu]0,\[Mu]0},\[Mu]0,"Log\[Phi]Range"->3,"LogTRange"->2.5]


phases=TracePhases[V,"TRange"->{\[Mu]0,8\[Mu]0},
	(*"BrokenPhaseScale"->100v,*)"TracingMethod"->"Numeric"]//EchoTiming;


(* ::Text:: *)
(*Testing numerical tracing methods*)


FindMinimum[V[\[Phi],200.],{\[Phi],1000.}]


With[{th=1.,n=30},
	\[Phi]B=Table[Flatten[{T,\[Phi]/.#[[2]]}]&@FindMinimum[V[\[Phi],T],{\[Phi],1000.}],{T,Subdivide[200.,400,n]}]//DeleteCases[#,{_,x_}/;Abs[x]<th]&;
]//EchoTiming


FindRoot[\!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Phi]\)]\(V[\[Phi], 300. ]\)\),{\[Phi],1000}]


With[{th=1.,n=30},
	\[Phi]B=Table[Flatten[{T,\[Phi]/.#}]&@FindRoot[\!\(
\*SubscriptBox[\(\[PartialD]\), \(\[Phi]\)]\(V[\[Phi], T]\)\),{\[Phi],1000.},AccuracyGoal->10,PrecisionGoal->10],{T,Subdivide[200.,400,n]}]//DeleteCases[#,{_,x_}/;Abs[x]<0.1]&;
]//EchoTiming


\[Phi]B


With[{th=1.},
	\[Phi]S=Table[Flatten[{T,\[Phi]/.#[[2]]}]&@NMinimize[{Re[V[\[Phi],T]],Abs[\[Phi]]<1},{\[Phi],100,2000}],{T,Subdivide[200.,400,30]}]//DeleteCases[#,{_,x_}/;Abs[x]>0.1]&;
]//EchoTiming


With[{th=1.,n=100},
	\[Phi]B=Table[Flatten[{T,\[Phi]/.#[[2]]}]&@NMinimize[{Re[V[\[Phi],T]],\[Phi]>1},{\[Phi],100,2000}],{T,Subdivide[200.,400,n]}]//DeleteCases[#,{_,x_}/;Abs[x]-1<0.001]&;
]//EchoTiming


phases=Interpolation[#,"ExtrapolationHandler"->{Indeterminate &,"WarningMessage"->False}]&/@{\[Phi]S,\[Phi]B}


(* ::Subsubsection::Closed:: *)
(*SearchPhases*)


tr=SearchPhases[V,phases,vw,
	"Metadata"->bp,
	"PlotAction"->True,"PlotGW"->True
	]//EchoTiming;


(-8/3-1)/2.15


Show[PlotGW/@{tr,tr3d}]


tr>>dpTransition4d.m


tr3d=<<dpTransition3d.m


{tr,tr3d}


Show[PlotAction/@{tr,tr3d}]


("Tp" "\[Alpha]")/.tr3d[Association]
tr["\[Alpha]"]


tr[Dataset]


Show[PlotGW/@{tr,tr3d}]


SetDirectory[NotebookDirectory[]]
tr=<<"darkPhoton_transition.m"


(* extract results *)
{Tc,Tn,Tp,actionFunction}=tr[{"Tc","Tn","Tp","ActionFunction"}]
{STfun,TvsST}=actionFunction[{"Function","Data"}];
{fPeak,gwPeak}=tr[{"fPeak","h2OmegaPeak"}];


(* ::Text:: *)
(*Compare nucleation criterions*)


FindRoot[Log10@\[CapitalGamma]OverH4[T,STfun,V,phases],{T,Tn}]
FindRoot[Log10@Int\[CapitalGamma]OverH4[T,Tc,STfun,V,phases],{T,Tn}]


\[CapitalGamma]OverH4[T,V,phases]/.%407


(* ::Subsection:: *)
(*Scan parameter space*)


(* ::Subsubsection:: *)
(*Scan*)


sep=StringRepeat["\[Placeholder]\[SelectionPlaceholder]",30]


(* run a benchmark and add the transition found to the DS DataFrame *)
Options[run]={Print->True};
run[benchmarkValues_,opt:OptionsPattern[]]:=Module[{print=OptionValue[Print]},
	If[!MatchQ[DS,_Dataset],DS=Dataset[{}]];
	If[print,Print@sep;Echo[benchmarkValues,"Running AH at \[Rule]"]];
	V=ComputeEffectivePotential[{"gsq","\[Lambda]","msq"}/.benchmarkValues,{1,.01,10}"v"/.benchmarkValues,
		"LoadDRFrom"->"ahDR_soft.m",
		"OrderDR"->"NLO",
		"OrderVeff"->"NLO",
		"US"->False,
		"NumericRules"->{Y\[Phi]->1},
		"PlotRG"->False
		];
	dsRun=SearchPotential[V,vw,
		opt,
		"TRange"->{0.01,1.3}"v"/.benchmarkValues,"NTracingPoints"->1500,
		"BrokenPhaseScale"->10 "v"/.benchmarkValues,Dataset->True,
		"Metadata"->benchmarkValues,
		MaxIterations->20,
		ProgressIndicator->False,"PlotAction"->False,"PlotPhaseDiagram"->False,Print->False
		]//EchoTiming;
	If[dsRun=!=Dataset[{}],DS=DS~Join~dsRun]
	]



run[bp,Print->False]


n=9;
bpScan=Table[<|"v"->40.,"\[Lambda]"->\[Lambda]0,"g"->g0|>,
	{g0,.2,1.,.1(*Subdivide[.15,1.,n]*)},
	{\[Lambda]0,10^Subdivide[-4.,-1.,n]//Rest}
	]//Flatten//Dataset;
ListPlot[bpScan[[All,2;;]]//Values,ScalingFunctions->{"Log"},AxesLabel->{"\[Lambda]","g"}]
bpScan=bpScan[All,<|#,"msq"->-#\[Lambda] #v^2, "gsq"->#g^2|>&];


bp=bpScan[42]//Normal


phases=TracePhases[V,"TRange"->{4.,20},"NTracingPoints"->1000]


\[CapitalDelta]V[T_]:=Subtract@@(V[#[T],T]&/@phases);
NSolve[\[CapitalDelta]V[T]==0,T]
FindRoot[\[CapitalDelta]V[T],{T,Mean@{13.584,15.36},13.584,15.36}]


FindTcrit[V,phases] 


With[{T=15},
V[phases[[1]][T],T]-V[phases[[2]][T],T]
]


\[CapitalGamma]OverH4[4.7,V,phases]


run[bp,"Plots"->All,"TRange"->{4.,20},ProgressIndicator->True]


Bisection[Log@\[CapitalGamma]OverH4[#,V,phases]&, {5.97}, 0.1, 0.1]


Options@Scan


bpScan


Scan[run[#,Print->False]&,
	bpScan//Normal
	]//EchoTiming


DS>>dpScan_DR.m


<<dpScan_DR.m


ds[SortBy["Tn"]][All,{"Tn","\[Alpha]","\[Lambda]","g"}]


(* ::Subsubsection:: *)
(*Parallel scan*)


ParallelEvaluate[
	PacletDirectoryLoad[pacletDir];
	<<TBounce`;
	<<TBounce/DRTools.m;
	DefineUnits["keV"];
	$TBouncePrint=False;
	]


SetSharedVariable[DS]


$TBouncePrint=False


ParallelEvaluate[$TBouncePrint=False]


SetSharedVariable[DS]


DS=Dataset[{}]


Parallelize@Scan[(V=ComputeEffectivePotential[{"gsq","\[Lambda]","msq"}/.#,{1,.01,10}"v"/.#,
		"LoadDRFrom"->"ahDR_soft.m",
		"OrderDR"->"NLO",
		"OrderVeff"->"NLO",
		"US"->False,
		"NumericRules"->{Y\[Phi]->1},
		"PlotRG"->False
		];
		dsRun=SearchPotential[V,vw,
		"TRange"->{0.01,1.3}"v"/.#,"NTracingPoints"->1500,
		"BrokenPhaseScale"->10 "v"/.#,Dataset->True,
		"Metadata"->#,
		MaxIterations->20,
		ProgressIndicator->False,"PlotAction"->False,"PlotPhaseDiagram"->False,Print->False
		]//Quiet;
		If[dsRun=!=Dataset[{}],DS=DS~Join~dsRun];
		)&,bpScan//Normal]//EchoTiming


UnitConvert[Quantity[400,"s"],MixedUnit[{"Minutes","s"}]]


DS0=DS


DS>>dpScan_DR_NLO.m


(* ::Subsubsection:: *)
(*Compare with CW*)


SetDirectory[NotebookDirectory[]]


DS=<<dpScan_DR.m;


DS0[Select[MissingQ[#["\[Alpha]"]]&]]


DS=DS0[Select[!MissingQ[#["\[Alpha]"]]&]]


DScw=<<dpScan4.m;


pts=Table[DS[i,{"\[Lambda]","g"}]//Normal//Values,{i,Length@DS}];


cd=ColorData[{"RedBlueTones",Reverse@DS[MinMax,"\[Alpha]",Log10]//Normal}]
cd\[Beta]=ColorData[{"RedBlueTones",DS[MinMax,"\[Beta]/H",Log10]//Normal}]


edgeForm={Black,Thickness[.07]};
mk=Graphics[{cd[#],EdgeForm[edgeForm],Disk[{0,0},1,{-\[Pi]/2,\[Pi]/2}],Sequence@@edgeForm,Circle[]}]&;
mk\[Beta]=Graphics[{cd\[Beta][#],EdgeForm[edgeForm],Disk[{0,0},1,{-\[Pi]/2,\[Pi]/2}],Sequence@@edgeForm,Circle[]}]&;
mks=Table[{mk[\[Alpha]],.05},{\[Alpha],DS[All,"\[Alpha]",Log10]//Normal//Normal}];
mks\[Beta]=Table[{mk\[Beta][\[Beta]],.05},{\[Beta],DS[All,"\[Beta]/H",Log10]//Normal//Normal}];
{mks[[1]],mks\[Beta][[1]]}


ptsCW=Table[DScw[i,{"\[Lambda]","g"}]//Normal//Values,{i,Length@DScw}];


mkCW=Graphics[{cd[#],EdgeForm[edgeForm],Disk[{0,0},1,{\[Pi]/2,3\[Pi]/2}],Sequence@@edgeForm,Circle[]}]&;
mk\[Beta]CW=Graphics[{cd\[Beta][#],EdgeForm[edgeForm],Disk[{0,0},1,{\[Pi]/2,3\[Pi]/2}],Sequence@@edgeForm,Circle[]}]&;
mksCW=Table[{mkCW[\[Alpha]],.05},{\[Alpha],DScw[All,"\[Alpha]",Log10]//Normal//Normal}];
mks\[Beta]CW=Table[{mk\[Beta]CW[\[Beta]],.05},{\[Beta],DScw[All,"\[Beta]/H",Log10]//Normal//Normal}];
{mksCW[[1]],mks\[Beta]CW[[1]]}


rad=.1;
txtsize=13.5;
lg=Graphics[{Thickness[.05],Circle[{0,0},rad],Line[rad{{0,-1},{0,1}}],
		Text[Style["DR",txtsize],{1 rad,rad},{Left,Bottom}],
		Text[Style["CW",txtsize],{-1 rad,rad},{Right,Bottom}]	
		},ImageSize->60]


fs=22;
font=Sequence[FontSize->fs,FontFamily->"Latin Modern Math"];
Labeled[Show[
ListPlot[{ptsCW}\[Transpose],
	PlotMarkers->mksCW,
	PlotRange->{{10^-4,2 10^-1},{.15,1.05}},
	ScalingFunctions->{"Log"},
	AspectRatio->1,
	AxesLabel->{Style["\!\(\*SubscriptBox[\(\[Lambda]\), \(S\)]\)",font],Style["\!\(\*SubscriptBox[\(g\), \(D\)]\)",font]},
	Ticks->{Table[{10^i, Superscript[10, i]}, {i, -4,-1}],Automatic},
	TicksStyle->Larger,
	GridLines->{10^Subdivide[-4.,-1.,9],Table[g,{g,.2,1.,.1}]},
	PlotLegends->BarLegend[{cd,Reverse@cd[[3]]},ColorFunctionScaling->False,LegendMarkerSize->330],
	Epilog->Inset[lg,{Left,Top},{-.8,1.3}(*{Left,Top},{-24,15}*),Background->White](* coordinates adapted to .pdf *)
	],
ListPlot[{pts}\[Transpose],
	PlotRange->{{10^-4,2 10^-1},{.15,1.05}},
	PlotMarkers->mks,
	ScalingFunctions->{"Log"},
	AspectRatio->1
	]
],Style["\!\(\*SubscriptBox[\(log\), \(10\)]\)"<>" \[Alpha]",font],Right,RotateLabel->True]


Export["/home/marco/Aveiro/Papers/Models/Dark photon/DarkMath/figs/scanDR_NLO_compare_\[Alpha]_halfDisk_labeled.pdf",TraditionalForm[%]]


Labeled[Show[
ListPlot[{ptsCW}\[Transpose],
	PlotMarkers->mks\[Beta]CW,
	PlotRange->{{10^-4,2 10^-1},{.15,1.05}},
	ScalingFunctions->{"Log"},
	AspectRatio->1,
	AxesLabel->{Style["\!\(\*SubscriptBox[\(\[Lambda]\), \(S\)]\)",font],Style["\!\(\*SubscriptBox[\(g\), \(D\)]\)",font]},
	Ticks->{Table[{10^i, Superscript[10, i]}, {i, -4,-1}],Automatic},
	TicksStyle->Larger,
	GridLines->{10^Subdivide[-4.,-1.,9],Table[g,{g,.2,1.,.1}]},
	PlotLegends->BarLegend[{cd\[Beta],cd\[Beta][[3]]},ColorFunctionScaling->False,LegendMarkerSize->330]
	],
ListPlot[{pts}\[Transpose],
	PlotRange->{{10^-4,2 10^-1},{.15,1.05}},
	PlotMarkers->mks\[Beta],
	ScalingFunctions->{"Log"},
	AspectRatio->1
	]
],Style["\!\(\*SubscriptBox[\(log\), \(10\)]\)"<>" \[Beta]/H",font],Right,RotateLabel->True]//TraditionalForm


Export["/home/marco/Aveiro/Papers/Models/Dark photon/DarkMath/scanDR_NLO_compare_\[Beta]_halfDisk.pdf",TraditionalForm[%]]


Labeled[Show[
PointValuePlot[DScw[All,{"lam","g"}][All,{"lam"->(1.1#&)}]->DScw[All,"alpha",Log10](*DS[All,{"\[Alpha]"->Log10}][All,#]*)//Normal,"Color",
		PlotRange->{{10^-4,10^-1},{.15,1.05}},
		PlotStyle->AbsolutePointSize[10],
		FrameLabel->{"\[Lambda]","g"},
		PlotLegends->None,
		ColorFunction->ColorData[{"RedBlueTones",Reverse@DS[MinMax,"\[Alpha]",Log10]//Normal}],
		ColorFunctionScaling->False,
		ScalingFunctions->{"Log"},
		FrameLabel->{"lam","g"},
		ImageSize->Medium],
	plotDS["\[Alpha]"]
],"\!\(\*SubscriptBox[\(log\), \(10\)]\)"<>"\[Alpha]",Right]//TraditionalForm
