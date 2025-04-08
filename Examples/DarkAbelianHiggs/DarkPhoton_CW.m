(* ::Package:: *)

(* ::Text:: *)
(*Dark Cold & Noisy paper*)
(*Decoupled dark sector including a scalar and a U(1) gauge boson.*)


(* ::Title:: *)
(*Daisy resummation + PT2GW*)


(* ::Input:: *)
(*Quit*)


(* ::Section::Closed:: *)
(*Effective Potential*)


(* ::Subsection::Closed:: *)
(*Tree-level*)


Vtree[S_]=-(1/2)\[Mu]sq S^2+\[Lambda]/8 S^4


minRule=Solve[Vtree'[S]==0/.S->v,v]//Last (* rule for v^2 *)


(* ::Subsection::Closed:: *)
(*1-loop corrections*)


(* ::Subsubsection::Closed:: *)
(*Masses*)


mSsq=\!\(
\*SubscriptBox[\(\[PartialD]\), \({S, 2}\)]\(Vtree[S]\)\)


\[Mu]sqRule=Solve[v==(v/.minRule),\[Mu]sq]//First


(* ::Text:: *)
(*From Goldstone (S is complex)*)


m\[Sigma]sq=-\[Mu]sq + 1/2 \[Lambda] S^2;


mAsq=g^2 S^2;


msq[i_]:=Evaluate[{mSsq,m\[Sigma]sq,mAsq,mAsq,mAsq}/.\[Mu]sqRule//Simplify][[i]]


msq[All]


\!\(
\*SubscriptBox[\(\[PartialD]\), \(x\)]\((
\*SuperscriptBox[\(x\), \(4\)] \((LogCW[
\*SuperscriptBox[\(x\), \(2\)]] - c)\))\)\)//Simplify


(* ::Subsubsection::Closed:: *)
(*CW*)


ClearAll[LogCW];
LogCW[msq_]:=\[Piecewise]{
 {Log[msq/\[CapitalLambda]^2], msq=!=0},
 {Log[\[CurlyEpsilon]/\[CapitalLambda]^2], True}
}


Unprotect[Log];
Log[0.]=Log[\[CurlyEpsilon]/\[CapitalLambda]^2];
Log[0]=Log[\[CurlyEpsilon]/\[CapitalLambda]^2];
Protect[Log];


c[i_]:=Which[MemberQ[{1,2},i],3/2,MemberQ[{3,4,5},i],5/6,True,Print["Bad i=",i," in c[i]"];Abort[]]


Vct[S_]=-(\[Delta]\[Mu]sq/2) S^2+\[Delta]\[Lambda]/8 S^4;


VCW[S_]=1/(64 \[Pi]^2) \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(Length[msq[All]]\)]\(
\*SuperscriptBox[\(msq[i]\), \(2\)] \((LogCW[msq[i]] - c[i])\)\)\)+Vct[S]//Simplify


(*cwExpr=\!\(
\*SubscriptBox[\(\[Limit]\), \(S -> v\)]\({
\*SubscriptBox[\(\[PartialD]\), \(S\)]VCW[S], 
\*SubscriptBox[\(\[PartialD]\), \({S, 2}\)]VCW[S]}\)\)//Simplify//EchoTiming;*)
cwExpr=({VCW'[S],VCW''[S]}/.S->v)//Collect[#,{g,\[Lambda]},Simplify]&//
Echo[#,"",TableForm]&;


(* ::Text:: *)
(*Here we manually remove the divergence!! (following paper)*)


cwExpr=cwExpr/.\[CurlyEpsilon]->\[CapitalLambda]^2//
Echo[#,"",TableForm]&;


cwSol=Solve[Thread[cwExpr==0],{\[Delta]\[Mu]sq,\[Delta]\[Lambda]}]//First//Simplify//Echo[#,"CT sol:",TableForm]&;


(* ::Text:: *)
(*Check divergence*)


VCW''[S]//Collect[#,{g,\[Lambda]},Simplify]&
%/.S->v


\!\(
\*SubscriptBox[\(\[PartialD]\), \(S\)]\((
\*SuperscriptBox[\(m2[S]\), \(2\)] \((LogCW[m2[S]] - c)\))\)\)//Simplify
\!\(
\*SubscriptBox[\(\[PartialD]\), \({S, 2}\)]\((
\*SuperscriptBox[\(m2[S]\), \(2\)] \((LogCW[m2[S]] - c)\))\)\)//Simplify


Vcw[S_]:=1/(64 \[Pi]^2) m2[S]^2 (LogCW[m2[S]]-C[i])


Vcw''[S]//Simplify


\!\(
\*SubscriptBox[\(\[PartialD]\), \(S\)]\(msq[2]\)\)


(* diverging term *)
\!\(
\*SubscriptBox[\(\[PartialD]\), \({S, 2}\)]\((
\*SuperscriptBox[\(msq[2]\), \(2\)] LogCW[msq[2]])\)\)//Simplify


(* ::Subsubsection::Closed:: *)
(*V Thermal*)


(*VT[S_?NumericQ,T_?NumericQ]:=T^4/(2\[Pi]^2) \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(Length[msq[All]]\)]\(NIntegrate[
\*SuperscriptBox[\(x\), \(2\)]Log[1 - Exp[\(-
\*SqrtBox[\(
\*SuperscriptBox[\(x\), \(2\)] + 
\*FractionBox[\(msq[i]\), 
SuperscriptBox[\(T\), \(2\)]]\)]\)]], {x, 0, \[Infinity]}]\)\)*)


(* ::Text:: *)
(*Constructing Jb*)


(*Jb[\[Theta]_?NumericQ]:=Module[{f,f1},*)
(*	f[x_]:=x^2 Log[1-Exp[-Sqrt[x^2+\[Theta]]]];*)
(*	If[\[Theta]>=0,*)
(*	NIntegrate[f[x],{x,0,\[Infinity]}],*)
(*	f1[x_]:=x^2 Log[2Abs[Sin[Sqrt[-x^2-\[Theta]]/2]]];*)
(*	NIntegrate[f1[x],{x,0,Sqrt[Abs[\[Theta]]]}]+NIntegrate[f[x],{x,Sqrt[Abs[\[Theta]]],\[Infinity]}]*)
(*	]]*)


(*DiscretePlot[Jb[\[Theta]],{\[Theta],-5,5,0.1}]*)


(*NIntegrate[x^2 Log[1-Exp[-Sqrt[x^2+msq[i]/T^2]]],{x,0,\[Infinity]}]*)


(*xRange={ArcSinh[-1.3*20],ArcSinh[-(1410./-3.724)20]};*)
(*xlist=Subdivide[Sequence@@xRange,1000];*)
(*xlist=Abs[xRange[[1]]]Sinh[xlist]/20;*)


(*Jb[28.163183606812638`]*)


(*JbList=Re[Jb/@xlist];//EchoTiming*)


(*ListPlot[{xlist,JbList}\[Transpose][[;;600]]]*)


(*JbInt=Interpolation[{xlist,JbList}\[Transpose],Method->"Spline"]*)


(*data={xlist,JbList}\[Transpose];*)


(* ::Text:: *)
(*also  from  CosmoTransitions  data:*)


(*data=Import["/home/marco/anaconda3/lib/python3.1.c~/site-packages/cosmoTransitions/finiteT_b.dat.txt","Data"];*)


(*JbCosmo=Interpolation[data]*)


(*Save["JbInterpolationData.m",data]*)


SetDirectory[NotebookDirectory[]]
JbInt=<<JbInterpolation.m


VT[S_,T_]=T^4/(2\[Pi]^2) \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(Length[msq[All]]\)]\(JbInt[
\*FractionBox[\(msq[i]\), 
SuperscriptBox[\(T\), \(2\)]]]\)\)


VT0[S_,T_]=T^4/12 \!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(Length[msq[All]]\)]\((
\*FractionBox[\(msq[i]\), \(2 
\*SuperscriptBox[\(T\), \(2\)]\)] - 
\*FractionBox[\(1\), \(\[Pi]\)] Re[
\*SuperscriptBox[\((
\*FractionBox[\(msq[i]\), 
SuperscriptBox[\(T\), \(2\)]])\), 
FractionBox[\(3\), \(2\)]]])\)\)


(* ::Subsubsection::Closed:: *)
(*V daisy*)


\[CapitalPi]S[T_]:=(\[Lambda]/6+g^2/4)T^2
\[CapitalPi]A[T_]:=g^2/3 T^2
\[CapitalPi][i_]:={\[CapitalPi]S[T],\[CapitalPi]S[T],\[CapitalPi]A[T],0,0}[[i]]


Vdaisy[S_,T_]=-(T/(12 \[Pi]))\!\(
\*UnderoverscriptBox[\(\[Sum]\), \(i = 1\), \(Length[msq[All]]\)]\(Re[
\*SuperscriptBox[\((msq[i] + \[CapitalPi][i])\), 
FractionBox[\(3\), \(2\)]] - 
\*SuperscriptBox[\(msq[i]\), 
FractionBox[\(3\), \(2\)]]]\)\)//Simplify


(* ::Subsubsection::Closed:: *)
(*V total*)


Vtot[S_,T_]=Vtree[S]+(VCW[S]/.cwSol)+VT[S,T]+Vdaisy[S,T]//Simplify;


Vtot[S_,T_]=Vtot[S,T]/.Log[x_]->Log[RealAbs[x]];


Vtot[S_,T_]=Vtot[S,T]/.Log[x_]->Log[x+10.^-100];


SetDirectory[NotebookDirectory[]];
Vtot[S,T]>>Vtot.m


(* ::Subsection:: *)
(*Benchmarks*)


BPs=Dataset[{
	<|"v"->40.,"g"->0.6,"\[Lambda]"->0.01|>,
	<|"v"->40.,"g"->0.5,"\[Lambda]"->0.001|>, (* no critical temperature *)
	<|"v"->40.,"g"->0.4,"\[Lambda]"->0.001|>, (* critical but no nucleation reached *)
	<|"v"->40.,"g"->0.35,"\[Lambda]"->0.001|>,
	<|"v"->40.,"g"->1.,"\[Lambda]"->0.1|>,
	<|"v"->40.,"g"->0.71,"\[Lambda]"->0.01|> (* strong *)
	}] (* ?? nucleation but percolation condition violated *)
bps[i_]:=KeyMap[ToExpression,BPs[i]//Normal]


SetDirectory[NotebookDirectory[]];
Vtot[S_,T_]=<<"Vtot.m";


\[Mu]sqRule=\[Mu]sq->(v^2 \[Lambda])/2;


bp=bps[6]
V[S_,T_]=Vtot[S,T]/.\[Mu]sqRule/.\[CapitalLambda]->v/.bp/.RealAbs[S^2]->S^2//Simplify;


V[\[Phi],10.]


(* ::Section:: *)
(*TBounce*)


(* ::Subsection:: *)
(*Init*)


(* this is for pre-release only *)
pacletDir="/home/marco/Aveiro/Nerdy/Mathematica nbs/PTFB/TBounce";
PacletDirectoryLoad[pacletDir]
<<TBounce`


DefineUnits["keV"]


vw=1.;


gstarSM=3.38;
\[Xi]=.48;
gstarHS=5;
RelativisticDOF=gstarSM/\[Xi]^4+gstarHS


V[10^2,10.]


bp


PlotPotential[V,{-1,1}1.3v/.bp,.1v/.bp,"LogTRange"->1,"Log\[Phi]Range"->1.5]/.bp


(* ::Subsection::Closed:: *)
(*SearchPhases*)


v/.bp


{0.01,.6}v/.bp


Phases=TracePhases[V,
	"TRange"->{0.01,.3}v/.bp,
	"SymmetricPhaseThreshold",
	"BrokenPhaseScale"->10v/.bp
	]//EchoTiming


vw=0.95;


tr=SearchPhases[V,Phases,vw]


tr[Dataset][{"Tc","Tn","Tp","\[Alpha]","\[Beta]/H","fPeak","h2OmegaPeak"}]


AutoComplete[tr];


{af,Tc,Tn,Tp}=tr[{"ActionFunction","Tc","Tn","Tp"}]


PlotGW[tr]


(* ::Subsection::Closed:: *)
(*SearchPotential*)


vw=0.95;


trs=SearchPotential[V,vw,
	"TRange"->{0.01,.3}v/.bp,"NTracingPoints"->300,
	"PlotPhaseDiagram"->True,	
	"BrokenPhaseScale"->10v/.bp,
	"Metadata"->bp,
	Dataset->True,
	"PrintIterations"->False]//EchoTiming


DS=Dataset[{}]


DS~Join~trs~Join~trs


AppendTo[DS,trs//Normal]


tr=trs[[1]]


tr[Dataset]


AutoComplete[tr];


{phases,af,Tc,Tn,Tp}=tr[{"Phases","ActionFunction","Tc","Tn","Tp"}]


(* ::Subsection:: *)
(*Internal Functions*)


(* ::Subsubsection::Closed:: *)
(*Efficiency of Potential*)


With[{n=10^4},Do[V[S,T],
	{S,Subdivide[.1v,10v,Round[Sqrt@n]]/.bp},
	{T,Subdivide[.1v,10v,Round[Sqrt@n]]/.bp}];//EchoTiming
	]


(* ::Subsubsection::Closed:: *)
(*Derivatives*)


\!\(
\*SubscriptBox[\(\[PartialD]\), \({\[Phi], 2}\)]\(V[\[Phi], 7. ]\)\)/.\[Phi]->10.^-10


TBounce`Private`\[Alpha]Fun[8.2,V,Phases]


ND[V[s,7.],{s,2},0.]


\[Phi]Bscale=10.^6;
\[Phi]th=1.;
With[{T=10.5},
	FindMinimum[V[\[Phi],T],{\[Phi],0.1\[Phi]th,-\[Phi]th,\[Phi]th}]//Echo[#,"\!\(\*SubscriptBox[\(\[Phi]\), \(S\)]\)"]&;
	FindMinimum[V[\[Phi],T],{\[Phi],\[Phi]Bscale,Sequence@@NumericalSort[Sign[\[Phi]Bscale]{\[Phi]th,\[Infinity]}]}]//Echo[#,"\!\(\*SubscriptBox[\(\[Phi]\), \(B\)]\)"]&;
	]


(* ::Subsubsection::Closed:: *)
(*Phase tracing*)


Phases=TracePhases[V,"TRange"->{.1,20.}]


Tc=FindCritical[V,Phases]


(* ::Subsubsection:: *)
(*Nucleation*)


overlap=Overlap[Phases];


DiscretePlot[Action[T,V,Phases],{T,Subdivide[3.68,3.8,10]},ScalingFunctions->"Log"]


FindNucleation[V,{3.68,4.},Phases]


DiscretePlot[Action[T,V,Phases,"Shift"->.5],{T,Subdivide[3.68,3.69,10]},ScalingFunctions->"Log"]


DiscretePlot[DecayOverHubble[T,V,Phases],{T,Subdivide[3.65,3.75,10]},ScalingFunctions->"Log"]


DiscretePlot[DecayOverHubble[T,V,Phases],{T,Subdivide[3.5,4.,10]},ScalingFunctions->"Log"]


DiscretePlot[DecayOverHubble[T,V,Phases],{T,Subdivide[First@overlap,4.,10]},ScalingFunctions->"Log"]


(* ::Text:: *)
(*Nucleation from ActionFunction*)


STfun=af["Function"];


IntegralDecay[5.36,Tc,af["Function"],V,phases]


DiscretePlot[Int\[CapitalGamma]OverH4[T,Tc,STfun,V,phases],{T,Subdivide[5.2,5.4,10]},ScalingFunctions->"Log",Epilog->{InfiniteLine[{1.,Log@0.34},{1,0}]}]


af=ActionFit[V,Phases,{3.6,4.5},Tc,"RefineInflection"->{True,"NHits"->4,"MaxIterations"->30}]


PlotAction[af]


af["Domain"]


FindNucleation[V,{af["Domain"][[1]],3.9},Phases,"NucleationCriterion"->"IntegralDecay",
"NucleationMethod"->{"ActionFunction" -> af["Function"], "Tc" -> Tc, "TnEst" -> 3.69}]


IntegralDecay[af["Domain"][[1]],Tc,af["Function"],V,Phases]


(* ::Subsubsection::Closed:: *)
(*Percolation*)


TpFun[t_?NumericQ]:=IFV[STfun,t,Tc,vw,V,phases]


Tn


0.34000000000001096`


TpFun[Tp]


DiscretePlot[IFV[STfun,T,Tc,vw,V,phases],{T,Subdivide[5.3,5.4,10]},
	ScalingFunctions->"Log",
	Epilog->{InfiniteLine[{1.,Log@0.34},{1,0}]}]


FindRoot[Log[TpFun[T]]==Log[0.34],{T,Tn}]//EchoTiming


TpFun[T/.%277]


(* ::Text:: *)
(*0.34000000000001096`*)


(* ::Subsection:: *)
(*Scan*)


sep=StringRepeat["\[Placeholder]\[SelectionPlaceholder]",30]


(* run a benchmark and add the found transitions to DataFrame *)
run[benchmarkValues_,opt:OptionsPattern[]]:=Module[{},
	If[!MatchQ[DS,_Dataset],DS=Dataset[{}]];
	Print@sep;
	Echo[benchmarkValues,"Running AH at \[Rule]"];
	V[S_,T_]=Vtot[S,T]/.\[Mu]sqRule/.\[CapitalLambda]->v/.benchmarkValues/.RealAbs[S^2]->S^2//Simplify;
	dsRun=SearchPotential[V,vw,
		opt,
		"TRange"->{0.001,.2}v/.benchmarkValues,"NTracingPoints"->600,
		"BrokenPhaseScale"->10v/.benchmarkValues,Dataset->True,
		"Metadata"->KeyMap[ToString,Association[benchmarkValues]],
		"PlotPhaseDiagram"->False,
		"StopAtFailure"->False,
		ProgressIndicator->False
		]//EchoTiming;
	If[dsRun=!=Dataset[{}],DS=DS~Join~dsRun]
	]


bp={v->40.,g->.7,\[Lambda]->0.05};
run[bp]


Options@Action


bp=bpScan[[1,2]]


Options@ActionFit


phases=TracePhases[V,"TRange"->{1,10.}]


Through[phases[3.68373]]


Options@Action


Action[3.695,V,phases]


phases


V[0.1,3.68373]


T=3.68373;
bf=FindBounce[V[x,T],x,Through[phases[T]],"Gradient"->None,"Dimension"->3,"FieldPoints"->51];
BouncePlot[bf]//Echo;
bf["Action"]/T


Action[3.68,V,phases,"ReBounce"->True,"CheckProfile"->True]


Options@FindBounce


With[{fps={21,31,51}},
	DiscretePlot[Table[Action[T,V,phases,"FieldPoints"->fp],{fp,fps}]//Evaluate,{T,Subdivide[3.68,3.7,20]},PlotLegends->fps,PlotRange->Full]
	]//EchoTiming


Show[%90,Epilog->{Dashed,InfiniteLine[{3.6837,300},{0,1}]}]


bp=bpScan[[1,2]]


Options@SearchPhases


run[bp,
	"PlotPhaseDiagram"->True,"PlotAction"->True,"FieldPoints"->21,"NActionPoints"->51,"\[CapitalDelta]TFraction"->0.2,
	"PrintIterations"->False (*,"TRange"->{.1,5.}*)
	]


DS


\[CapitalGamma]OverH4[T,V,phases]


n=9;
bpScan=Table[{v->40.,\[Lambda]->\[Lambda]0,g->g0},
	{g0,.2,1.,.1(*Subdivide[.15,1.,n]*)},
	{\[Lambda]0,10^Subdivide[-4.,-1.,n]}
	];
ListPlot[Flatten[bpScan,1][[All,2;;]]//Values,ScalingFunctions->{"Log"},AxesLabel->{"\[Lambda]","g"}]


run[bpScan[[1,2]],"Plots"->All]


PlotPotential[V,10^3{-1,1},5,"Log\[Phi]Range"->2]


bpScan=Table[{v->40.,g->g0,\[Lambda]->.1},
	{g0,Range[.2,1.,.1]}
	];


DS=.;
Scan[run,
	(*bpScan*)Flatten[bpScan,1]
	]//EchoTiming


DS0=DS


DS=DS~Join~DS0


SetDirectory[NotebookDirectory[]];
DS>>dpScan4.m


DS[Select[#\[Lambda]==0.01\[And]#g==0.7&]]


SetDirectory[NotebookDirectory[]];
DS=<<dpScan4.m;


DSct=Import["/home/marco/Aveiro/Papers/Models/Dark photon/DarkMath/dpScan_CosmoTransitions.csv","Dataset",HeaderLines->1][Select[#"alpha"<10.&]];


Labeled[PointValuePlot[DS[All,{"\[Lambda]","g"}]->Values@DS[All,{"\[Alpha]","\[Beta]/H"}][All,{"\[Alpha]"->Log10}]//Normal,{1->"Color",2->"Size"},
		PlotRange->{Log@{10^-4,10^-1},{.15,1.05}},
		PlotLegends->Automatic,
		ColorFunction->ColorData["RedBlueTones"],
		ScalingFunctions->{"Log"},
		FrameLabel->{"\[Lambda]","g"}],
	"\!\(\*SubscriptBox[\(Log\), \(10\)]\)\[Alpha]",Right]


Labeled[PointValuePlot[DS[All,{"\[Lambda]","g"}]->Values@DS[All,{"\[Alpha]","\[Beta]/H"}][All,{"\[Alpha]"->Log10}]//Normal,{1->"Color",2->"Size"},
		PlotRange->{Log@{10^-4,10^-1},{.15,1.05}},
		PlotLegends->Automatic,
		ColorFunction->ColorData["RedBlueTones"],
		ScalingFunctions->{"Log"},
		FrameLabel->{"\[Lambda]","g"}],
	"\!\(\*SubscriptBox[\(Log\), \(10\)]\)\[Alpha]",Right]


Labeled[PointValuePlot[DS[All,{"\[Lambda]","g"}]->DS[All,#,Log10](*DS[All,{"\[Alpha]"->Log10}][All,#]*)//Normal,"Color",
		PlotRange->{{10^-4,10^-1},{.15,1.05}},
		PlotLegends->Automatic,
		ColorFunction->ColorData[#/.{"\[Alpha]"->{"RedBlueTones","Reverse"},"\[Beta]/H"->"RedBlueTones"}],
		ScalingFunctions->{"Log"},
		FrameLabel->{"\[Lambda]","g"},
		ImageSize->Medium],
	"\!\(\*SubscriptBox[\(Log\), \(10\)]\)"<>#,Right]&/@{"\[Alpha]","\[Beta]/H"}//Row[#,Spacer[20]]&


plotDS=PointValuePlot[DS[All,{"\[Lambda]","g"}]->DS[All,#,Log10](*DS[All,{"\[Alpha]"->Log10}][All,#]*)//Normal,"Color",
		PlotRange->{{10^-4,10^-1},{.15,1.05}},
		PlotStyle->AbsolutePointSize[10],
		GridLines->None,
		PlotLegends->Automatic,
		ColorFunction->ColorData[#/.{"\[Alpha]"->{"RedBlueTones","Reverse"},"\[Beta]/H"->"RedBlueTones"}],
		ScalingFunctions->{"Log"},
		FrameLabel->{"\[Lambda]","g"},
		ImageSize->Medium]&;
Labeled[plotDS[#],"\!\(\*SubscriptBox[\(Log\), \(10\)]\)"<>#,Right]&/@{"\[Alpha]","\[Beta]/H"}//Row[#,Spacer[20]]&


plotct[lab_,crange_:Automatic]:=PointValuePlot[DSct[All,{"lam","g"}]->DSct[All,lab,Log10](*DS[All,{"\[Alpha]"->Log10}][All,#]*)//Normal,"Color",
		PlotRange->{{10^-4,10^-1},{.15,1.05}},
		PlotLegends->Automatic,
		ColorFunction->ColorData[{"RedBlueTones",crange,If[lab=="alpha"\[And]ValueQ@crange,"Reverse",Nothing]}],
		ScalingFunctions->{"Log"},
		FrameLabel->{"lam","g"},
		ImageSize->Medium];
Labeled[plotct[#],"\!\(\*SubscriptBox[\(Log\), \(10\)]\)"<>#,Right]&/@{"alpha","betaH"}//Row[#,Spacer[20]]&


DS[MinMax,"\[Alpha]",Log10]//Normal


PointValuePlot[DSct[All,{"lam","g"}]->DSct[All,"alpha",Log10](*DS[All,{"\[Alpha]"->Log10}][All,#]*)//Normal,"Color",
		PlotRange->{{10^-4,1.3 10^-1},{.15,1.05}},
		PlotStyle->AbsolutePointSize[10],
		PlotLegends->Automatic,
		ColorFunction->ColorData[{"RedBlueTones",Reverse@DS[MinMax,"\[Alpha]",Log10]//Normal}],
		ColorFunctionScaling->False,
		ScalingFunctions->{"Log"},
		FrameLabel->{"lam","g"},
		ImageSize->Medium]


keys=Table[DS[i,{"\[Lambda]","g"}]//Normal,{i,Length@DS}];


keysct=KeyMap[#/."lam"->"\[Lambda]"&]/@Table[DSct[i,{"lam","g"}]//Normal,{i,Length@DSct}];


(* ::Subsubsection:: *)
(*Compare with CT*)


pts=Table[DS[i,{"\[Lambda]","g"}]//Normal//Values,{i,Length@DS}]


cd=ColorData[{"RedBlueTones",Reverse@DS[MinMax,"\[Alpha]",Log10]//Normal}]
cd\[Beta]=ColorData[{"RedBlueTones",DS[MinMax,"\[Beta]/H",Log10]//Normal}]


edgeForm={Black,Thickness[.07]};
mk=Graphics[{cd[#],EdgeForm[edgeForm],Disk[{0,0},1,{\[Pi]/2,3\[Pi]/2}],Sequence@@edgeForm,Circle[]}]&;
mk\[Beta]=Graphics[{cd\[Beta][#],EdgeForm[edgeForm],Disk[{0,0},1,{\[Pi]/2,3\[Pi]/2}],Sequence@@edgeForm,Circle[]}]&;
mks=Table[{mk[\[Alpha]],.05},{\[Alpha],DS[All,"\[Alpha]",Log10]//Normal//Normal}];
mks\[Beta]=Table[{mk\[Beta][\[Beta]],.05},{\[Beta],DS[All,"\[Beta]/H",Log10]//Normal//Normal}];
{mks[[1]],mks\[Beta][[1]]}


ptsCT=Table[DSct[i,{"lam","g"}]//Normal//Values,{i,Length@DSct}]


mkCT=Graphics[{cd[#],EdgeForm[edgeForm],Disk[{0,0},1,{-\[Pi]/2,\[Pi]/2}],Sequence@@edgeForm,Circle[]}]&;
mk\[Beta]CT=Graphics[{cd\[Beta][#],EdgeForm[edgeForm],Disk[{0,0},1,{-\[Pi]/2,\[Pi]/2}],Sequence@@edgeForm,Circle[]}]&;
mksCT=Table[{mkCT[\[Alpha]],.05},{\[Alpha],DSct[All,"alpha",Log10]//Normal//Normal}];
mks\[Beta]CT=Table[{mk\[Beta]CT[\[Beta]],.05},{\[Beta],DSct[All,"betaH",Log10]//Normal//Normal}];
{mksCT[[1]],mks\[Beta]CT[[1]]}


rad=1;
txtsize=12.35;
lg=Graphics[{Thickness[.05],Circle[{0,0},rad],Line[rad{{0,-1},{0,1}}],
		Text[Style["CosmoTransitions",txtsize],{1.5 rad,rad},{Left,Bottom}],
		Text[Style["TBounce",txtsize],{-1.5 rad,rad},{Right,Bottom}]	
		},ImageSize->Small]


fs=22;
font=Sequence[FontSize->fs,FontFamily->"Latin Modern Math"];
Labeled[Show[
ListPlot[{ptsCT}\[Transpose],
	PlotMarkers->mksCT,
	PlotRange->{{10^-4,2 10^-1},{.15,1.05}},
	ScalingFunctions->{"Log"},
	AspectRatio->1,
	AxesLabel->{Style["\!\(\*SubscriptBox[\(\[Lambda]\), \(S\)]\)",font],Style["\!\(\*SubscriptBox[\(g\), \(D\)]\)",font]},
	Ticks->{Table[{10^i, Superscript[10, i]}, {i, -4,-1}],Automatic},
	TicksStyle->Larger,
	GridLines->{10^Subdivide[-4.,-1.,9],Table[g,{g,.2,1.,.1}]},
	PlotLegends->BarLegend[{cd,Reverse@cd[[3]]},ColorFunctionScaling->False,LegendMarkerSize->330],
	Epilog->Inset[lg,{Left,Top},{-7,5}(*{Left,Top},{-24,15}*),Background->White](* coordinates adapted to .pdf *)
	],
ListPlot[{pts}\[Transpose],
	PlotRange->{{10^-4,2 10^-1},{.15,1.05}},
	PlotMarkers->mks,
	ScalingFunctions->{"Log"},
	AspectRatio->1
	]
],Style["\!\(\*SubscriptBox[\(log\), \(10\)]\)"<>" \[Alpha]",font],Right,RotateLabel->True]//TraditionalForm


Export["/home/marco/Aveiro/Papers/Models/Dark photon/DarkMath/scanCW_compare_\[Alpha]_halfDisk_labeled_.pdf",TraditionalForm[%]]


Labeled[Show[
ListPlot[{ptsCT}\[Transpose],
	PlotMarkers->mks\[Beta]CT,
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


Export["/home/marco/Aveiro/Papers/Models/Dark photon/DarkMath/scanCW_compare_\[Beta]_halfDisk.pdf",TraditionalForm[%]]


Labeled[Show[
PointValuePlot[DSct[All,{"lam","g"}][All,{"lam"->(1.1#&)}]->DSct[All,"alpha",Log10](*DS[All,{"\[Alpha]"->Log10}][All,#]*)//Normal,"Color",
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


Labeled[Show[
PointValuePlot[DSct[All,{"lam","g"}][All,{"lam"->(1.1#&)}]->DSct[All,"betaH",Log10](*DS[All,{"\[Alpha]"->Log10}][All,#]*)//Normal,"Color",
		PlotRange->{{10^-4,10^-1},{.15,1.05}},
		FrameLabel->{"\[Lambda]","g"},
		(*PlotStyle\[Rule]AbsolutePointSize[10],*)
		PlotLegends->None,
		ColorFunction->ColorData[{"RedBlueTones",DS[MinMax,"\[Beta]/H",Log10]//Normal}],
		ColorFunctionScaling->False,
		ScalingFunctions->{"Log"},
		FrameLabel->{"lam","g"},
		ImageSize->Medium],
plotDS["\[Alpha]"]
],"\!\(\*SubscriptBox[\(log\), \(10\)]\)"<>"\[Beta]/H",Right]//TraditionalForm


r={"\[Alpha]"->"alpha","\[Beta]/H"->"betaH"};
Show[
PointValuePlot[DS[All,{"\[Lambda]","g"}]->DS[All,#,Log10](*DS[All,{"\[Alpha]"->Log10}][All,#]*)//Normal,"Color",
		PlotRange->{{10^-4,10^-1},{.15,1.05}},
		PlotLegends->Automatic,
		ColorFunction->ColorData[#/.{"\[Alpha]"->{"RedBlueTones","Reverse"},"\[Beta]/H"->"RedBlueTones"}],
		ScalingFunctions->{"Log"},
		FrameLabel->{"\[Lambda]","g"},
		ImageSize->Medium],
PointValuePlot[DSct[All,{"lam","g"}]->DSct[All,#/.r,Log10](*DS[All,{"\[Alpha]"->Log10}][All,#]*)//Normal,"Color",
		PlotRange->{{10^-4,10^-1},{.15,1.05}},
		PlotLegends->None,
		ColorFunction->ColorData[#/.{"\[Alpha]"->{"RedBlueTones","Reverse"},"\[Beta]/H"->"RedBlueTones"}],
		ScalingFunctions->{"Log"},
		FrameLabel->{"lam","g"},
		ImageSize->Medium]
	]&/@{"\[Alpha]"}
