(* ::Package:: *)

(* ::Text:: *)
(*Dark Cold & Noisy paper*)
(*Decoupled dark sector including a scalar and a U(1) gauge boson.*)


(* ::Title:: *)
(*Daisy resummation + TBounce*)


(* ::Input:: *)
(*Quit*)


(* ::Section:: *)
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


bp=bps[4]
V[S_,T_]=Vtot[S,T]/.\[Mu]sqRule/.\[CapitalLambda]->v/.bp/.RealAbs[S^2]->S^2//Simplify;


V[\[Phi],10.]
