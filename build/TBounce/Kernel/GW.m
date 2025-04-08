(* ::Package:: *)

(* ::Section:: *)
(*Begin Package*)


BeginPackage["GW`"]


(* ::Subsection:: *)
(*Available public Symbols*)


(* high-level Symbols *)
GWData;
(*h2Omega; (* GW amplitude *)
h2\[CapitalOmega]col;
h2\[CapitalOmega]sw;
h2\[CapitalOmega]mhd;*)
ComputeSGWB;
PISC; (* peak-integrated sensitivity curves *)


(* phase transition parameters *)
{\[CapitalDelta]m2Fun,g2\[CapitalDelta]mvFun,\[Alpha]\[Infinity]Fun,\[Alpha]eqFun,\[Gamma]eqFun,\[Gamma]starFun};
\[Kappa]ColFun;
KColFun;
\[Kappa]swFun;
KswFun;
\[Beta]HFun;
HRFrom\[Beta]H;
\[Beta]HFromHR;
HRFun;
nBFun;
cs;


(* public TBounce` symbols *)
H;
Subscript[\[Rho], \[Gamma]];


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


(* ::Subsubsection::Closed:: *)
(*Initialize GWData*)


If[!MatchQ[GWData,_Association],GWData=<||>];


(* ::Subsection:: *)
(*Constants*)


cs=1/Sqrt[3]; (* radiation sound speed *)


(* ::Subsection:: *)
(*Phase transition parameters*)


(* ::Subsubsection:: *)
(*Bubble radius & number density*)


(* bubble number density *)
nBFun[ST_Function|ST_Symbol,T_?NumericQ,Tc_,vw_,opt:OptionsPattern[]]:=T^3 NIntegrate[\[CapitalGamma][\[ScriptCapitalT],ST,opt]E^-IFV[ST,\[ScriptCapitalT],Tc,vw]/(\[ScriptCapitalT]^4 H[T]),{\[ScriptCapitalT],T,Tc}]
nBFun[ST_Function|ST_Symbol,T_?NumericQ,Tc_,vw_,V_,phases_,opt:OptionsPattern[]]:=T^3 NIntegrate[\[CapitalGamma][\[ScriptCapitalT],ST,opt]E^-IFV[ST,\[ScriptCapitalT],Tc,vw]/(\[ScriptCapitalT]^4 H[\[ScriptCapitalT],V,phases]),{\[ScriptCapitalT],T,Tc}]
(*Rstar[ST_Function|ST_Symbol,T_?NumericQ,Tc_,vw_,opt:OptionsPattern[]]:=nBFun[ST,T,Tc,vw,opt]^(-1/3)*)
HRFun[ST_Function|ST_Symbol,T_?NumericQ,Tc_,vw_,opt:OptionsPattern[]]:=H[T]nBFun[ST,T,Tc,vw,opt]^(-1/3)
HRFun[ST_Function|ST_Symbol,T_?NumericQ,Tc_,vw_,V_,phases_,opt:OptionsPattern[]]:=H[T,V,phases]nBFun[ST,T,Tc,vw,V,phases,opt]^(-1/3)
HRFrom\[Beta]H[\[Beta]H_?NumericQ,vw_]:=(8\[Pi])^(1/3)Max[vw,cs]/\[Beta]H


(* ::Subsubsection:: *)
(*\[Beta]H*)


\[Beta]HFromHR[HR_?NumericQ,vw_?NumericQ,cs_:cs]:=(8\[Pi])^(1/3) Max[vw,cs]/HR
\[Beta]HFromHR[ST_Function|ST_Symbol,T_?NumericQ,Tc_,vw_,cs_:cs,opt:OptionsPattern[]]:=(8\[Pi])^(1/3) Max[vw,cs]/HRFun[ST,T,Tc,vw,opt]


(* ::Subsubsection:: *)
(*Strengths \[Alpha]*)


(* ::Text:: *)
(*Mass functions*)


(* mass functions *)
\[CapitalDelta]m2Fun[\[CapitalDelta]mf_,\[CapitalDelta]mv_,nf_:1,nv_:1]:=Total[1/2 (nf \[CapitalDelta]mf^2)]+Total[nv \[CapitalDelta]mv^2]
g2\[CapitalDelta]mvFun[g_,\[CapitalDelta]mv_,nv_:1]:=g^2 nv \[CapitalDelta]mv//Total


\[Alpha]\[Infinity]Fun[\[CapitalDelta]m2_,T_]:=1/24 \[CapitalDelta]m2 T^2/Subscript[\[Rho], \[Gamma]][T]
(*\[Alpha]\[Infinity]Fun[T_,\[Phi]_]:=4.9 10^-3 (\[Phi]/T)^2*) (* minimum run-away alpha for SM-like theory *)
\[Alpha]eqFun[g2\[CapitalDelta]mv_,T_]:=g2\[CapitalDelta]mv T^3/Subscript[\[Rho], \[Gamma]][T]


(* attempt to compute R0 from action potential contribution and fast expansion, see 1903.09642 *)
(*E0V[Action_,T_]:=-Action[T]/2 (* E_0_V  *)
R0Fun[E0V_,\[CapitalDelta]V_]:=(3 E0V/(4\[Pi] \[CapitalDelta]V))^(1/3)
Req=1/Sqrt[1-(R'[t])^2]==(2R[t])/(3 Rc)+Rc^2/(3 R[t]^2);
Rsol[Rc_,\[Delta]_:10^-5]:=NDSolve[{Req,R[0]==(1+\[Delta])Rc},R,{t,0,100}][[2]]
RstarFun[T_]:=With[{sol=Rsol[Rc,\[Delta]]},(R/.sol)[T]]*)
RFun[T_,action_,V_,phases_,\[Xi]w_]:=(8\[Pi])^(1/3)Max[\[Xi]w,cs]/\[Beta]HFun[T,action]/H[T,V,phases]


\[Gamma]eqFun[\[Alpha]_,\[Alpha]\[Infinity]_,\[Alpha]eq_]:=(\[Alpha]-\[Alpha]\[Infinity])/\[Alpha]eq
(*\[Gamma]starFun[action_,\[Beta]overH_,T_,T0_,V_,phases_,\[Xi]w_]:=2/3 \[Xi]w (8\[Pi])^(1/3)/(H[T,V,phases]\[Beta]overH)/((3 action[T0])/(2\[Pi] Abs[V[phases[[1]][T0],T0]-V[phases[[2]][T0],T0]]))^(1/3)*)
\[Gamma]starFun[R0_,Rstar_]:=2/3 Rstar/R0
(* T0 is the temperature that gives the initial radius R0. Here we'll take it to be T nucleation, which should be okay if no supercooling *)


(* ::Subsubsection:: *)
(*K, \[Kappa] - efficiency*)


(* ::Text:: *)
(*Efficiency factor for bubble wall collisions, from *)


(* efficiency & relative coefficients *)
(*\[Kappa]ColFun[\[Alpha]_,\[Alpha]\[Infinity]_]:=1-\[Alpha]\[Infinity]/\[Alpha]*)
\[Kappa]ColFun[\[Gamma]star_,\[Gamma]eq_,\[Alpha]_,\[Alpha]\[Infinity]_]:=\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"1", "-", 
RowBox[{"\[Alpha]\[Infinity]", "/", "\[Alpha]"}]}], 
RowBox[{"\[Gamma]star", "<=", "\[Gamma]eq"}]},
{
RowBox[{
RowBox[{"\[Gamma]eq", "/", "\[Gamma]star"}], " ", 
RowBox[{"(", 
RowBox[{"1", "-", 
RowBox[{
RowBox[{"\[Alpha]\[Infinity]", "/", "\[Alpha]"}], " ", 
RowBox[{
RowBox[{"(", 
RowBox[{"\[Gamma]eq", "/", "\[Gamma]star"}], ")"}], "^", "2"}]}]}], ")"}]}], 
RowBox[{"\[Gamma]star", ">", "\[Gamma]eq"}]}
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
StripWrapperBoxes->True]\)
Options[\[Kappa]ColFun]={Return->"\[Kappa]col"};
\[Kappa]ColFun[colData_Association,OptionsPattern[]]:=Module[{
	data=<||>,\[Kappa]ColRequirements,missing,
	massFun,T0,T,V,phases,vw,action,\[Alpha],\[Beta]H,
	\[CapitalDelta]mf,\[CapitalDelta]mv,nf,nv,gaugeCoup,\[CapitalDelta]m2,g2\[CapitalDelta]mv,R0,Rstar,\[Alpha]\[Infinity],\[Alpha]eq,\[Gamma]eq,\[Gamma]star,
	\[Kappa]Col,return=OptionValue[Return]
	},
	(* \[Kappa]col provided *)
	If[MatchQ[colData,_Association|_List]\[And]Lookup[colData,"\[Kappa]col",False,NumericQ],
		\[Kappa]Col=colData["\[Kappa]col"];
		Return[return/.{"\[Kappa]col"->\[Kappa]Col,"Data"->None}]
		];
	(* \[Kappa]col computed from data *)
	\[Kappa]ColRequirements={"MassFunctions","GaugeCouplings","T0","T","Potential","Phases","vw","ActionFunction","\[Alpha]","\[Beta]/H"};
	AssociateTo[data,KeyTake[colData,\[Kappa]ColRequirements]];
	If[(missing=Complement[\[Kappa]ColRequirements,Keys[data]])!={},
		Print@StringTemplate["Missing `` in CollisionData.\n\!\(\*SubscriptBox[\(\[Kappa]\), \(col\)]\) is Indeterminate."][missing];
		Return[return/.{"\[Kappa]col"->Indeterminate,"Data"->None}]
		];
	Check[
		{V,phases,vw,action,\[Alpha],\[Beta]H}={"Potential","Phases","vw","ActionFunction","\[Alpha]","\[Beta]/H"}/.data;
		(*If[MatchQ[action,_ActionFunction],action=action["Function"]];*)
		action=Check[action["Function"],action]//Quiet;
		{T0,T}={"T0","T"}/.data;
		massFun=({"Fermions","Bosons"}/.("MassFunctions"/.data)/.{"Fermions"->{},"Bosons"->{}});
		{\[CapitalDelta]mf,\[CapitalDelta]mv}=Map[#[phases[[2]][T],T]-#[phases[[1]][T],T]&,massFun,{2}];
		{nf,nv}={"nF","nB"}/.data/.{"nF"->1,"nB"->1}; (* number of fermions and bosons. Set to 1 if not defined. *)
		gaugeCoup="GaugeCouplings"/.data/.{}->0.;
		\[CapitalDelta]m2=\[CapitalDelta]m2Fun[\[CapitalDelta]mf,\[CapitalDelta]mv,nf,nv];
		g2\[CapitalDelta]mv=g2\[CapitalDelta]mvFun[gaugeCoup,\[CapitalDelta]mv,nv];
		\[Alpha]\[Infinity]=\[Alpha]\[Infinity]Fun[\[CapitalDelta]m2,T];
		\[Alpha]eq=\[Alpha]eqFun[g2\[CapitalDelta]mv,T];
		\[Gamma]eq=\[Gamma]eqFun[\[Alpha],\[Alpha]\[Infinity],\[Alpha]eq];
		R0=RFun[T0,action,V,phases,vw];
		Rstar=RFun[T,action,V,phases,vw];
		\[Gamma]star=\[Gamma]starFun[R0,Rstar];
		\[Kappa]Col=\[Kappa]ColFun[\[Gamma]star,\[Gamma]eq,\[Alpha],\[Alpha]\[Infinity]],
		(* handle failed computation *)
		Print["Computation of collision efficiency failed. \!\(\*SubscriptBox[\(\[Kappa]\), \(col\)]\) is Indeterminate."];
		\[Kappa]Col=Indeterminate
		];
	OptionValue[Return]/.{"\[Kappa]col"->\[Kappa]Col,"Data"->AssociationThread[
		{"\[CapitalDelta]mf","\[CapitalDelta]mv","nF","nV","gaugeCoup","\[CapitalDelta]m2","g2\[CapitalDelta]mv","R0","Rstar","\[Alpha]\[Infinity]","\[Alpha]eq","\[Gamma]eq","\[Gamma]star"}->{\[CapitalDelta]mf,\[CapitalDelta]mv,nf,nv,gaugeCoup,\[CapitalDelta]m2,g2\[CapitalDelta]mv,R0,Rstar,\[Alpha]\[Infinity],\[Alpha]eq,\[Gamma]eq,\[Gamma]star}]}
	]
(* if no data is provided *)
\[Kappa]ColFun[_?(Not@*NumericQ),OptionsPattern[]]:=Module[{},
	Print["No GWs from bubble collisions: \!\(\*SubscriptBox[\(\[Kappa]\), \(col\)]\) is Indeterminate. \n    Collisions' efficiency must be provided or computed via mass functions."];
	OptionValue[Return]/.{"\[Kappa]col"->Indeterminate,"Data"->None}
	]
KColFun[\[Alpha]_,\[Kappa]col_]:=\[Kappa]col \[Alpha]/(1+\[Alpha]) (* fractional kinetic energy density for strong (collision) PTs *)


(* ::Text:: *)
(*The efficiency coefficient \[Kappa]sw indicates the fraction of latent heat that is transformed into bulk motion of the plasma and finally into GWs.*)
(*NB Expression from eq. 37 of Espinosa et al. (JCAP.2010.06.028).*)


(* soundwaves *)
\[Alpha]EffFun[\[Alpha]_,\[Kappa]col_?NumericQ]:=(1-\[Kappa]col)\[Alpha]
\[Alpha]EffFun[\[Alpha]_,\[Kappa]col_]:=\[Alpha]    (* explicitly: \[Alpha]EffFun[\[Alpha]_,\[Kappa]col_?(Not@*NumericQ)]:=\[Alpha] *)
\[Kappa]swFun[\[Alpha]_,vw_,cs_:cs]:=Module[{vJ,\[Kappa]A,\[Kappa]B,\[Kappa]C,\[Kappa]D,\[Delta]\[Kappa]},
	vJ=(Sqrt[2/3\[Alpha]+\[Alpha]^2]+1/Sqrt[3])/(1+\[Alpha]);
	\[Kappa]A=vw^(6/5) 6.9\[Alpha]/(1.36-0.037Sqrt[\[Alpha]]+\[Alpha]);
	\[Kappa]B=\[Alpha]^(2/5)/(0.017+(0.997+\[Alpha])^(2/5));
	\[Kappa]C=Sqrt[\[Alpha]]/(0.135+Sqrt[0.98+\[Alpha]]);
	\[Kappa]D=\[Alpha]/(0.73+0.083Sqrt[\[Alpha]]+\[Alpha]);
	(* efficiency coefficient \[Kappa] *)
	Which[
		vw<=cs, cs^(11/5)\[Kappa]A \[Kappa]B/((cs^(11/5)-vw^(11/5))\[Kappa]B + vw cs^(6/5)\[Kappa]A),
		cs<vw<=vJ, \[Delta]\[Kappa]=-0.9 Log[Sqrt[\[Alpha]]/(1+Sqrt[\[Alpha]])];
			\[Kappa]B + (vw-cs)\[Delta]\[Kappa] + ((vw-cs)/(vJ-cs))^3 (\[Kappa]C-\[Kappa]B-(vJ-cs)\[Delta]\[Kappa]),
		vJ<vw, (vJ-1)^3 vJ^(5/2)vw^(-5/2)\[Kappa]C \[Kappa]D/(((vJ-1)^3-(vw-1)^3)vJ^(5/2)\[Kappa]C + (vw-1)^3 \[Kappa]D)
		]
	]
(*\[Kappa]swFun[\[Alpha]_]:=\[Alpha]/(0.73+0.083Sqrt[\[Alpha]]+\[Alpha])*) (* 2403.03723v1, for vw\[TildeTilde]1 *)
(*\[Kappa]swFun[\[Alpha]_]:=(1/(1+0.715\[Alpha]))(0.715\[Alpha]+(4/27)\[Sqrt]((3/2)\[Alpha]))*)
(*\[Kappa]swFun[\[Alpha]eff_,\[Kappa]col_]:=((1-\[Kappa]col)\[Alpha]eff)/(0.73+0.083 Sqrt[\[Alpha]eff]+\[Alpha]eff)*) (* 1903.09642 (3.1): for very relativistic walls *)
KswFun[\[Alpha]_,\[Kappa]_,\[Delta]_:0]:=0.6 \[Kappa] \[Alpha]/(1+\[Alpha]+\[Delta]) (* fractional kinetic energy density for soundwaves and turbulence. \[Delta]=0 in the bag E.O.S. *)


(* ::Subsection:: *)
(*GW Amplitude*)


(* ::Text:: *)
(*BPL and DBPL templates from 2403.03723*)


(* amplitude parameters *)
Acol=0.05; (* strong PT*)
Asw=0.11; (* soundwave *)
A=0.085; (* turbulence: Amhd=3\[Times]2.2\[Times]A/(4\[Pi]^2)\[Times]2^(-11/(3Subscript[a, 2]))\[TildeTilde]4.37\[Times]10-3 *)
(* energy density red-shift factor
	 f = H*0 f*/H* 
	 h^2\[CapitalOmega]=h^2 Fgw0 \[CapitalOmega]*
*)
h2Fgw0[gstar_]:=1.64 10^-5 (100/gstar)^(1/3) 
Hstar0[T_,gstar_,unit_]:=1.65 10^-5 (gstar/100)^(1/6) (Quantity[T,unit]/Quantity[100.,"GeV"]//UnitConvert)
(* spectral shape parameters *)
\[CapitalOmega]pars=Dataset@<|
	"Collisions"-><|"n1"->2.4,"n2"->-2.4,"n3"->Null,"a1"->1.2,"a2"->Null|>,
	"Soundwaves"-><|"n1"->3,"n2"->1,"n3"->-3,"a1"->2,"a2"->4|>,
	"Turbulence"-><|"n1"->3,"n2"->1,"n3"->-8/3,"a1"->4,"a2"->2.15|>
	|>;


(* ::Text:: *)
(*Broken power law (BPL)*)


fp[T_,\[Beta]H_,gstar_,unit_]:=0.11 Hstar0[T,gstar,unit] \[Beta]H (* eq. 2.7 *)
h2\[CapitalOmega]p[K_,\[Beta]H_,gstar_]:=h2Fgw0[gstar] Acol K^2 \[Beta]H^-2 (* eq. 2.7 *)
ShapeFun[f_,fp_,{n1_,n2_,a1_}]:=Module[{\[CapitalDelta]n=n1-n2,ffp=f/fp},
	\[CapitalDelta]n^(\[CapitalDelta]n/a1)/(-n2 ffp^(-n1 a1/\[CapitalDelta]n) + n1 ffp^(-n2 a1/\[CapitalDelta]n))^(\[CapitalDelta]n/a1)]

Options[h2\[CapitalOmega]col]=\[CapitalOmega]pars["Collisions"]//Normal//Normal;
h2\[CapitalOmega]col[f_,fp_,K_,\[Beta]H_,gstar_,unit_,OptionsPattern[]]:=Module[{S,n1,n2,n3,a1,a2},
	{n1,n2,a1}=OptionValue[{"n1","n2","a1"}];
	S[\[ScriptF]_]:=ShapeFun[\[ScriptF],fp,{n1,n2,a1}];
	h2\[CapitalOmega]p[K,\[Beta]H,gstar]S[f]
	]


(* ::Text:: *)
(*Double broken power law (DBPL)*)


(* soundwaves *)
(* dimensionless sound shell thickness *)
\[Xi]shell[vw_,cs_:cs]:=Abs[vw-cs] (* \[Xi]shell = \[Xi]front - \[Xi]rear, approximated for detonations/subsonic deflagrations *)
\[CapitalDelta]w[vw_,cs_:cs]:=\[Xi]shell[vw,cs]/Max[vw,cs]
(* mean adiabatic index *)
\[CapitalGamma]ad=4/3; (* for a radiation fluid *)
(* average bulk fluid velocity (subrelativistic limit) *)
vfsq[K_,\[CapitalGamma]ad_:\[CapitalGamma]ad]:=K/\[CapitalGamma]ad
(* decay time into turbulence *)
H\[Tau][HR_,vfsq_]:=Min[HR/Sqrt[vfsq],1]
(* frequency breaks *)
f1["Soundwaves",T_,HR_,gstar_,unit_]:=0.2 Hstar0[T,gstar,unit]/HR
f2["Soundwaves", T_,HR_,gstar_,unit_,vw_,cs_:cs]:=0.5 Hstar0[T,gstar,unit]/HR/\[CapitalDelta]w[vw,cs]
h2\[CapitalOmega]int[K_,HR_,gstar_]:= h2Fgw0[gstar] Asw K^2 H\[Tau][HR,vfsq[K,\[CapitalGamma]ad]] HR
h2\[CapitalOmega]2["Soundwaves",{f1_,f2_},K_,HR_,gstar_]:=1/\[Pi](Sqrt[2]+2 (f2/f1)/(1+f2^2/f1^2))h2\[CapitalOmega]int[K,HR, gstar]
ShapeFun[f_,{f1_,f2_},{n1_,n2_,n3_,a1_,a2_}]:=(f/f1)^n1 (1+(f/f1)^a1)^((-n1+n2)/a1) (1+(f/f2)^a2)^((-n2+n3)/a2)

Options[h2\[CapitalOmega]sw]=\[CapitalOmega]pars["Soundwaves"]//Normal//Normal;
h2\[CapitalOmega]sw[f_,{f1_,f2_},Treh_,K_,HR_,gstar_,vw_,cs_:cs,OptionsPattern[]]:=Module[{\[ScriptF]1,\[ScriptF]2,S,n1,n2,n3,a1,a2},
	{n1,n2,n3,a1,a2}=OptionValue[{"n1","n2","n3","a1","a2"}];
	(*\[ScriptF]1=f1["Soundwaves",Treh,HR,gstar,unit];
	\[ScriptF]2=f2["Soundwaves",Treh,HR,gstar,unit,vw,cs];*)
	S[\[ScriptF]_]:=ShapeFun[\[ScriptF],{f1,f2},{n1,n2,n3,a1,a2}];
	h2\[CapitalOmega]2["Soundwaves",{f1,f2},K,HR,gstar] S[f]/S[f2]
	]


(* MHD turbulence *)
Amhd[a2_]:=3 2.2 A/(4\[Pi]^2) 2^(-11/(3a2)) (* amplitude parameter (depends on spectral shape) *)
\[ScriptCapitalN]=2; (* number of eddy turnover times  *)
\[CurlyEpsilon]=0.5; (* mhd efficiency parameter (free) *)
\[CapitalOmega]s[\[CurlyEpsilon]_,K_]:=\[CurlyEpsilon] K
f1["Turbulence",T_,HR_,\[CapitalOmega]s_,gstar_,unit_,\[ScriptCapitalN]_:\[ScriptCapitalN]]:=Sqrt[3\[CapitalOmega]s]/(2\[ScriptCapitalN]) Hstar0[T,gstar,unit]/HR
f2["Turbulence", T_,HR_,gstar_,unit_]:=2.2 Hstar0[T,gstar,unit]/HR
h2\[CapitalOmega]2["Turbulence",Amhd_,\[CapitalOmega]s_,HR_,gstar_]:=h2Fgw0[gstar] Amhd \[CapitalOmega]s^2 HR^2

Options[h2\[CapitalOmega]mhd]=\[CapitalOmega]pars["Turbulence"]//Normal//Normal;
h2\[CapitalOmega]mhd[f_,{f1_,f2_},Treh_,\[ScriptCapitalA]mhd_,\[CapitalOmega]\[ScriptS]_,HR_,gstar_,\[CurlyEpsilon]_:0.5,\[ScriptCapitalN]_:\[ScriptCapitalN],OptionsPattern[]]:=Module[{\[ScriptF]1,\[ScriptF]2,S,n1,n2,n3,a1,a2},
	{n1,n2,n3,a1,a2}=OptionValue[{"n1","n2","n3","a1","a2"}];
	(*\[ScriptF]1=f1["Turbulence",Treh,HR,\[CapitalOmega]\[ScriptS],\[ScriptCapitalN]];
	\[ScriptF]2=f2["Turbulence",Treh,HR];*)
	S[\[ScriptF]_]:=ShapeFun[\[ScriptF],{f1,f2},{n1,n2,n3,a1,a2}];
	h2\[CapitalOmega]2["Turbulence",\[ScriptCapitalA]mhd,\[CapitalOmega]\[ScriptS],HR,gstar] S[f]/S[f2]
	]


(* ::Subsection:: *)
(*h^2 \[CapitalOmega]*)


h2Omega::usage="h2Omega[f,T,\[Alpha],\"\[Beta]/H\",\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\),\!\(\*SubscriptBox[\(g\), \(*\)]\),unit] gives the gravitational waves spectral amplitude at frequency f, for given phase transition parameters."
(*Options[h2Omega]={"Sources"->"Soundwaves","\[Kappa]col"->0.,"SoundSpeed"->cs,"\[CurlyEpsilon]"->\[CurlyEpsilon],"\[ScriptCapitalN]"->\[ScriptCapitalN]}
h2Omega[f_,T_,\[Alpha]_,\[Beta]H_,vw_,gstar_,unit_,opt:OptionsPattern[]]:=OptionValue["Sources"]/.{
		"Collisions"->h2\[CapitalOmega]col[f,T,\[Alpha],\[Beta]H,gstar,unit],
		"Soundwaves"->h2\[CapitalOmega]sw[f,T,\[Alpha],\[Beta]H,gstar,unit,vw,OptionValue["SoundSpeed"]],
		"Turbulence"->h2\[CapitalOmega]mhd[f,T,\[Alpha],\[Beta]H,gstar,unit,vw,OptionValue["SoundSpeed"],OptionValue["\[CurlyEpsilon]"],OptionValue["\[ScriptCapitalN]"]]
		}*)
(*Options[h2Omega]={"Sources"->"Soundwaves","\[Kappa]col"->0.,"SoundSpeed"->cs,"\[CurlyEpsilon]"->\[CurlyEpsilon],"\[ScriptCapitalN]"->\[ScriptCapitalN],"\[Alpha]eff"->None}*)
h2Omega[f\[ScriptP]_,{f1sw_,f2sw_},{f1mhd_,f2mhd_},Kcol_,\[Beta]H_,Treh_,Ksw_,HR_,\[ScriptCapitalA]mhd_,\[CapitalOmega]\[ScriptS]_,HR_,vw_,cs_,\[CurlyEpsilon]_,\[ScriptCapitalN]_,gstar_,unit_,opt:OptionsPattern[]]:=
	<|
		"Collisions"->If[NumericQ[Kcol],Evaluate@h2\[CapitalOmega]col[#,f\[ScriptP],Kcol,\[Beta]H,gstar,unit]&,Indeterminate],
		"Soundwaves"->(Evaluate@h2\[CapitalOmega]sw[#,{f1sw,f2sw},Treh,Ksw,HR,gstar,vw,cs]&),
		"Turbulence"->(Evaluate@h2\[CapitalOmega]mhd[#,{f1mhd,f2mhd},Treh,\[ScriptCapitalA]mhd,\[CapitalOmega]\[ScriptS],HR,gstar,\[CurlyEpsilon],\[ScriptCapitalN]]&),
		"Combined"->(Evaluate[
			If[NumericQ[Kcol],h2\[CapitalOmega]col[#,f\[ScriptP],Kcol,\[Beta]H,gstar,unit],0]+
			h2\[CapitalOmega]sw[#,{f1sw,f2sw},Treh,Ksw,HR,gstar,vw,cs]+
			h2\[CapitalOmega]mhd[#,{f1mhd,f2mhd},Treh,\[ScriptCapitalA]mhd,\[CapitalOmega]\[ScriptS],HR,gstar,\[CurlyEpsilon],\[ScriptCapitalN]]//Simplify
			]&)
		|>


(* ::Subsection:: *)
(*Compute SGWB*)


ComputeSGWB::usage="ComputeSGWB[T,\[Alpha],\"\[Beta]/H\",\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\),\!\(\*SubscriptBox[\(g\), \(*\)]\),Unit] computes the stochastic gravitational wave background for given phase transition parameters.
ComputeSGWB[T,\[Alpha],\"\[Beta]/H\",\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\),\!\(\*SubscriptBox[\(g\), \(*\)]\),Unit,\"CollisionData\"\[Rule]Association] computes the contribution from bubble wall collisions.
ComputeSGWB[Association] extracts the relevant parameters from Association.";
Options[ComputeSGWB]={
	"CollisionData"->None,
	"Sources"->{"Collisions","Soundwaves","Turbulence","Combined"},
	"SoundSpeed"->cs,"\[CurlyEpsilon]"->\[CurlyEpsilon],"\[ScriptCapitalN]"->\[ScriptCapitalN],"ShapeParameters"->\[CapitalOmega]pars
	};
AutoComplete[ComputeSGWB];
ComputeSGWB[T_,\[Alpha]_,\[Beta]H_,vw_,gstar_,unit_,OptionsPattern[]]:=Module[{
	\[CapitalOmega]pars,
	Kcol,\[Kappa]col,\[Kappa]Data,colMetaData,
	Ksw,\[Kappa]sw,
	\[Alpha]eff,Treh,
	f\[ScriptP],f1sw,f2sw,f1mhd,f2mhd,
	HR,\[CapitalOmega]\[ScriptS],\[ScriptCapitalA]mhd,\[CurlyEpsilon],\[ScriptCapitalN],
	h2\[CapitalOmega],
	sources=OptionValue["Sources"],
	cs=OptionValue["SoundSpeed"],
	colData=OptionValue["CollisionData"]
	},
	(*GWData["ShapeParameters"]=*)\[CapitalOmega]pars=OptionValue["ShapeParameters"];
	{\[Kappa]col,colMetaData}=\[Kappa]ColFun[colData,Return->{"\[Kappa]col","Data"}];
	If[MatchQ[colMetaData,_Association],AssociateTo[GWData,colMetaData]];
	GWData["\[Kappa]col"]=\[Kappa]col;
	GWData["Kcol"]=Kcol=KColFun[\[Alpha],\[Kappa]col];
	GWData["\[Alpha]eff"]=\[Alpha]eff=\[Alpha]EffFun[\[Alpha],\[Kappa]col];
	GWData["\[Kappa]sw"]=\[Kappa]sw=\[Kappa]swFun[\[Alpha]eff,vw];
	GWData["Ksw"]=Ksw=KswFun[\[Alpha]eff,\[Kappa]sw];
	GWData["fp"]=f\[ScriptP]=fp[T,\[Beta]H,gstar,unit];
	GWData["f1sw"]=f1sw=f1["Soundwaves",Treh,HR,gstar,unit];
	GWData["f2sw"]=f2sw=f2["Soundwaves",Treh,HR,gstar,unit,vw,cs];
	GWData["f1mhd"]=f1mhd=f1["Turbulence",Treh,HR,\[CapitalOmega]\[ScriptS],gstar,unit,\[ScriptCapitalN]];
	GWData["f2mhd"]=f2mhd=f2["Turbulence",Treh,HR,gstar,unit];
	GWData["HR"]=HR=HRFrom\[Beta]H[\[Beta]H,vw];
	GWData["\[CapitalOmega]s"]=\[CapitalOmega]\[ScriptS]=\[CapitalOmega]s[\[CurlyEpsilon],Ksw];
	GWData["Amhd"]=\[ScriptCapitalA]mhd=Amhd[\[CapitalOmega]pars["Turbulence","a2"]];
	GWData["\[CurlyEpsilon]"]=\[CurlyEpsilon]=OptionValue["\[CurlyEpsilon]"];
	GWData["\[ScriptCapitalN]"]=\[ScriptCapitalN]=OptionValue["\[ScriptCapitalN]"];
	GWData["Treh"]=Treh=(1+\[Alpha](*or \[Alpha]eff?*))^(1/4) T;
	h2\[CapitalOmega]=h2Omega[f\[ScriptP],{f1sw,f2sw},{f1mhd,f2mhd},Kcol,\[Beta]H,Treh,Ksw,HR,\[ScriptCapitalA]mhd,\[CapitalOmega]\[ScriptS],HR,vw,cs,\[CurlyEpsilon],\[ScriptCapitalN],gstar,unit];
	(* SWGB templates *)
	GWData["h2Omega"]=h2\[CapitalOmega]//KeyTake[sources]//Select[Head[#]==Function&]; (* filter by "Sources" and proper spectra *)
	(* AutoComplete Keys *)
	AutoComplete[GWData];
	GWData
	]
ComputeSGWB[assoc_Association,opt:OptionsPattern[]]:=Module[{
	Tp,\[Alpha],\[Beta]H,vw,gstar,unit,GWRequirements,colRequirements,missing,
	colData=OptionValue["CollisionData"]
	},
	(* extract SGWB parameters *)
	GWRequirements={"Tp","\[Alpha]","\[Beta]/H","vw","gstar","Unit"};
	If[(missing=Complement[GWRequirements,Keys@assoc])!={},
		Print@StringTemplate["Missing `` in input Association.\nSGWB cannot be computed!"][missing];
		Abort[]
		];
	{Tp,\[Alpha],\[Beta]H,vw,gstar,unit}=GWRequirements/.assoc;
	(* optional collision data *)
	If[MatchQ[colData,_Association|_List],
		colData=Association[colData];
		colRequirements={"ActionFunction","Phases","vw","\[Alpha]","\[Beta]/H"};
		If[(missing=Complement[colRequirements,Keys@assoc])=!={},
			Print@StringTemplate["Missing `` in input Association.\n\!\(\*SubscriptBox[\(\[Kappa]\), \(col\)]\) set to 0."][missing];
			colData=None,
			AssociateTo[colData,KeyTake[assoc,colRequirements]]
			];
		colData=colData/.assoc  (* allow delayed evaluation of collision data. E.g. colData=<|"T"->"Tn",..|> \[Rule] <|"T"->value,..|> *)
		];
	ComputeSGWB[Tp,\[Alpha],\[Beta]H,vw,gstar,unit,"CollisionData"->colData,opt]
	]
ComputeSGWB[tr_Transition,opt:OptionsPattern[]]:=ComputeSGWB[tr[Association],opt]


(* ::Subsection::Closed:: *)
(*Sensitivities*)


(* ::Text:: *)
(*Peak-integrated sensitivity curves from 2002.04615*)


PISC::usage="PISC[f,\"detector\"] gives the peak-integrated sensitivity curve for \"detector\", at the frequency f.";
PISC[f_,"LISA"]:=Module[{\[ScriptF],polyCoeffs,powers},
	\[ScriptF]=10^3 f;
	polyCoeffs={3.58 10^-3, 3.26 10^-1, 1.20, 2.48, 2.85 10^-1, 1.81 10^-2, 1.50 10^-3};
	powers={-4,-3,-2,-1,1,2,3};
	10.^-14 polyCoeffs . \[ScriptF]^powers
	]	
PISC[f_,"DECIGO"]:=Module[{\[ScriptF],polyCoeffs,powers},
	\[ScriptF]=10^3 f;
	polyCoeffs={3.82 10^-1, 2.26, 1.10 10^-3, 2.56 10^-6, 2.91 10^-8, 7.54 10^-12};
	powers={-4,-1.5,0,1,2,3};
	10.^-14 polyCoeffs . \[ScriptF]^powers
	]
PISC[f_,"BBO"]:=Module[{\[ScriptF],polyCoeffs,powers},
	\[ScriptF]=10^3 f;
	polyCoeffs={1.77 10^-1, 1.06, 1.35 10^-4, 2.23 10^-6, 1.29 10^-9, 2.99 10^-12};
	powers={-4,-1.5,0,1,2,3};
	10.^-14 polyCoeffs . \[ScriptF]^powers
	]


(* ::Section:: *)
(*End Package*)


End[]; (* end "Private`" *)


(* ReadProtected attribute on public symbols prevents rendering of huge box with all 
definitions (DownValues) when they are called in Information or with shortcut ?FindBounce. *)
SetAttributes[Evaluate@Names["`*"],{ReadProtected}]; (* from FindBounce.m *) 


EndPackage[];
