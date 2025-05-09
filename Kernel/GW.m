(* ::Package:: *)

(* ::Section:: *)
(*Begin Package*)


BeginPackage["GW`"]


(* ::Subsection::Closed:: *)
(*Available public Symbols*)


(* high-level Symbols *)
GWData;
(*h2Omega; (* GW amplitude *)
h2\[CapitalOmega]col;
h2\[CapitalOmega]sw;
h2\[CapitalOmega]mhd;*)
ComputeGW;
PISC; (* peak-integrated sensitivity curves *)
PLISC; (* power-law integrated sensitivity curves *)
GWSensitivities; (* generic sensitivity function *)


(* phase transition parameters *)
(*{\[CapitalDelta]m2Fun,g2\[CapitalDelta]mvFun,\[Alpha]\[Infinity]Fun,\[Alpha]eqFun,\[Gamma]eqFun,\[Gamma]starFun,E0VFun,R0Fun,RFun};*) (* for testing purposes only *)
kappaCollision;
KCollision;
kappaSoundwaves;
KSoundwaves;
HRFromBetaH;
BetaHFromHR;
HRFun;
nBFun;


(* constants & units *)
$FrequencyUnit;


(* public PT2GW` symbols *)
H;
DecayRate;
BetaHubble;
RadiationEnergyDensity;
IntegralFalseVacuum;
$PT2GWPrint;


(* ::Section:: *)
(*Code*)


Begin["Private`"]


(* ::Subsection::Closed:: *)
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
(*Initialize GWData*)


If[!MatchQ[GWData,_Association],GWData=<||>];


(* ::Subsubsection::Closed:: *)
(*Messages*)


PT2GWPrint:=If[$PT2GWPrint,Print];
PT2GWEcho:=If[$PT2GWPrint===True,Echo,#1&];


msg["missingGW"]=StringTemplate["Missing `` in input Association.\nSGWB cannot be computed!"];
msg["missing\[Kappa]col"]=StringTemplate["Missing `` in input Association.\n\!\(\*SubscriptBox[\(\[Kappa]\), \(col\)]\) is Indeterminate."];
msg["no\[Kappa]col"]="No GWs from bubble collisions: \!\(\*SubscriptBox[\(\[Kappa]\), \(col\)]\) is Indeterminate. \n    Collisions' efficiency must be provided or computed via mass functions.";


msg["noDetector"]=StringTemplate["Missing \!\(\*StyleBox[\"`1`\",StripOnInput->False,LineColor->RGBColor[1, 0, 0],FrontFaceColor->RGBColor[1, 0, 0],BackFaceColor->RGBColor[1, 0, 0],GraphicsColor->RGBColor[1, 0, 0],FontColor->RGBColor[1, 0, 0]]\). Available detectors are\n`2`"];
msg["noDetectorConf"]=StringTemplate["Missing \!\(\*StyleBox[\"`1`\",StripOnInput->False,LineColor->RGBColor[1, 0, 0],FrontFaceColor->RGBColor[1, 0, 0],BackFaceColor->RGBColor[1, 0, 0],GraphicsColor->RGBColor[1, 0, 0],FontColor->RGBColor[1, 0, 0]]\). Available detector configurations are\n`2`"];


(* ::Subsection::Closed:: *)
(*Constants*)


$FrequencyUnit="Hz";


$SpeedOfSound=1/Sqrt[3]; (* radiation sound speed *)


(* ::Subsection::Closed:: *)
(*Phase transition parameters*)


(* ::Subsubsection::Closed:: *)
(*Bubble radius & number density*)


(* bubble number density *)
nBFun::usage="nBFun[ActionFunction,T,\!\(\*SubscriptBox[\(T\), \(c\)]\),\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\)] estimates the density of bubbles at a given temperature.
nBFun[ActionFunction,T,\!\(\*SubscriptBox[\(T\), \(c\)]\),\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\),V,{\!\(\*SubscriptBox[\(\[Phi]\), \(1\)]\),\!\(\*SubscriptBox[\(\[Phi]\), \(2\)]\)}] includes the vacuum energy contribution to the Hubble parameter.";
nBFun[ST_Function|ST_Symbol,T_?NumericQ,Tc_,vw_,opt:OptionsPattern[]]:=T^3 NIntegrate[DecayRate[\[ScriptCapitalT],ST,opt]E^-IntegralFalseVacuum[ST,\[ScriptCapitalT],Tc,vw]/(\[ScriptCapitalT]^4 H[T]),{\[ScriptCapitalT],T,Tc}]
nBFun[ST_Function|ST_Symbol,T_?NumericQ,Tc_,vw_,V_,phases_,opt:OptionsPattern[]]:=T^3 NIntegrate[DecayRate[\[ScriptCapitalT],ST,opt]E^-IntegralFalseVacuum[ST,\[ScriptCapitalT],Tc,vw]/(\[ScriptCapitalT]^4 H[\[ScriptCapitalT],V,phases]),{\[ScriptCapitalT],T,Tc}]
(*Rstar[ST_Function|ST_Symbol,T_?NumericQ,Tc_,vw_,opt:OptionsPattern[]]:=nBFun[ST,T,Tc,vw,opt]^(-1/3)*)
HRFun::usage="HRFun[actionFunction,T,\!\(\*SubscriptBox[\(T\), \(c\)]\),\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\)] computes \!\(\*SubscriptBox[\(H\), \(*\)]\)\!\(\*SubscriptBox[\(R\), \(*\)]\), the average bubble radius times the Hubble parameter at a given temperature.
HRFun[actionFunction,T,\!\(\*SubscriptBox[\(T\), \(c\)]\),\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\),V,phases] includes the vacuum energy contribution to the Hubble parameter.";
HRFun[ST_Function|ST_Symbol,T_?NumericQ,Tc_,vw_,opt:OptionsPattern[]]:=H[T]nBFun[ST,T,Tc,vw,opt]^(-1/3)
HRFun[ST_Function|ST_Symbol,T_?NumericQ,Tc_,vw_,V_,phases_,opt:OptionsPattern[]]:=H[T,V,phases]nBFun[ST,T,Tc,vw,V,phases,opt]^(-1/3)
HRFromBetaH::usage="HRFromBetaH[\"\[Beta]/H\",\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\),\!\(\*SubscriptBox[\(c\), \(s\)]\)] estimates HR from the inverse duration \[Beta]/H.";
HRFromBetaH[\[Beta]H_?NumericQ,vw_,cs_:$SpeedOfSound]:=(8\[Pi])^(1/3)Max[vw,cs]/\[Beta]H


(* ::Subsubsection::Closed:: *)
(*\[Beta]/H*)


BetaHFromHR::usage="BetaHFromHR[HR,\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\),\!\(\*SubscriptBox[\(c\), \(s\)]\)] estimates the inverse duration (in Hubble units) of a transition from H\[CenterDot]R.
BetaHFromHR[ActionFunction,T,\!\(\*SubscriptBox[\(T\), \(c\)]\),\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\),\!\(\*SubscriptBox[\(c\), \(s\)]\)] computes internally H\[CenterDot]R.
";
BetaHFromHR[HR_?NumericQ,vw_?NumericQ,cs_:$SpeedOfSound]:=(8\[Pi])^(1/3) Max[vw,cs]/HR
BetaHFromHR[ST_Function|ST_Symbol,T_?NumericQ,Tc_,vw_,cs_:$SpeedOfSound,opt:OptionsPattern[]]:=(8\[Pi])^(1/3) Max[vw,cs]/HRFun[ST,T,Tc,vw,opt]


(* ::Subsubsection::Closed:: *)
(*Strengths \[Alpha]*)


(* ::Text:: *)
(*Mass functions*)


(* mass functions *)
\[CapitalDelta]m2Fun[\[CapitalDelta]mf_,\[CapitalDelta]mv_,nf_:1,nv_:1]:=Total[1/2 (nf \[CapitalDelta]mf^2)]+Total[nv \[CapitalDelta]mv^2]
g2\[CapitalDelta]mvFun[g_,\[CapitalDelta]mv_,nv_:1]:=g^2 nv \[CapitalDelta]mv//Total


\[Alpha]\[Infinity]Fun[\[CapitalDelta]m2_,T_]:=1/24 \[CapitalDelta]m2 T^2/RadiationEnergyDensity[T]
(*\[Alpha]\[Infinity]Fun[T_,\[Phi]_]:=4.9 10^-3 (\[Phi]/T)^2*) (* minimum run-away alpha for SM-like theory *)
\[Alpha]eqFun[g2\[CapitalDelta]mv_,T_]:=g2\[CapitalDelta]mv T^3/RadiationEnergyDensity[T]


(* attempt to compute R0 from action potential contribution and fast expansion, see 1903.09642 *)
E0VFun[Action_,T_]:= 2 Action[T] (* E_0_V  *)
R0Fun[E0V_,\[CapitalDelta]V_]:=(3 E0V/(4\[Pi] \[CapitalDelta]V))^(1/3)
(*Req=1/Sqrt[1-(R'[t])^2]==(2R[t])/(3 Rc)+Rc^2/(3 R[t]^2);
Rsol[Rc_,\[Delta]_:10^-5]:=NDSolve[{Req,R[0]==(1+\[Delta])Rc},R,{t,0,100}][[2]]
RstarFun[T_]:=With[{sol=Rsol[Rc,\[Delta]]},(R/.sol)[T]]*)
RFun[T_,action_,V_,phases_,\[Xi]w_,cs_:$SpeedOfSound]:=(8\[Pi])^(1/3)Max[\[Xi]w,cs]/BetaHubble[T,action]/H[T,V,phases]


\[Gamma]eqFun[\[Alpha]_,\[Alpha]\[Infinity]_,\[Alpha]eq_]:=(\[Alpha]-\[Alpha]\[Infinity])/\[Alpha]eq
(*\[Gamma]starFun[action_,\[Beta]overH_,T_,T0_,V_,phases_,\[Xi]w_]:=2/3 \[Xi]w (8\[Pi])^(1/3)/(H[T,V,phases]\[Beta]overH)/((3 action[T0])/(2\[Pi] Abs[V[phases[[1]][T0],T0]-V[phases[[2]][T0],T0]]))^(1/3)*)
\[Gamma]starFun[R0_,Rstar_]:=2/3 Rstar/R0
(* T0 is the temperature that gives the initial radius R0. Here we'll take it to be T nucleation, which should be okay if no supercooling *)


(* ::Subsubsection::Closed:: *)
(*K, \[Kappa] - efficiency*)


(* ::Text:: *)
(*Efficiency factor for bubble wall collisions, from *)


(* efficiency & relative coefficients *)
(*kappaCollision[\[Alpha]_,\[Alpha]\[Infinity]_]:=1-\[Alpha]\[Infinity]/\[Alpha]*)
kappaCollision::usage="kappaCollision[\[Gamma]*,\!\(\*SubscriptBox[\(\[Gamma]\), \(eq\)]\),\[Alpha],\!\(\*SubscriptBox[\(\[Alpha]\), \(\[Infinity]\)]\)] gives the efficiency coefficient for bubble wall collisions \!\(\*SubscriptBox[\(\[Kappa]\), \(col\)]\).
kappaCollision[assoc] computes \!\(\*SubscriptBox[\(\[Kappa]\), \(col\)]\) by extracting the relevant quantities from assoc. Must include \"MassFunctions\",\"GaugeCouplings\",\"T0\",\"T\",\"Potential\",\"Phases\",\"vw\",\"ActionFunction\",\"\[Alpha]\",\"\[Beta]/H\"
kappaCollision[None] returns Indeterminate (for compatibility).
";
kappaCollision[\[Gamma]star_,\[Gamma]eq_,\[Alpha]_,\[Alpha]\[Infinity]_]:=\!\(\*
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
Options[kappaCollision]={Return->"\[Kappa]col"};
kappaCollision[colData:(_Association|_List),OptionsPattern[]]:=Module[{
	data=<||>,\[Kappa]ColRequirements,missing,
	massFun,T0,T,V,\[CapitalDelta]V,phases,vw,action,\[Alpha],\[Beta]H,
	\[CapitalDelta]mf,\[CapitalDelta]mv,nf,nv,gaugeCoup,\[CapitalDelta]m2,g2\[CapitalDelta]mv,E0V,R0,Rstar,\[Alpha]\[Infinity],\[Alpha]eq,\[Gamma]eq,\[Gamma]star,
	\[Kappa]Col,return=OptionValue[Return]
	},
	(* \[Kappa]col provided *)
	If[Lookup[colData,"\[Kappa]col",False,NumericQ[\[Kappa]Col=#]&],
		Return[return/.{"\[Kappa]col"->\[Kappa]Col,"Data"->None}]
		];
	(* \[Kappa]col computed from data *)
	\[Kappa]ColRequirements={"MassFunctions","GaugeCouplings","T0","T","Potential","Phases","WallVelocity","ActionFunction","\[Alpha]","\[Beta]/H"};
	AssociateTo[data,KeyTake[colData,\[Kappa]ColRequirements]];
	If[(missing=Complement[\[Kappa]ColRequirements,Keys[data]])!={},
		PT2GWPrint@StringTemplate["Missing `` in CollisionData.\n\!\(\*SubscriptBox[\(\[Kappa]\), \(col\)]\) is Indeterminate."][missing];
		Return[return/.{"\[Kappa]col"->Indeterminate,"Data"->None}]
		];
	Check[
		{V,phases,vw,action,\[Alpha],\[Beta]H}={"Potential","Phases","WallVelocity","ActionFunction","\[Alpha]","\[Beta]/H"}/.data;
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
		E0V=E0VFun[action,T];
		\[CapitalDelta]V=Abs[Subtract@@(V[#[T0],T0]&/@phases)];
		R0=R0Fun[E0V,\[CapitalDelta]V];
		Rstar=RFun[T,action,V,phases,vw];
		\[Gamma]star=\[Gamma]starFun[R0,Rstar];
		\[Kappa]Col=kappaCollision[\[Gamma]star,\[Gamma]eq,\[Alpha],\[Alpha]\[Infinity]],
		(* handle failed computation *)
		PT2GWPrint["Computation of collision efficiency failed. \!\(\*SubscriptBox[\(\[Kappa]\), \(col\)]\) is Indeterminate."];
		\[Kappa]Col=Indeterminates
		];
	OptionValue[Return]/.{"\[Kappa]col"->\[Kappa]Col,"Data"->AssociationThread[
		{"\[CapitalDelta]mf","\[CapitalDelta]mv","nF","nV","gaugeCoup","\[CapitalDelta]m2","g2\[CapitalDelta]mv","R0","R*","\[Alpha]\[Infinity]","\[Alpha]eq","\[Gamma]eq","\[Gamma]*"}->
		{\[CapitalDelta]mf,\[CapitalDelta]mv,nf,nv,gaugeCoup,\[CapitalDelta]m2,g2\[CapitalDelta]mv,R0,Rstar,\[Alpha]\[Infinity],\[Alpha]eq,\[Gamma]eq,\[Gamma]star}]}
	]
(* if no data is provided *)
kappaCollision[_?(Not@*NumericQ),OptionsPattern[]]:=Module[{},
	PT2GWPrint[msg["no\[Kappa]col"]];
	OptionValue[Return]/.{"\[Kappa]col"->Indeterminate,"Data"->None}
	]
KCollision::usage="kappaCollision[\[Alpha],\[Kappa]col] gives the fractional energy density for bubble wall collisions.";
KCollision[\[Alpha]_,\[Kappa]col_]:=\[Kappa]col \[Alpha]/(1+\[Alpha]) (* fractional kinetic energy density for strong (collision) PTs *)


(* ::Text:: *)
(*The efficiency coefficient \[Kappa]sw indicates the fraction of latent heat that is transformed into bulk motion of the plasma and finally into GWs.*)
(*NB Expression from eq. 37 of Espinosa et al. (JCAP.2010.06.028).*)


(* soundwaves *)
AlphaEffective[\[Alpha]_,\[Kappa]col_?NumericQ]:=(1-\[Kappa]col)\[Alpha]
AlphaEffective[\[Alpha]_,\[Kappa]col_]:=\[Alpha]    (* explicitly: AlphaEffective[\[Alpha]_,\[Kappa]col_?(Not@*NumericQ)]:=\[Alpha] *)
kappaSoundwaves::usage="kappaSoundwaves[\[Alpha],\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\),\!\(\*SubscriptBox[\(c\), \(s\)]\)] gives the efficiency coefficient for soundwaves.
kappaSoundwaves[\[Alpha],\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\)] assumes a speed of sound \!\(\*SubscriptBox[\(c\), \(s\)]\)=1/\!\(\*SqrtBox[\(3\)]\) for the false-vacuum phase.";
kappaSoundwaves[\[Alpha]_,vw_,cs_:$SpeedOfSound]:=Module[{vJ,\[Kappa]A,\[Kappa]B,\[Kappa]C,\[Kappa]D,\[Delta]\[Kappa]},
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
(*kappaSoundwaves[\[Alpha]_]:=\[Alpha]/(0.73+0.083Sqrt[\[Alpha]]+\[Alpha])*) (* 2403.03723v1, for vw\[TildeTilde]1 *)
(*kappaSoundwaves[\[Alpha]_]:=(1/(1+0.715\[Alpha]))(0.715\[Alpha]+(4/27)\[Sqrt]((3/2)\[Alpha]))*)
(*kappaSoundwaves[\[Alpha]eff_,\[Kappa]col_]:=((1-\[Kappa]col)\[Alpha]eff)/(0.73+0.083 Sqrt[\[Alpha]eff]+\[Alpha]eff)*) (* 1903.09642 (3.1): for very relativistic walls *)
KSoundwaves::usage="KSoundwaves[\[Alpha],\!\(\*SubscriptBox[\(\[Kappa]\), \(sw\)]\),\[Delta]] gives the fractional energy density available to soundwaves.
KSoundwaves[\[Alpha],\!\(\*SubscriptBox[\(\[Kappa]\), \(sw\)]\)] assumes \[Delta]=0, proper to the bag E.o.S.";
KSoundwaves[\[Alpha]_,\[Kappa]_,\[Delta]_:0]:=0.6 \[Kappa] \[Alpha]/(1+\[Alpha]+\[Delta]) (* fractional kinetic energy density for soundwaves and turbulence. \[Delta]=0 in the bag E.O.S. *)


(* ::Subsection::Closed:: *)
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
h2Fgw0[relDOF_]:=1.64 10^-5 (100/relDOF)^(1/3) 
Hstar0[T_,relDOF_,unit_]:=1.65 10^-5 (relDOF/100)^(1/6) (Quantity[T,unit]/Quantity[100.,"GeV"]//UnitConvert)
(* spectral shape parameters *)
\[CapitalOmega]pars=Dataset@<|
	"Collisions"-><|"n1"->2.4,"n2"->-2.4,"n3"->Null,"a1"->1.2,"a2"->Null|>,
	"Soundwaves"-><|"n1"->3,"n2"->1,"n3"->-3,"a1"->2,"a2"->4|>,
	"Turbulence"-><|"n1"->3,"n2"->1,"n3"->-8/3,"a1"->4,"a2"->2.15|>
	|>;


(* ::Text:: *)
(*Broken power law (BPL)*)


fp[T_,\[Beta]H_,relDOF_,unit_]:=0.11 Hstar0[T,relDOF,unit] \[Beta]H (* eq. 2.7 *)
h2\[CapitalOmega]p[K_,\[Beta]H_,relDOF_]:=h2Fgw0[relDOF] Acol K^2 \[Beta]H^-2 (* eq. 2.7 *)
ShapeFun[f_,fp_,{n1_,n2_,a1_}]:=Module[{\[CapitalDelta]n=n1-n2,ffp=f/fp},
	\[CapitalDelta]n^(\[CapitalDelta]n/a1)/(-n2 ffp^(-n1 a1/\[CapitalDelta]n) + n1 ffp^(-n2 a1/\[CapitalDelta]n))^(\[CapitalDelta]n/a1)]

Options[h2\[CapitalOmega]col]=\[CapitalOmega]pars["Collisions"]//Normal//Normal;
h2\[CapitalOmega]col[f_,fp_,K_,\[Beta]H_,relDOF_,unit_,OptionsPattern[]]:=Module[{S,n1,n2,n3,a1,a2},
	{n1,n2,a1}=OptionValue[{"n1","n2","a1"}];
	S[\[ScriptF]_]:=ShapeFun[\[ScriptF],fp,{n1,n2,a1}];
	h2\[CapitalOmega]p[K,\[Beta]H,relDOF]S[f]
	]


(* ::Text:: *)
(*Double broken power law (DBPL)*)


(* soundwaves *)
(* dimensionless sound shell thickness *)
\[Xi]shell[vw_,cs_:$SpeedOfSound]:=Abs[vw-cs] (* \[Xi]shell = \[Xi]front - \[Xi]rear, approximated for detonations/subsonic deflagrations *)
\[CapitalDelta]w[vw_,cs_:$SpeedOfSound]:=\[Xi]shell[vw,cs]/Max[vw,cs]
(* mean adiabatic index *)
\[CapitalGamma]ad=4/3; (* for a radiation fluid *)
(* average bulk fluid velocity (subrelativistic limit) *)
vfsq[K_,\[CapitalGamma]ad_:\[CapitalGamma]ad]:=K/\[CapitalGamma]ad
(* decay time into turbulence *)
H\[Tau][HR_,vfsq_]:=Min[HR/Sqrt[vfsq],1]
(* frequency breaks *)
f1["Soundwaves",T_,HR_,relDOF_,unit_]:=0.2 Hstar0[T,relDOF,unit]/HR
f2["Soundwaves", T_,HR_,relDOF_,unit_,vw_,cs_:$SpeedOfSound]:=0.5 Hstar0[T,relDOF,unit]/HR/\[CapitalDelta]w[vw,cs]
h2\[CapitalOmega]int[K_,HR_,relDOF_]:= h2Fgw0[relDOF] Asw K^2 H\[Tau][HR,vfsq[K,\[CapitalGamma]ad]] HR
h2\[CapitalOmega]2["Soundwaves",{f1_,f2_},K_,HR_,relDOF_]:=1/\[Pi](Sqrt[2]+2 (f2/f1)/(1+f2^2/f1^2))h2\[CapitalOmega]int[K,HR, relDOF]
ShapeFun[f_,{f1_,f2_},{n1_,n2_,n3_,a1_,a2_}]:=(f/f1)^n1 (1+(f/f1)^a1)^((-n1+n2)/a1) (1+(f/f2)^a2)^((-n2+n3)/a2)

Options[h2\[CapitalOmega]sw]=\[CapitalOmega]pars["Soundwaves"]//Normal//Normal;
h2\[CapitalOmega]sw[f_,{f1_,f2_},Treh_,K_,HR_,relDOF_,vw_,cs_:$SpeedOfSound,OptionsPattern[]]:=Module[{\[ScriptF]1,\[ScriptF]2,S,n1,n2,n3,a1,a2},
	{n1,n2,n3,a1,a2}=OptionValue[{"n1","n2","n3","a1","a2"}];
	(*\[ScriptF]1=f1["Soundwaves",Treh,HR,relDOF,unit];
	\[ScriptF]2=f2["Soundwaves",Treh,HR,relDOF,unit,vw,cs];*)
	S[\[ScriptF]_]:=ShapeFun[\[ScriptF],{f1,f2},{n1,n2,n3,a1,a2}];
	h2\[CapitalOmega]2["Soundwaves",{f1,f2},K,HR,relDOF] S[f]/S[f2]
	]


(* MHD turbulence *)
Amhd[a2_]:=3 2.2 A/(4\[Pi]^2) 2^(-11/(3a2)) (* amplitude parameter (depends on spectral shape) *)
\[ScriptCapitalN]=2; (* number of eddy turnover times  *)
\[CurlyEpsilon]=0.5; (* mhd efficiency parameter (free) *)
\[CapitalOmega]s[\[CurlyEpsilon]_,K_]:=\[CurlyEpsilon] K
f1["Turbulence",T_,HR_,\[CapitalOmega]s_,relDOF_,unit_,\[ScriptCapitalN]_:\[ScriptCapitalN]]:=Sqrt[3\[CapitalOmega]s]/(2\[ScriptCapitalN]) Hstar0[T,relDOF,unit]/HR
f2["Turbulence", T_,HR_,relDOF_,unit_]:=2.2 Hstar0[T,relDOF,unit]/HR
h2\[CapitalOmega]2["Turbulence",Amhd_,\[CapitalOmega]s_,HR_,relDOF_]:=h2Fgw0[relDOF] Amhd \[CapitalOmega]s^2 HR^2

Options[h2\[CapitalOmega]mhd]=\[CapitalOmega]pars["Turbulence"]//Normal//Normal;
h2\[CapitalOmega]mhd[f_,{f1_,f2_},Treh_,\[ScriptCapitalA]mhd_,\[CapitalOmega]\[ScriptS]_,HR_,relDOF_,\[CurlyEpsilon]_:0.5,\[ScriptCapitalN]_:\[ScriptCapitalN],OptionsPattern[]]:=Module[{\[ScriptF]1,\[ScriptF]2,S,n1,n2,n3,a1,a2},
	{n1,n2,n3,a1,a2}=OptionValue[{"n1","n2","n3","a1","a2"}];
	(*\[ScriptF]1=f1["Turbulence",Treh,HR,\[CapitalOmega]\[ScriptS],\[ScriptCapitalN]];
	\[ScriptF]2=f2["Turbulence",Treh,HR];*)
	S[\[ScriptF]_]:=ShapeFun[\[ScriptF],{f1,f2},{n1,n2,n3,a1,a2}];
	h2\[CapitalOmega]2["Turbulence",\[ScriptCapitalA]mhd,\[CapitalOmega]\[ScriptS],HR,relDOF] S[f]/S[f2]
	]


(* ::Subsection::Closed:: *)
(*h^2 \[CapitalOmega]*)


h2Omega::usage="h2Omega[f,T,\[Alpha],\"\[Beta]/H\",\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\),g*,unit] gives the gravitational waves spectral amplitude at frequency f, for given phase transition parameters."
(*Options[h2Omega]={"Sources"->"Soundwaves","\[Kappa]col"->0.,"SpeedOfSound"->$SpeedOfSound,"\[CurlyEpsilon]"->\[CurlyEpsilon],"\[ScriptCapitalN]"->\[ScriptCapitalN],"\[Alpha]eff"->None}*)
h2Omega[f\[ScriptP]_,{f1sw_,f2sw_},{f1mhd_,f2mhd_},Kcol_,\[Beta]H_,Treh_,Ksw_,HR_,\[ScriptCapitalA]mhd_,\[CapitalOmega]\[ScriptS]_,HR_,vw_,cs_,\[CurlyEpsilon]_,\[ScriptCapitalN]_,relDOF_,unit_,opt:OptionsPattern[]]:=
	<|
		"Collisions"->If[NumericQ[Kcol],Evaluate@h2\[CapitalOmega]col[#,f\[ScriptP],Kcol,\[Beta]H,relDOF,unit]&,Indeterminate],
		"Soundwaves"->(Evaluate@h2\[CapitalOmega]sw[#,{f1sw,f2sw},Treh,Ksw,HR,relDOF,vw,cs]&),
		"Turbulence"->(Evaluate@h2\[CapitalOmega]mhd[#,{f1mhd,f2mhd},Treh,\[ScriptCapitalA]mhd,\[CapitalOmega]\[ScriptS],HR,relDOF,\[CurlyEpsilon],\[ScriptCapitalN]]&),
		"Combined"->(Evaluate[
			If[NumericQ[Kcol],h2\[CapitalOmega]col[#,f\[ScriptP],Kcol,\[Beta]H,relDOF,unit],0]+
			h2\[CapitalOmega]sw[#,{f1sw,f2sw},Treh,Ksw,HR,relDOF,vw,cs]+
			h2\[CapitalOmega]mhd[#,{f1mhd,f2mhd},Treh,\[ScriptCapitalA]mhd,\[CapitalOmega]\[ScriptS],HR,relDOF,\[CurlyEpsilon],\[ScriptCapitalN]]//Simplify
			]&)
		|>


(* ::Subsection::Closed:: *)
(*Compute SGWB*)


ComputeGW::usage="ComputeGW[T,\[Alpha],\"\[Beta]/H\",\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\),g*,$Unit] computes the stochastic gravitational wave background for given phase transition parameters.
ComputeGW[T,\[Alpha],\"\[Beta]/H\",\!\(\*SubscriptBox[\(\[Xi]\), \(w\)]\),g*,$Unit,\"CollisionData\"\[Rule]Association] computes the contribution from bubble wall collisions.
ComputeGW[association] extracts the relevant parameters from association.
ComputeGW[transition] extracts the relevant parameters from transition.
";
Options[ComputeGW]={
	"CollisionData"->None,
	"Sources"->{"Collisions","Soundwaves","Turbulence","Combined"},
	"SpeedOfSound"->$SpeedOfSound,"\[CurlyEpsilon]"->\[CurlyEpsilon],"\[ScriptCapitalN]"->\[ScriptCapitalN],"ShapeParameters"->\[CapitalOmega]pars
	};
AutoComplete[ComputeGW];
ComputeGW[T_,\[Alpha]_,\[Beta]H_,vw_,relDOF_,unit_,OptionsPattern[]]:=Module[{
	\[CapitalOmega]pars,
	Kcol,\[Kappa]col,\[Kappa]Data,colMetaData,
	Ksw,\[Kappa]sw,
	\[Alpha]eff,Treh,
	f\[ScriptP],f1sw,f2sw,f1mhd,f2mhd,
	HR,\[CapitalOmega]\[ScriptS],\[ScriptCapitalA]mhd,\[CurlyEpsilon],\[ScriptCapitalN],
	h2\[CapitalOmega],
	sources=OptionValue["Sources"],
	cs=OptionValue["SpeedOfSound"],
	colData=OptionValue["CollisionData"]
	},
	(*GWData["ShapeParameters"]=*)\[CapitalOmega]pars=OptionValue["ShapeParameters"];
	{\[Kappa]col,colMetaData}=kappaCollision[colData,Return->{"\[Kappa]col","Data"}];
	If[MatchQ[colMetaData,_Association],AssociateTo[GWData,colMetaData]];
	GWData["\[Kappa]col"]=\[Kappa]col;
	GWData["Kcol"]=Kcol=KCollision[\[Alpha],\[Kappa]col];
	GWData["\[Alpha]eff"]=\[Alpha]eff=AlphaEffective[\[Alpha],\[Kappa]col];
	GWData["\[Kappa]sw"]=\[Kappa]sw=kappaSoundwaves[\[Alpha]eff,vw];
	GWData["Ksw"]=Ksw=KSoundwaves[\[Alpha]eff,\[Kappa]sw];
	GWData["fp"]=f\[ScriptP]=fp[T,\[Beta]H,relDOF,unit];
	GWData["Treh"]=Treh=(1+\[Alpha](*or \[Alpha]eff?*))^(1/4) T;
	GWData["H*0"]=Hstar0[T,relDOF,unit];
	GWData["HR"]=HR=HRFromBetaH[\[Beta]H,vw];
	GWData["f1sw"]=f1sw=f1["Soundwaves",Treh,HR,relDOF,unit];
	GWData["f2sw"]=f2sw=f2["Soundwaves",Treh,HR,relDOF,unit,vw,cs];
	GWData["\[CurlyEpsilon]"]=\[CurlyEpsilon]=OptionValue["\[CurlyEpsilon]"];
	GWData["\[ScriptCapitalN]"]=\[ScriptCapitalN]=OptionValue["\[ScriptCapitalN]"];
	GWData["\[CapitalOmega]s"]=\[CapitalOmega]\[ScriptS]=\[CapitalOmega]s[\[CurlyEpsilon],Ksw];
	GWData["f1mhd"]=f1mhd=f1["Turbulence",Treh,HR,\[CapitalOmega]\[ScriptS],relDOF,unit,\[ScriptCapitalN]];
	GWData["f2mhd"]=f2mhd=f2["Turbulence",Treh,HR,relDOF,unit];
	GWData["Amhd"]=\[ScriptCapitalA]mhd=Amhd[\[CapitalOmega]pars["Turbulence","a2"]];
	h2\[CapitalOmega]=h2Omega[f\[ScriptP],{f1sw,f2sw},{f1mhd,f2mhd},Kcol,\[Beta]H,Treh,Ksw,HR,\[ScriptCapitalA]mhd,\[CapitalOmega]\[ScriptS],HR,vw,cs,\[CurlyEpsilon],\[ScriptCapitalN],relDOF,unit];
	(* SWGB templates *)
	GWData["h2Omega"]=h2\[CapitalOmega]//KeyTake[sources]//Select[Head[#]==Function&]; (* filter by "Sources" and proper spectra *)
	(* AutoComplete Keys *)
	AutoComplete[GWData];
	GWData
	]
ComputeGW[assoc_Association,opt:OptionsPattern[]]:=Module[{
	Tp,\[Alpha],\[Beta]H,vw,relDOF,unit,GWRequirements,colRequirements,missing,
	colData=OptionValue["CollisionData"]
	},
	(* extract SGWB parameters *)
	GWRequirements={"Tp","\[Alpha]","\[Beta]/H","WallVelocity","RelativisticDOF","Unit"};
	If[(missing=Complement[GWRequirements,Keys@assoc])!={},
		PT2GWPrint@msg["missingGW"][missing];
		Abort[]
		];
	{Tp,\[Alpha],\[Beta]H,vw,relDOF,unit}=GWRequirements/.assoc;
	(* optional collision data *)
	If[MatchQ[colData,_Association|_List],
		colData=Association[colData];
		colRequirements={"ActionFunction","Phases","WallVelocity","\[Alpha]","\[Beta]/H"};
		If[(missing=Complement[colRequirements,Keys@assoc])=!={},
			PT2GWPrint@msg["missing\[Kappa]col"][missing];
			colData=None,
			AssociateTo[colData,KeyTake[assoc,colRequirements]]
			];
		colData=colData/.assoc  (* allow delayed evaluation of collision data. E.g. colData=<|"T"->"Tn",..|> \[Rule] <|"T"->value,..|> *)
		];
	ComputeGW[Tp,\[Alpha],\[Beta]H,vw,relDOF,unit,"CollisionData"->colData,opt]
	]
ComputeGW[tr_Transition,opt:OptionsPattern[]]:=ComputeGW[tr[Association],opt]
AutoComplete[ComputeGW];


(* ::Subsection::Closed:: *)
(*Sensitivities*)


(* ::Text:: *)
(*Peak-integrated sensitivity curves from 2002.04615*)


PISC::usage="PISC[f,\"detector\"] gives the peak-integrated sensitivity curve for \"detector\", at the frequency f.";
Options[PISC]={"Source"->"Soundwaves"};
PISCDetectors={"LISA","DECIGO","BBO"};
PISC[f_,detector_String,opt:OptionsPattern[PISC]]:=Module[{
	\[ScriptF],\[DoubleStruckF],polyCoeffs,powers,powersalt,fpowers,norm,hstar,window=10^{-6,2},source=OptionValue["Source"]
	},
	\[ScriptF]=10^3 f; (* normalized frequency: \[ScriptF]=f/1mHz *)
	If[source=="Turbulence",hstar=Lookup[GWData,"H*0","H*0"]];
	(* select detector *)
	Switch[detector,
		"LISA",
		(*window=10.^{-5,2};*)
		Switch[source,
			"Collisions",
				polyCoeffs={2.63, 3.26 10^-1,3.29 10^-3, 4.67 10^-3};
				powers={-1,1,2,2.8};
				fpowers=\[ScriptF]^powers;
				norm = 10.^-14,
			"Soundwaves",
				polyCoeffs={3.58 10^-3,3.26 10^-1,1.20,2.48,2.85 10^-1,1.81 10^-2,1.50 10^-3};
				powers={-4,-3,-2,-1,1,2,3};
				fpowers=\[ScriptF]^powers;
				norm = 10.^-14,
			"Turbulence",
				\[DoubleStruckF]=hstar f;
				polyCoeffs={1.07, 1.96, 3.50, 4.77 10^-1, 3.32 10^-2, 1.05 10^-4};
				powers={-5/3, -1, 0,1,2,3};
				powersalt={0.98, 1.04, 0.96, 1.03, 0.96, 0.05};
				fpowers=\[ScriptF]^powers \[DoubleStruckF]^powersalt;
				norm = 10.^-12
			],
		"DECIGO",
		(*window=10.^{-3,1};*)
		Switch[source,
			"Collisions",
			polyCoeffs={2.06, 7.61 10^-3, 4.58 10^-5, 2.19 10^-7, 6.35 10^-10};
				powers={-1,0,1,2,2.8};
				fpowers=\[ScriptF]^powers;
				norm = 10.^-15,
			"Soundwaves",
				polyCoeffs={3.82 10^-1,2.26,1.10 10^-3,2.56 10^-6,2.91 10^-8,7.54 10^-12};
				powers={-4,-1.5,0,1,2,3};
				fpowers=\[ScriptF]^powers;
				norm = 10.^-14,
			"Turbulence",
				\[DoubleStruckF]=hstar f;
				polyCoeffs={5.31, 3.34, 3.75 10^-2, 2.16 10^-4, 1.92 10^-7, 9.33 10^-12};
				powers={-5/3, -1, 0,1,2,3};
				powersalt={1.00, 1.00, 0.99, 1.01, 0.96, 0.07};
				fpowers=\[ScriptF]^powers \[DoubleStruckF]^powersalt;
				norm = 10.^-13
			],
		"BBO",
		(*window=10.^{-3,1};*)
		Switch[source,
			"Collisions",
				polyCoeffs={8.24, 1.61 10^-2, 1.98 10^-4, 4.24 10^-9, 2.06 10^-9};
				powers={-1,0,1,2,2.8};
				fpowers=\[ScriptF]^powers;
				norm=10.^-16,
			"Soundwaves",
				polyCoeffs={1.77 10^-1,1.06,1.35 10^-4,2.23 10^-6,1.29 10^-9,2.99 10^-12};
				powers={-4,-1.5,0,1,2,3};
				fpowers=\[ScriptF]^powers;
				norm = 10.^-14,
			"Turbulence",
				\[DoubleStruckF]=hstar f;
				polyCoeffs={2.44, 1.41, 1.00 10^-2, 5.06 10^-5, 3.19 10^-8, 1.37 10^-12};
				powers={-5/3, -1, 0,1,2,3};
				powersalt={1.00, 1.00, 1.00, 1.00, 0.98, 0.31};
				fpowers=\[ScriptF]^powers \[DoubleStruckF]^powersalt;
				norm = 10.^-13
			],
		_,PT2GWPrint@msg["noDetector"][detector,PISCDetectors];Return[Indeterminate]
		];
		\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"norm", " ", 
RowBox[{"polyCoeffs", ".", "fpowers"}]}], 
RowBox[{
RowBox[{"Between", "[", "window", "]"}], "[", "f", "]"}]},
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
StripWrapperBoxes->True]\)
		]
addCodeCompletion["PISC"][0,PISCDetectors];
AutoComplete[PISC];


(* ::Text:: *)
(*Power-law integrated sensitivity curves from 2002.04615*)


(* compressed PLISC data *)
logPLISCcompressed="1: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";
logPLISC=Uncompress[logPLISCcompressed];
PLISCDetectors=Keys[logPLISC]//Sort;


PLISC::usage="PLISC[f,\"detector\"] gives the power-law integrated sensitivity curve for \"detector\", at the frequency f.";
PLISC[f_,detector_]:=10^logPLISC[detector][Log10[f]]
addCodeCompletion["PLISC"][0,PLISCDetectors];


(* ::Text:: *)
(*Overall sensitivity function (PISC or PLISC)*)


GWSensitivities::usage="GWSensitivities[f,\"detectorConfiguration\"] gives the sensitivity of the specified detector configuration at frequency f.
GWSensitivities[f,\"detector PISC\",\"Source\"->source] specifies the GW source (defaults to \"Soundwaves\"), for peak-integrated sensitivity curves (PISCs).";
$DetectorSensitivities=Join[#<>" PISC"&/@PISCDetectors,#<>" PLISC"&/@PLISCDetectors]//Sort;
GWSensitivities[f_,detectorConfiguration_,opt:OptionsPattern[]]:=Module[{detector,sens},
	{detector,sens}=StringSplit[detectorConfiguration];
	\!\(\*
TagBox[GridBox[{
{"\[Piecewise]", GridBox[{
{
RowBox[{"PLISC", "[", 
RowBox[{"f", ",", "detector"}], "]"}], 
RowBox[{
RowBox[{"sens", "==", "\"\<PLISC\>\""}], "\[And]", 
RowBox[{"MemberQ", "[", 
RowBox[{"PLISCDetectors", ",", "detector"}], "]"}]}]},
{
RowBox[{"PISC", "[", 
RowBox[{"f", ",", "detector", ",", "opt"}], "]"}], 
RowBox[{
RowBox[{"sens", "==", "\"\<PISC\>\""}], "\[And]", 
RowBox[{"MemberQ", "[", 
RowBox[{"PISCDetectors", ",", "detector"}], "]"}]}]},
{
RowBox[{
RowBox[{
RowBox[{"PT2GWPrint", "@", 
RowBox[{
RowBox[{"msg", "[", "\"\<noDetectorConf\>\"", "]"}], "[", 
RowBox[{"detectorConfiguration", ",", "$DetectorSensitivities"}], "]"}]}], ";"}], "\n", "Indeterminate"}], "True"}
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
	]
GWSensitivities[f_,All,opt:OptionsPattern[]]:=GWSensitivities[f,#,opt]&/@$DetectorSensitivities
GWSensitivities["List"]=$DetectorSensitivities;
addCodeCompletion["GWSensitivities"][{"List"},$DetectorSensitivities,{"Source"}];


(* ::Section::Closed:: *)
(*End Package*)


End[]; (* end "Private`" *)


(* ReadProtected attribute on public symbols prevents rendering of huge box with all 
definitions (DownValues) when they are called in Information or with shortcut ?FindBounce. *)
SetAttributes[Evaluate@Names["`*"],{ReadProtected}]; (* from FindBounce.m *) 


EndPackage[];
