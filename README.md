Given a scalar potential, PT2GW searches for 1^st-order, thermal phase transitions and computes gravitational wave spectra.
The package relies on [FindBounce](https://github.com/vguada/FindBounce) to compute the Euclidean bounce action.
To construct the effective potential in the dimensional reduction approach, a simple interface with [DRalgo](https://github.com/DR-algo/DRalgo) is provided.

The accompanying paper [2505.04744](https://arxiv.org/abs/2505.04744) serves as a user manual. Please cite this paper if the analysis you perform with PT2GWFinder results in a publication.

For troubleshooting, please open an issue or contact
- [marco.finetti@hotmail.com](mailto:marco.finetti@hotmail.com)
- [marco.matteini@ijs.si](mailto:marco.matteini@ijs.si)


# Installation
Download the latest `.paclet` file from the [Releases](https://github.com/finshky/PT2GW/releases) repository. To permanently install to the `$UserBasePacletsDirectory`, evaluate the folloging command in Mathematica:

```PacletInstall["path/to/PT2GW.paclet-x.y.z"]```

with the appropriate path (`path/to/`) and version (`x.y.z`).

Installed versions can be listed by running `PacletFind["PT2GW"]`: Mathematica will always use the latest installed version of the package. You can get more detailed information about the latest version `PacletInformation["PT2GW"]`. All versions can be uninstalled with `PacletUninsall["PT2GW"]`.

# Requirements
- [Mathematica](https://www.wolfram.com/mathematica/) 13.x and 14.x
- [FindBounce](https://github.com/vguada/FindBounce) 1.1.0

To implement a model in the dimensional reduction approach:
- [DRalgo](https://github.com/DR-algo/DRalgo) 1.x

# Quick start
A [coupled fluid-field model](https://arxiv.org/abs/1504.03291) and a [Dark Abelian Higgs model](https://ui.adsabs.harvard.edu/abs/2019JCAP...07..007B/abstract) are readily implemented in `PT2GWFinder`. Here we present the minimal setup required to run the former.
1. Load `PT2GWFinder`:\
   ```<<PT2GW` ```
3. Load the pre-implemented models:\
   ```<<PT2GW/Models.m```
5. Load a benchmark of the coupled-fluid field (CFF) model:\
   ```V=CFFModel[1]```
7. Run the search for phase transitions, selecting a bubble wall velocity `vw` and a tracing method:\
   ```SearchPotential[V,vw=0.9,"TracingMethod"->NSolve]```
   
   The following information is displayed:
   
   <img width="603" alt="transition_output_cff" src="https://github.com/user-attachments/assets/7f88e471-bb11-43b6-b759-b0583d141879" />

When a first-order phase transition is identified, a `Transition` object is returned, which can be easily inspected.
- Extract transition parameters:\
  ```transition[{"α","β/H","fPeak"}]```
  
  ```{0.00154779, 1677.1, 0.0193534}```
- Plot the transition diagram:\
  ```PlotTransition[transition]```
  
  <img width="373" alt="transition_plot_cff" src="https://github.com/user-attachments/assets/13303b7b-7a8d-4313-aa0b-619113e84890" />

- Plot the Euclidean action as a function of temperature:\
  ```PlotAction[transition]```
  
  <img width="468" alt="action_plot_cff" src="https://github.com/user-attachments/assets/5d4b54ee-b083-4163-8615-ee5650095776" />

- Plot the gravitational wave spectra:\
  ```PlotGW[transition]```
  
  <img width="504" alt="GW_plot_cff" src="https://github.com/user-attachments/assets/d00325f0-42d7-4caa-b0d4-bf250ad7dbfd" />

The Dark Abelian Higgs is loaded instead with `DPModel[#benchmark]`.

# Implementing a model
`PT2GW` requires a thermal potential of the form $V(\phi,T)$ as input, where $\phi$ is the scalar-field direction along which a transition is expected. To construct an effective potential, we provide
1. `DarkAbelianHiggs_CW`: an example notebook implementing the Dark Abelian Higgs model in the "daisy resummation" approach.
   The potential is constructed in a straightforward and intuitive way, making it easy to adapt to a generic model.
3. `DRTools`: a tool to implement a model in the dimensional reduction approach, provided all quantities from the dimensionally reduced theory are first comptued with `DRalgo`.
   The tool can loaded with `<<PT2GW/DRTools.m`.
   We provide an example notebook implementing the same Dark Abelian Higgs model with this method, which can be found in `Examples/DarkAbelianHiggs_DR`.

Alternatively, you can construct any thermal potential $V(\phi,T)$ by your own means.

We welcome community contributions of custom models. To submit a model, please use the [Issue Tracker](https://github.com/finshky/PT2GW/issues). All submissions will be reviewed and verified before inclusion in the official model repository.
