Given a scalar potential, PT2GW searches for 1^st-order, thermal phase transitions and computes gravitational wave spectra.
The package relies on [FindBounce](https://github.com/vguada/FindBounce) to compute the Euclidean bounce action.
To construct the effective potential in the dimensional reduction approach, a simple interface with [DRalgo](https://github.com/DR-algo/DRalgo) is provided.

The accompanying paper [2505.04744](https://arxiv.org/abs/2505.04744) serves as a user manual. Please cite this paper if the analysis you perform with PT2GWFinder results in a publication.

# Installation
Download the latest `.paclet` file from the [Releases](https://github.com/finshky/PT2GW/releases) repository. To permanently install to the `$UserBasePacletsDirectory`, evaluate the folloging command in Mathematica:

```PacletInstall["path/to/PT2GW.paclet-x.y.z"]```

with the appropriate path (`path/to/`) and version (`x.y.z`).

Installed versions can be listed by running `PacletFind["PT2GW"]`: Mathematica will always use the latest installed version of the package. You can get more detailed information about the latest version `PacletInformation["PT2GW"]`. All versions can be uninstalled with `PacletUninsall["PT2GW"]`.

For troubleshooting, please open an issue or contact
- [marco.finetti@hotmail.com](mailto:marco.finetti@hotmail.com)
- [marco.matteini@ijs.si](mailto:marco.matteini@ijs.si)
