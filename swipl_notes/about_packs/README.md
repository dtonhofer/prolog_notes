# Packs

SWI-Prolog apparently has to distinct elements of extension

**Packages**: Official extension to SWI Prolog to cover certain functionalities

In the [SWI-Prolog manual](https://eu.swi-prolog.org/pldoc/doc_for?object=manual), open the
drop-down menu _Documentation > Packages_ and you reach the ["Packages" page](https://eu.swi-prolog.org/pldoc/doc_for?object=packages)

We read: 

> Packages are relatively independent add-on libraries that may not be available in all installations. 
> Packages are part of the source code releases of SWI-Prolog and may be enabled or disabled during the build.
  
For example:

- SWI-Prolog Semantic Web Library 3.0
- Constraint Query Language - A high level interface to SQL databases
- SWI-Prolog binding to GNU readline
- SWI-Prolog ODBC Interface

Note that there is another page with the SWI-Prolog [lbraries](https://eu.swi-prolog.org/pldoc/man?section=libpl).

**Packs**: Inofficial extensions by third parties 

These may also be called **Add-Ons** or **Extension Packages** (the vocabulary seems to be a bit inconsistent).

We read:

> Please be aware that packs are not moderated. Installing a pack does not execute code in the pack, 
> but simply loading a library from the pack may execute arbitrary code.

The package manager seems to be SWI-Prolog specific. It is in the following library:

- [`library(prolog_pack)` -  A package manager for Prolog](https://eu.swi-prolog.org/pldoc/man?section=prologpack)
   - [pack_list_installed/0](https://eu.swi-prolog.org/pldoc/doc_for?object=pack_list_installed/0)
   - [pack_info/1](https://eu.swi-prolog.org/pldoc/doc_for?object=pack_info/1)
   - [pack_search/1]()
   - [pack_list/1]()
   - [pack_install/1](https://eu.swi-prolog.org/pldoc/doc_for?object=pack_install/1)
   - [pack_install/2]()
   - [pack_url_file/2]()
   - [pack_rebuild/1]()
   - [pack_rebuild/0]()
   - [environment/2](https://eu.swi-prolog.org/pldoc/doc_for?object=environment/2)
   - [pack_upgrade/1]()
   - [pack_remove/1]()
   - [pack_property/2]()
  
Howtos:

- [Howto: Creating and submitting extension packages for SWI-Prolog](https://eu.swi-prolog.org/howto/Pack.html)
- [Howto: Creating a pack that uses C or C++ code](https://eu.swi-prolog.org/howto/ForeignPack.txt)

Specification:

- [Spec: The contents of `pack.pl`](https://eu.swi-prolog.org/howto/PackInfo.txt) (the package MANIFEST file, basically)

Some specific modules of the distribution (not sure what these are, they show up in the search drop-down):

- [pack_info.pl -- Visual (web) components that show info about packs](https://eu.swi-prolog.org/pldoc/doc/_CWD_/pack_info.pl)
- [pack_analyzer.pl -- Analyse the content of a Prolog pack](https://eu.swi-prolog.org/pldoc/doc/_CWD_/pack_analyzer.pl)
- [pack_mirror.pl-- Mirror pack archives](https://eu.swi-prolog.org/pldoc/doc/_CWD_/pack_mirror.pl)

List   

- [List of submitted packs](https://eu.swi-prolog.org/pack/list)

*Next up*: How to build a pack from a github repository.

- It should be possible to have multiple versions of the pack (that sounds like scripting)
- How do I generate the SWI-Prolog equivalent of JavaDoc? One can inspect the doc with a running "document web server" but can it be just HTML pages?
- How do I subvert security? (Ha HA!)
- What tools do I need?
- Oh man.
