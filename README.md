`EventNetworks`
=====

__Note:__ I'm finally getting back to making some updates to the code and documentation. The end result should be a cleaner and more workable tool.

Download, munge, and transform Phoenix and ICEWS auto-coded event data to daily event-networks.

`EventNetworks` includes a set of functions designed to gather event data, format and munge
these data, convert them into daily networks of interactions between states in the 
international system. 

These functions rely on the public release of the [ICEWS data](https://dataverse.harvard.edu/dataverse/harvard?q=icews) for events occurring between 
1995 and one year behind the present day. For events occurring from June 2014 through the present, they
utilize the [Phoenix daily data sets](http://phoenixdata.org/data/current) created and
released through the [Open Event Data Project](http://openeventdata.org). Note that this
means there are some time periods (currently June-October 2016) where events will be constructed
using *both* Phoenix and ICEWS; in these cases, event records are de-duplicated and some diagnostics
are reported showing the level of overlap between the two data sources. In general, I recommend only
using one of the two data sets, as this overlap may produce some odd biases.

The main function 'EventNetworks' intakes raw data files. It will look for them on disk, intaking
the paths of Phoenix and ICEWS files separately. If files are not found, it will automatically 
attempt to download the full data sets. If the "Update" argument is set as TRUE, it will also 
compare the existing files to the online repositories for the dates requested.

The current development of the Phoenix Data Project is a collaborative effort between 
Caerus Associates (Erin Simpson, Andrew Halterman, and John Beieler), Parus Analytics 
(Phil Schrodt), The University of Texas at Dallas (Patrick Brandt), The Cline Center 
for Democracy at the University of Illinois at Urbana-Champaign, and The University 
of Oklahoma. Visit the Phoenix website here: [http://phoenixdata.org/](http://phoenixdata.org/)
and the website of the Open Event Data Alliance here: [http://openeventdata.org/](http://openeventdata.org).

The Integrated Crisis Early Warning System (ICEWS) public-release data is an ongoing data-coding
and analysis initiative hosted at Lockheed Martin, originally funded through DARPA, and more recently 
funded through the Office of Naval Research. ICEWS data is being released monthly through the Harvard
Dataverse, with a 12-month (give or take a few months) lag: as of 11/3/2015, ICEWS data is available
through the end of June 2014. For more information, visit [The W-ICEWS site](http://www.lockheedmartin.com/us/products/W-ICEWS/W-ICEWS_Team/Publications.html) at Lockheed Martin.

`EventNetworks` is still in a very early stage of development, and is likely to change
significantly over time.

To-Do List
------------
- [x] Add support for specifying a subset of actors to examine, or a 'container' (e.g., a state) within which to examine all actors.
- [x] Add support for specifying the level of temporal aggregation for event-networks (e.g., day/week/month/year).
- [x] Add support for specifying a particular class of interactions to extract and examine (e.g., rootcodes 2,4,6).
- [x] Increase efficiency of network-stats extraction module by enabling parallelization of plyr functions using doMC backend.
- [ ] Add support for using the Cline Center's recent historic Phoenix data releases (http://www.clinecenter.illinois.edu/data/speed/phoenix/)
- [ ] Speed up the internal functions, particularly the ICEWS-to-CAMEO-code conversion. This is a major bottleneck.
- [ ] Think more about the output data structure. Nested lists are easy, but inconvenient to work with (e.g. net_data$dailynets$code10$netstats$mean_degree) so it might be better to return discrete objects.
- [ ] Clean up the documentation and clarify arguments to make it easier for others to use.
- [ ] Set up more informative error messages.


Installation
------------
`devtools::install_github("jrhammond/EventNetworks")`
