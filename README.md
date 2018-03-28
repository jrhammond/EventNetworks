`EventNetworks`
=====

Download, process, and transform historic and live-updated event data from the Phoenix and ICEWS repositories to temporal event-network structures for analysis.

Package description
------------
`EventNetworks` includes a set of functions designed to gather event data, format and process these data, convert them into daily networks of interactions between states in the international system. 

Data sources
------------
The package can intake any or all of three different data sources:

1. For events occurring between 1995 and one year behind the present day, it can use the public release of the [ICEWS data](https://dataverse.harvard.edu/dataverse/harvard?q=icews) for events occurring between 
1995 and one year behind the present day. 
2. For events occurring from June 2014 through the present, it can use
the [daily live-updated Phoenix data](http://phoenixdata.org/data/current) created and
released through the [Open Event Data Project](http://openeventdata.org). 
3. For events occurring from January 1945 to December 2015, it can use the [historic Phoenix data](http://www.clinecenter.illinois.edu/data/speed/phoenix/)created and maintained by the Cline Center for Democracy at the University of Illinois Urbana-Champaign. The historic Phoenix is based on three data sets (the New York Times, the BBC Summary of World Broadcasts, and the CIA's Foreign Broadcast Information Service) with differing timespan, which means that the base content will vary depending on the time specified.

Note that this means there are some time periods where events will be constructed
using more than one data source. For example, events in 2015 are drawn from ICEWS, the live-updated Phoenix, and the historic Phoenix BBC SWB records. In cases where sources overlap, event records are de-duplicated based on the event-dyad-day tuple. I advise careful comparison of time periods with partial multiple-source overlap, as even with de-duplication this can introduce some significant changes in the number and coverage of reported events.

The main function 'eventNetworks' intakes raw data files based on the provided folder locations for live Phoenix, historic Phoenix, and ICEWS data sets. If files are not found for live Phoenix and ICEWS (both of which are regularly updated), it will automatically attempt to download the full data sets. If the "Update" argument is set as TRUE, it will also compare the existing files to the online repositories for the dates requested.

More information on data sets
------------
The current development of the Phoenix Data Project is a collaborative effort between Caerus Associates (Erin Simpson, Andrew Halterman, and John Beieler), Parus Analytics (Phil Schrodt), The University of Texas at Dallas (Patrick Brandt), The Cline Center for Democracy at the University of Illinois at Urbana-Champaign, and The University of Oklahoma. Visit the Phoenix website here: [http://phoenixdata.org/](http://phoenixdata.org/) and the website of the Open Event Data Alliance here: [http://openeventdata.org/](http://openeventdata.org).

The historic Phoenix data files are maintained by the Cline Center for Democracy at the University of Illinois Urbana-Champaign. Visit the Cline Center's website here: [http://www.clinecenter.illinois.edu/data/speed/phoenix/](http://www.clinecenter.illinois.edu/data/speed/phoenix/).

The Integrated Crisis Early Warning System (ICEWS) public-release data is an ongoing data-coding and analysis initiative hosted at Lockheed Martin, originally funded through DARPA, and more recently  funded through the Office of Naval Research. ICEWS data is being released monthly through the Harvard Dataverse, with a 12-month (give or take a few months) lag: as of 12/10/2017, ICEWS data is available through the end of November 2016. For more information, visit [The W-ICEWS site](http://www.lockheedmartin.com/us/products/W-ICEWS/W-ICEWS_Team/Publications.html) at Lockheed Martin.

Current status and updates
------------
`EventNetworks` is still in a very early stage of development, and is likely to change significantly over time.

__Recent changes:__
1. I have integrated functionality for processing and converting the historic Phoenix data released by the Cline Center for Democracy at UIUC. These data can be used on their own or in conjunction with the live Phoenix and ICEWS data. This means it's now possible to generate event-networks for the international system going all the way back to 1945 (although at that point you're relying solely on the NYT digitized record, with all the accompanying geographic and substantive bias one would anticipate).
2. I have finally gotten around to fixing the major bottleneck in ICEWS data processing from complex actor descriptors to the standardized CAMEO actor set. This change significantly speeds up processing time.
3. EventNetworks now outputs data as a __list of array objects__. Each list entry corresponds to one time unit, and each array has dimensions (_k_ x _k_ x _m_) where _k_ = number of actors and _m_ = number of network layers, or number of unique tie types returned.


Long-term to-Do List
------------
- [x] Add support for specifying a subset of actors to examine, or a 'container' (e.g., a state) within which to examine all actors.
- [x] Add support for specifying the level of temporal aggregation for event-networks (e.g., day/week/month/year).
- [x] Add support for specifying a particular class of interactions to extract and examine (e.g., rootcodes 2,4,6).
- [x] Increase efficiency of network-stats extraction module by enabling parallelization of plyr functions using doMC backend.
- [x] Add support for using the Cline Center's recent historic Phoenix data releases (http://www.clinecenter.illinois.edu/data/speed/phoenix/)
- [x] Speed up the internal functions, particularly the ICEWS-to-CAMEO-code conversion. This is a major bottleneck.
- [ ] Circle back around to update and improve the network-stats functionality to extract and present more useful information from the generated networks.
- [ ] Clean up the documentation and clarify arguments to make it easier for others to use.
- [ ] Set up more informative error messages :)


Installation
------------
`devtools::install_github("jrhammond/EventNetworks")`
```
> pacman::p_load(EventNetworks)
> 
> sample_data <- EventNetworks::eventNetworks(
+   start_date = 20140101
+   , end_date = 20150101
+   , level = 'pentaclass'
+   , dv_key = [personal Harvard dataverse redacted]
+   , phoenix_loc = '/Users/localadmin/Box Sync/DataSets/phoenix'
+   , icews_loc = '/Users/localadmin/Box Sync/DataSets/icews'
+   , histphoenix_loc = '/Users/localadmin/Box Sync/DataSets/CCHPED_v2017_06_30'
+   , dv_server = 'harvard.dataverse.edu'
+   , update = F
+   , actorset = 'states'
+   , codeset = 'all'
+   , time_window = 'month'
+   , code_subset = 'all'
+   , tie_type = 'count'
+   , sources = 'all'
+   )
Checking ICEWS data...
Ingesting ICEWS data...
Reading in files...
  |===============================================================| 100%
Process complete
Munging ICEWS data...
Ingesting Phoenix data...
Note: specified range precedes the earliest Phoenix data.
Reading in files...
  |===============================================================| 100%
Process complete
Ingesting historic Phoenix data...
Read 817955 rows and 25 (of 25) columns from 0.096 GB file in 00:00:03
Read 1092211 rows and 25 (of 25) columns from 0.133 GB file in 00:00:03
Read 2906715 rows and 25 (of 25) columns from 0.373 GB file in 00:00:08
 ```
