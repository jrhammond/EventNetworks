`phoenixNet`
=====

Download, munge, and transform Phoenix and ICEWS auto-coded event data to daily event-networks.

`phoenixNet` includes a set of functions designed to gather event data, format and munge
these data, convert them into daily networks of interactions between states in the 
international system, and return a large set of network-level, dyad-level, and node-level
statistics over time. 

These functions rely on the public release of the [ICEWS data](https://dataverse.harvard.edu/dataverse/harvard?q=icews) for events occurring between 
1995 and October of 2014. For events occurring from June 2014 through the present, they
utilize the [Phoenix daily data sets](http://phoenixdata.org/data/current) created and
released through the [Open Event Data Project](http://openeventdata.org). Note that this
means there are some time periods (currently June-October 2014) where events will be constructed
using *both* Phoenix and ICEWS; in these cases, event records are de-duplicated and some diagnostics
are reported showing the level of overlap between the two data sources.

The main function 'phoenix_net' intakes raw data files. It will look for them on disk, intaking
the paths of Phoenix and ICEWS files separately. Due to API issues, these data are treated differently:
- If Phoenix data are not found in the source path, they are downloaded and saved automatically. This is
done using [Andrew Halterman's](https://github.com/ahalterman/phoxy) excellent `phoxy` package.
- ICEWS data have to be downloaded manually and stored on disk to be used in the functions. The
aforementioned `phoxy' package can be used to automatically access ICEWS data through 2013, but
cannot yet access the most recent year of data. Hopefully, this will be fixed soon once I better 
understand the new version of the Harvard Dataverse API.

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

`phoenixNet` is still in a very early stage of development, and is likely to change
significantly over time.

To-Do List
------------
- [ ] Add support for specifying a subset of actors to examine, or a 'container' (e.g., a state) within which to examine all actors.
- [x] Add support for specifying the level of temporal aggregation for event-networks (e.g., day/week/month/year).
- [x] Add support for specifying a particular class of interactions to extract and examine (e.g., rootcodes 2,4,6).
- [x] Increase efficiency of network-stats extraction module by enabling parallelization of plyr functions using doMC backend.
- [x] Add additional network-level statistics for daily networks (Jaccard index & Hamming distance by network-day)
- [ ] Add additional dyad-level statistics for daily networks (node IDs of symmetric and asymmetric dyads)
- [ ] Think more about the output data structure. Nested lists are easy, but inconvenient to work with (e.g. net_data$dailynets$code10$netstats$mean_degree) so it might be better to return discrete objects.
- [ ] Clean up the documentation and clarify arguments to make it easier for others to use.
- [ ] Set up more informative error messages.


Installation
------------
`devtools::install_github("jrhammond/phoenixNet")`

Usage
-----

'phoenixNet' contains the following functions:

* 'phoenix_net' intakes a start date, end date, and event aggregation level
  as arguments, along with a pair of file directories containing Phoenix and ICEWS
  data sets as tabular files. This function outputs a two-element list of data. The
  first element is a data.table object giving daily event counts and overlapping reports
  between the two data sets. The second is a list of dynamic network objects (tsna::networkDynamic). 
  Each dynamic network describes daily interactions along one CAMEO-coded event category
  or event root category between the 255 ISO-coded states in the international system. 
  These are directed, binarized (1/0) networks, in which a tie between two states i and j
  indicates that state i initiated at least one event of a given type towards state j.

* 'phoenix_stats' intakes a list of network objects generated by 'phoenix_net', and generates
  an array of network, dyad, and nodal statistics for each event-day-network in the input.
  The resulting object is a list of lists: layer (1) contains the event or root code,
  and layer (2) contains a set of tables containing statistics of interest organized
  by level of analysis and date.
  
Example:

```
> test <- phoenix_net(start_date = 20140620, end_date = 20140720, level = 'pentaclass', phoenix_loc = '/media/jesse/Files/Dropbox/Minerva/phoenix', icews_loc = '/media/jesse/Files/Dropbox/Minerva/icews', datasource = 'both', codeset = 'all', time_window = 'week')
Checking Phoenix data...
Phoenix data is current through today.
Checking ICEWS data...
ICEWS file location is valid.
Ingesting ICEWS data...
Reading in files...
  |=================================================================================================================| 100%
Process complete
Munging ICEWS data...
Ingesting Phoenix data...
Reading in files...
  |=================================================================================================================| 100%
Process complete
Warning message:
In eval(expr, envir, enclos) : NAs introduced by coercion
> dailynets <- test$dailynets
> net_stats <- phoenix_stats(dailynets, time_window = 'week', do_parallel = T, n_cores = 4)
Extracting network statistics for code 0 ...
Extracting dyadic shared-community statistics for code 0 ...
Extracting nodal centrality and transitivity statistics for code 0 ...
Extracting network statistics for code 1 ...
Extracting dyadic shared-community statistics for code 1 ...
Extracting nodal centrality and transitivity statistics for code 1 ...
Extracting network statistics for code 2 ...
Extracting dyadic shared-community statistics for code 2 ...
Extracting nodal centrality and transitivity statistics for code 2 ...
Extracting network statistics for code 3 ...
Extracting dyadic shared-community statistics for code 3 ...
Extracting nodal centrality and transitivity statistics for code 3 ...
Extracting network statistics for code 4 ...
Extracting dyadic shared-community statistics for code 4 ...
Extracting nodal centrality and transitivity statistics for code 4 ...
> net_stats$code3$netstats
         date net_jaccard net_hamming net_degree net_density net_trans net_modularity num_communities comm_meansize
1: 2014-06-22          NA          NA   2.713725 0.005341979 0.1901592     0.24925657              11      9.272727
2: 2014-06-29   0.2795918   0.9945713   2.203922 0.004338428 0.1830986     0.34461316              17      6.117647
3: 2014-07-06   0.2902542   0.9948481   2.572549 0.005064073 0.1902157     0.29398888              11      9.636364
4: 2014-07-13   0.2882012   0.9943406   2.650980 0.005218465 0.1942171     0.05588915               7     13.571429
   xcomm_ties dyads_mut dyads_asym dyads_null triads_003 triads_012 triads_102 triads_021D triads_021U triads_021C
1:  0.1994220        88        170      32127    2669289      38867      19806         386         285         527
2:  0.3487544        61        159      32165    2678032      37028      13711         359         169         412
3:  0.2896341        71        186      32128    2669500      42809      15695         278         444         573
4:  0.0443787        90        158      32137    2671717      36347      19999         164         364         419
   triads_111D triads_111U triads_030T triads_030C triads_201 triads_120D triads_120U triads_120C triads_210 triads_300
1:         642         720          30           6        358          29          22          42         91         35
2:         376         629          29           5        226          14          19          42         62         22
3:         663         584          41           7        334          23          33          35         86         30
4:         797         585          24           4        489          34          18          29        105         40

