phoenixNet
=====

Download, munge, and transform Phoenix auto-coded event data to daily event-networks.

`phoenixNet' includes a small set of functions that download a specified range of daily
event files from the Phoenix data storage (http://phoenixdata.org/data/current), 
process the data to remove possible duplicate events, subset by actors to focus on
interactions between state governments, and convert the resulting data to a set of daily 
event-networks by event code or event root code.

Please note: this tool does not (currently) save the raw Phoenix daily files to disk.
If you are interested in acquiring and saving the raw data to plot or analyze as 
events rather than network ties, I suggest using the excellent `phoxy' package created
by Andrew Halterman at Caerus Associates (https://github.com/ahalterman/phoxy).

The current development of the Phoenix Data Project is a collaborative effort between 
Caerus Associates (Erin Simpson, Andrew Halterman, and John Beieler), Parus Analytics 
(Phil Schrodt), The University of Texas at Dallas (Patrick Brandt), The Cline Center 
for Democracy at the University of Illinois at Urbana-Champaign, and The University 
of Oklahoma. Visit the Phoenix website here: [http://phoenixdata.org/](http://phoenixdata.org/)
and the website of the Open Event Data Alliance here: [http://openeventdata.org/](http://openeventdata.org).

`phoenixNet` is still in a very early stage of development, and is likely to change
significantly over time.

Installation
------------
`devtools::install_github("jrhammond/phoenixNet")`

Usage
-----

'phoenixNet' contains the following function:

* `phoenix_net' intakes a start date, end date, and event aggregation level
  as arguments, and outputs a list of network objects. Each network contains
  ties between the 255 ISO-recognized states in the international system,
  where a tie represents an event or interaction between two states.
  
Example:

```
> phoenix_data <- phoenix_net(20140620, 20140621, 'rootcode')
trying URL 'https://s3.amazonaws.com/openeventdata/current/events.full.20140620.txt.zip'
Content type 'application/zip' length 179107 bytes (174 KB)
downloaded 174 KB

trying URL 'https://s3.amazonaws.com/openeventdata/current/events.full.20140621.txt.zip'
Content type 'application/zip' length 341968 bytes (333 KB)
downloaded 333 KB

Warning message:
In eval(expr, envir, enclos) : NAs introduced by coercion
> phoenix_data$'code10'

$`20140620`
 Network attributes:
  vertices = 255 
  directed = TRUE 
  hyper = FALSE 
  loops = FALSE 
  multiple = FALSE 
  bipartite = FALSE 
  total edges= 7 
    missing edges= 0 
    non-missing edges= 7 

 Vertex attribute names: 
    vertex.names 

No edge attributes

$`20140621`
 Network attributes:
  vertices = 255 
  directed = TRUE 
  hyper = FALSE 
  loops = FALSE 
  multiple = FALSE 
  bipartite = FALSE 
  total edges= 5 
    missing edges= 0 
    non-missing edges= 5 

 Vertex attribute names: 
    vertex.names 

No edge attributes
