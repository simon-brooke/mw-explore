# mw-explore

Exploratory add-ons based on the MicroWorld engine, probably of interest only to me.

Not strictly speaking part of MicroWorld; the rest of the system works fine without this.

## Pre-requisites

You need **git**, **leiningen** and **mw-engine**

How you install [git](http://git-scm.com/) depends on your operating system; look for instructions [here](http://git-scm.com/book/en/v2/Getting-Started-Installing-Git).

To install [leiningen](http://leiningen.org/), follow the instructions [here](http://leiningen.org/#install).

To install [mw-engine](https://github.com/simon-brooke/mw-engine), do the following:

	git clone https://github.com/simon-brooke/mw-engine.git
    cd mw-engine
    lein install

You should then be good to go.

## Usage

Ideally, use

    lein gorilla

Then browse to the URL you're prompted with. From the menu, choose 'Load a worksheet'; currently you'll only find one. Play.

## License

Copyright © 2015 Simon Brooke

Distributed under the GNU General Public License, version 2 or (at your
option) any later version.

