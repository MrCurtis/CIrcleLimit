Circle Limit
============

A small application for plotting figures in the Poincaré disc.


For Users
---------

The app can be found [here](http://mrcurtis.github.io/CircleLimit/).

* To start drawing geodesics double-click anywhere on the disc.
* Each subsequent single-click will add another handle point.
* Double-click again to finish drawing.
* Triple-clicking on a handle point deletes it.

For Developers
---------------

A vagrant file is included. To start developing cd in to the projects root
directory and type:

    vagrant up && vagrant ssh

Inside the vagrant box change in to the correct directory

    cd /vagrant/circle_limit

and then start sbt using xvfb for headless browser testing

    xvfb-run sbt

Once on the sbt command run the tests to check that everything is working
correctly.

    test

This will also compile the javascript. The finished product can then be viewed
by entering

    file:///<path>/<to>/<project>/circle_limit/circle_limit.html

into the browser url bar.
