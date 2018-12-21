Circle Limit
============

A small application for plotting figures in the Poincaré disc.

![Screenshot](https://raw.github.com/MrCurtis/CircleLimit/gh-pages/circle_limit_screenshot.png)

For Users
---------

The app can be found [here](http://mrcurtis.github.io/CircleLimit/).

* To start drawing geodesics double-click anywhere on the disc.
* Each subsequent single-click will add another handle point.
* Double-click again to finish drawing.
* Triple-clicking on a handle point deletes it.
* Drag handles to move them.
* Clicking on the black dot plots images of the curve under a group.

For Developers
---------------

A vagrant file is included. To start developing cd in to the projects root
directory and type:

    vagrant up && vagrant ssh

Inside the vagrant box change in to the correct directory

    cd /vagrant/circle_limit

and then start sbt using xvfb for headless browser testing

    xvfb-run sbt

Once on the sbt command-line run the tests to check that everything is working
correctly.

    test

This will also compile the javascript. The finished product can then be viewed
by entering

    file:///<path>/<to>/<project>/circle_limit/circle_limit.html

into the browser url bar.
