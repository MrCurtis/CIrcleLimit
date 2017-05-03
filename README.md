Circle Limit
============

A small application for plotting figures in hyperbolic space.

Getting Started
---------------

A vagrant file is included. To start developing cd in to the projects root directory and type:

    vagrant up && vagrant ssh

Inside the vagrant box change in to the correct directory

    cd /vagrant/circle_limit

and then start sbt using xvfb for headless browser testing

    xvfb-run sbt

Once on the sbt command line type

    compile

to compile the source. The finished result can be viewed by putting 

    <path>/<to>/<project>/circle_limit/circle_limit.html 
