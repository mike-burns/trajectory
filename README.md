Trajectory
----------

Tools and a library.

Installation
============

    % cabal install trajectory
    % export PATH=~/.cabal/bin
    % inittj

You can find your API key in [the Trajectory settings](https://www.apptrajectory.com/profile/edit).

The account name and project name come from the URL:

    https://www.apptrajectory.com/ACCOUNTNAME/PROJECTNAME/stories

It probably makes a ton of sense to use a shell alias:

    % alias lsos='lsstory thoughtbot opensource'

Synopsis
========

Done:

    % inittj
    % inittj --profile thoughtbot-support

    % lsstory accountname projectname
    % lsstory accountname projectname --idea "Spam is out of control"
    % lsstory accountname projectname --next
    % lsstory accountname projectname --unstarted
    % lsstory accountname projectname --unestimated
    % lsstory accountname projectname --unstarted --next --unassigned --incomplete
    % lsstory accountname projectname --within-milestone "Fully Backboned"
    % lsstory accountname projectname --profile thoughtbot-support --project "Trajectory"
    % lsstory accountname projectname --completed

    % lsstory accountname projectname --development
    % lsstory accountname projectname --design
    % lsstory accountname projectname --design --development

    % lsstory accountname projectname --all-iterations
    % lsstory accountname projectname --current-iteration
    % lsstory accountname projectname --iteration 2012-01-02

To do:

    % lsidea
    % lsmilestone
    % lsmilestone --unstarted --next

    % mkstory "As a user I want to always be signed in so I don't have to care"
    % mkstory "As an admin I want to delete comments so I can remove spam" --idea "Spam is out of control"
    % mkstory "As a visitor I am not able to see private messages so I can maintain security" -m -a screenshot.png --top

Copyright
=========

Copyright 2011 Mike Burns
