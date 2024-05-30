
To run the ACT-R Standalone, extract the "ACT-R" folder from the zip archive and
place it anywhere on your machine.  

If you are using macOS 10.14 or newer, then before running the software you will
need to verify that the terminal application has access to the files.  To do
that you need to open System Preferences and then Security & Privacy.  In the
Privacy section, pick "Full Disk Access" from the items on the left.  If the 
terminal application is not listed with a checkmark on the right then you will 
have to add it.  It should be located in the Applications/utilities folder.

As a first step, you need to run the "one-time-setup.command" script by double
clicking on it (it may not run properly if run from a terminal prompt) in the
extracted ACT-R folder.  That will add all of the necessary files and folders
to that folder, and it should also clear the quarantine flags so that the
applications will be allowed to run without having to explicitly allow them.
If you get a warning that the file is damaged or cannot be opened then you will
need to Control-click the file and pick Open to run it, and that will require
an admin password.

Once that is done, you just need to double click the "run-act-r.command" script
to start ACT-R and the ACT-R environment.  That script must be located in the
same directory as the rest of the ACT-R files, but you may create an alias to
it and put that elsewhere if that is more convenient.  There are also 
additional scripts which can be used for starting pieces of the software
individually, and those are described below.

If when you run that you get a warning that the file is damaged or cannot be 
opened (for that file or one of the applications) then you will have to 
explicitly allow it to run and additional instructions for that are also found
below.

Running the run-act-r script will open a terminal window titled "ACT-R" and
start the ACT-R Environment "Control Panel".  If you get a dialog window which
says "Error occurred trying to connect to ACT-R" at the top, press the No 
button to have it try again.  If that works, then to prevent that from 
happening every time you start ACT-R, you will need to press the "Options" 
button at the bottom of the "Control Panel" window, check the box next to "use
localhost instead of 127.0.0.1", and then press the save button.  If the No 
button also does not work, then you may need to adjust the firewall settings to
allow the connection.

To close the ACT-R Standalone you need to close the "ACT-R" window.  That will
quit the terminal and the Environment will ask to verify that it should quit.

See the tutorial, reference manual, and environment manual that are found in
the docs folder for more details.

The Intel processor version of the ACT-R application is built using Clozure
Common Lisp (CCL) which is available at <http://clozure.com/>.  CCL is 
distributed under the Apache License, and the license is included in the 
ccl_license folder in the docs folder.

The Apple silicon (arm64) version of the application is built using Steel Bank
Common Lisp (SBCL). SBCL is free software available from <http://sbcl.org>.
It is mostly in the public domain, but with some subsystems under BSD-style
licenses which allow modification and reuse as long as credit is given.  More
information can be found in the files in the sbcl_license folder in the docs
folder.  
-------------------------------------------------------------------------------


If you get the unidentified developer or damaged warning when trying to it:

Open System Preferences and then Security & Privacy, and go to the General
section and leave that open.

Now Control-click the run-act-r.command script and select Open. There will
be a dialog which says the file is from an unidentified developer.  Press
the Open button.  That will open another dialog requesting an admin login
to allow the script to run.  After you enter that information it will open
another dialog indicating that the start-environment-osx program is from an
unidentified developer.  Press the Cancel button and then go to the System 
Preferences window and press the "Allow Anyway" button which appears.  Then
another dialog should appear which says that the act-r-64 application is 
from an unidentified developer.  Press the Cancel button and in the System
Preferences window press the "Allow Anyway" button.  If you get another 
unidentified developer dialog press the Open button if it has one.  After 
all that it probably did not start the software successfully so quit the 
application(s) which did run and then Control-click the run-act-r.command 
script again picking Open instead of Cancel if the unidentified developer
windows appear again, and pressing the "Allow Anyway" button if it shows
up in the System Preferences again for the applications.

If you get a warning that start-environment-osx program is damaged and it will
not run, there is an alternate version which may work instead.  To use the
alternate version delete the start-environment-osx program from the 
ACT-R:environment folder and rename alternate-start-environment-osx to
start-environment-osx.  Then run as described above.

-------------------------------------------------------------------------------
Other scripts:

run-act-r-from-terminal.sh

This script is similar to the run-act-r.command script in that it starts both
the ACT-R software and the ACT-R Environment application.  However, the 
.command script is intended to be used from the GUI and will often not work if
run from a terminal.  This script will work when called from a terminal prompt.

run-act-r-only.command

This script will run only the ACT-R terminal application without starting the
Environment GUI.

run-extra-listener.command

This script can be run to open another terminal running Lisp which connects to
the running ACT-R.  That terminal provides functions/macros to call the ACT-R
commands which are described in the tutorial and may be useful for inspecting
or debugging a model.  By default that terminal will not display the ACT-R
model trace but it does display the other traces.  If the model trace is 
desired in that terminal, the include-model-trace function can be called to add
it.  Any number of extra listeners may be run at the same time.

run-html-environment.command

This script can be run to start an application which allows one to use an
alternate version of the ACT-R Environment that is implemented in javascript
and works from a browser (it can be run instead of or in addition to the
default Tcl/Tk based Environment).  After you start the application you
should then open a browser and open the act-r.html file which is found in
the ACT-R directory.  It will show two links.  One goes to the Environment
tools and the other opens a viewer for the experiment windows created by the
ACT-R AGI tools.

envstarter.py

It is possible to run the standard ACT-R Environment from source code using
Python 3 if there is a problem with the application or you prefer not to use
it.  To do so, run the envstarter.py script in the ACT-R/environment directory
using either "python envstarter.py" or "python3 envstarter.py" depending on
the name of the application for Python 3 on your machine.

-------------------------------------------------------------------------------
Connecting other software to the ACT-R interface

By default the standalone uses the localhost address for the ACT-R interface,
but if you need to make external connections, delete the force-local.lisp file 
from the patches directory before starting the software.

-------------------------------------------------------------------------------

If you have any questions or problems with this please let me know.

Dan (db30@andrew.cmu.edu)
