# Astronomical Simulation

_Version 1.2.3+20220406  ([Version Release Notes](#ReleaseNotes))_ 

**Astronomical Simulation** is an open source n-body gravitational simulation.

A random collection of simulated asteroids interact with each other via mutual gravitational interaction in a 2-dimensional space.

When two or more asteroids come into contact with each other, rather than _rebound_ off each other they instead _coalesce_ into a single larger asteroid.

Such a combined asteroid has the combined _mass_ and _momentum_ of the colliding asteroids that make it up.

Depending on the random distribution of the initial set of asteroids the simulation may evolve into a primitive _solar system_ with smaller asteroids _orbiting_ larger ones.

Alternately some or most of the asteroids may be _ejected_ via a _gravitational whip_ beyond the visible limits of the simulation space.  

## About the Software

The software is a self-contained executable program, written in **[Free Pascal](https://www.freepascal.org/)**, that runs on Microsoft Windows or Ubuntu Linux (and presumably other Linux distributions).
(No separate run-time environment is required to run the program.)
The **[Lazarus Integrated Development Environment](https://www.lazarus-ide.org/)** was used to develop the program.
(Both Free Pascal and the Lazarus IDE are free open-source software products.) 

## Downloading and Running the Program

### Microsoft Windows

You can run the Astronomical Simulation program on Microsoft Windows as follows:

- Download the **AstroSim.exe** binary executable file from the **bin** sub-folder from this GitHub.com page.

- To uninstall the program, simply delete the **AstroSim.exe** file.

### Ubuntu Linux

You can run the Astronomical Simulation program on Ubuntu Linux (and presumably other Linux distributions) as follows:

- Download the **AstroSim** binary executable file (with no file extension) from the **bin** sub-folder from this GitHub.com page.

- Ensure the **AstroSim** file has the executable permission.  From a Files window, right-click the file, select Properties, and use the Permissions tab to enable the Execute permission.  To do this in a Terminal window, use the following command:
  
    chmod +x AstroSim

- To uninstall the program, simply delete the **AstroSim** binary executable file.

### Running the Program

Double-click the downloaded copy of **AstroSim.exe** (on Windows) or **AstroSim** (on Linux) to start the simulation.

When the program starts it displays the **Astronomical Simulation** Main Form.

Here is an image of the Main Form paused during a running simulation.

![AstroSim Form](img/AstroSim.png?raw=true "AstroSim Form")

The Main Form contains these elements:

- The **Randomize** button generates a random distribution of stationary asteroids, each with a standard _diameter_ of 2 pixels and a standard _mass_ of 4.
- The **Start** button is activated once the **Randomize** button has been clicked.  Clicking the **Start** button starts the simulation.
- The **Pause** button is activated when the simulation is running and temporarily stops the simulation.  The **Randomize** button becomes active to allow initiation of a new random distribution of asteroids.  Clicking the **Start** button resumes the simulation.
- The **Step** button advances the simulation for a single time step.  It is active when the simulation is paused, or has not yet been started.
- The **# Asteroids** spin-edit control determines the number of asteroids generated by the **Randomize** button.  Use the up/down arrows on the control to set the desired number.  Alternately, directly enter the desired number into the control.
- Displayed next to the **# Asteroids** control is the current number of asteroids.  As asteroids collide and _coalesce_ together this value is reduced.  If this value decreases to **1** the simulation stops.
- The majority of the Main Form contains the graphic display area for the simulation space.  Asteroids appear white on a black background.

### Resizing the Main Form

The initial size of the Main Form is designed to fit within an 800 by 600 monitor window.

To enlarge the form, drag its boundary or simply click the _maximize_ icon (small square in the upper right of the title bar).

Then click the **Randomize** button to enlarge the graphic simulation area to match the enlarged form.

### Re-centering the Visible Area of the Simulation Space

Click on any point within the simulation area to cause the visible window into the simulation space to be centered on that point.

### Zooming the Visible Area Out and In

Zoom out the view of the simulation space by pulling the mouse wheel toward you.

Zoom back in by pushing the mouse wheel away from you.

### Simulation Notes

Initial experimentation with the **AstroSim** program seems to indicate that _smaller_ numbers of asteroids produced more interesting results than larger numbers.

The maximum number of asteroids currently allowed is **10,000**.

If a given asteroid count produces an uninteresting simulation, simply **Pause** the simulation, click **Randomize** to generate a new set of asteroids, and click **Start** to start another simulation.

## Source code compilation notes

Download the **Lazarus IDE**, including **Free Pascal**, from  here:

- **<https://www.lazarus-ide.org/index.php?page=downloads>**

After installing the **Lazarus IDE**, clone this GitHub repository to your local disk.
Then double-click on the **src\SyncDirPas.lpr** project file to open it in **Lazarus**. 

_**Note:**_ Using the debugger in the **Lazarus IDE** on Windows 10 _**might**_ require the following configuration adjustment:

- **[Lazarus - Windows - Debugger crashing on OpenDialog](https://www.tweaking4all.com/forum/delphi-lazarus-free-pascal/lazarus-windows-debugger-crashing-on-opendialog/)**

When **Lazarus** includes debugging information the executable file is relatively large.
When ready to create a release executable, the file size can be significantly reduced by selecting the menu item **Project | Project Options ...** and navigating to the **Compile Options | Debugging** tab in the resulting dialog window.
Clear the check-mark from the **Generate info for the debugger** option and then click the **OK** button.
Then rebuild the executable using the **Run | Build** menu item (or using the shortcut key-stroke _**Shift-F9**_).

<a name="ReleaseNotes"></a>

## Release Notes

### Version 1.2.3

Update current Asteroid count when using the **Step** button.

### Version 1.2.2

Show the Product Version on the main form title bar.

### Version 1.2.1

Compiled on Ubuntu Linux version 20.04.4 LTS and included resulting **AstroSim** executable file in the **bin** sub-folder.

### Version 1.2.0

Added ability to zoom the simulation view space out and in via the mouse wheel.

### Version 1.1.0

Added the ability to re-center the simulation space by clicking on a point to become the new center of the visible display area.

### Version 1.0.0

This is the initial version of the software.
