FlatLand.arx  Copyright 2002  Tony Tanzillo   All rights reserved

In order to use this software, you must read and agree to 
terms of the End User License Agreement, in the included 
file FlatLand.lic

IMPORANT: FLATLAND.ARX is bound to the file FlatLand.Lic.

We apologize to all for the inconvenence of the required
license file, but we have been informed that FlatLand.arx
is being pirated, and that altered copies of it in which 
the author's name and copyright have been removed, have 
been finding their way into other software products as so
called "bonus" material. 

As a countermeasure, FlatLand.arx now requires the presence 
of an unmodified copy of the included file, FlatLand.lic.

In order for FlatLand.arx to run, the included file 
FlatLand.Lic must be placed in the same folder/location 
as FlatLand.arx. In addition, the file FlatLand.lic must
not be altered from its distributed form. 

If the included FlatLand.lic is not present or if it has 
been modified from its original form, FlatLand.arx will 
not function. 

Additionally, FlatLand.arx now performs a self-check to
determine if it has been tampered with, and will not run
if it has been altered in any way.

Note: This software requires AutoCAD 2000 or later.

Installation:

1. Extract the entire contents of the distribution archives 
   to a temporary folder and from that folder, copy both 
   FlatLand.arx and FlatLand.lic to a folder that is located 
   on the AutoCAD Library search path. 

2. Add FlatLand.Arx to your AutoCAD startup suite (From the 
   Tools menu, choose "Load Application", and then click on 
   the StartUp Suite button).
   
   If you see an error message displayed when FlatLand.arx
   loads, it probably is caused by not having the FlatLand.lic
   file in the same location as FlatLand.arx. FlatLand.lic
   must be located in the same folder as FlatLand.arx, or the
   latter will not function.


Documentation

Flatland.arx includes a number of tools that provide
enhanced flexibility and control over coordinate entry
in AutoCAD 2000 or later. These include:

1.  Temporary disabling of OSNAP, OTRACK, and other drawing
    aids. While entering coordinate input, you can disable
    running object snap and tracking by holding the ALT key
    down while picking points. If you do this and pick a point,
    the point AutoCAD computes is not influenced by object snap
    or tracking.

2.  FLATLAND command

    The FLATLAND command implements a running coordinate filter
    whose effects are similar to the old FLATLAND setting in 
    earlier releases of AutoCAD.

    When turned on, any coordinate influenced by object snap or 
    tracking is further modified, such that its coordinate is 
    translated in a direction parallel to the Z axis of the 
    current UCS, and is projected into the UCS XY plane (Z=0). 
    
    In other words, the Z component of all osnapped-to 
    coordinates are automatically changed to the value 0.0.

    When FLATLAND is off you can toggle it on temporarily for
    only the current coordinate by holding down the CTRL key.

    When FLATLAND modifies the cursor coordinate, it displays
    a vector extending from the current osnapped-to point to
    the XY plane of the current UCS, and displays an X in the
    UCS XY plane at the coordinate that will result if you
    press the pick button.

    To see how FLATLAND works, open the included
    flatland.dwg file, and follow these instructions:

    1.  Set running object snap to ENDPOINT and turn OSNAP on.

    2.  Issue FLATLAND and enter "ON" to turn FLATLAND on.

    3.  Issue the LINE command, and move the cursor over the
        roof edge geometry (which is drawn in the color red).
        When you do this, you will see a vector extending
        from the osnap marker location, down to the XY plane of 
        the WCS, along with an "X" at the end of the vector. 
        
        If you press the pick button while the vector and the
        "X" marker is visible, the resulting point is computed
        by projecting the current object snapped-to point into
        the XY plane of the current UCS (the WCS in this case), 
        which allows you to 'trace' a projection of the roof 
        geometry endpoints onto the WCS XY plane.

        The point denoted by the "X" marker is the point that's
        returned to AutoCAD if you press the pick button. This
        point is the projection of the osnap point into the XY
        plane of the current UCS.

    In essence, pressing CTRL to activate flatland for a
    single point, is precisely the same as using an .XY
    coordinate filter sequence like this:

       Enter point: .XY of <osnap to point> (need Z): 0

    Generally, keeping FLATLAND off and using the Ctrl key is
    the normal and most often used mode of operation. Turning
    FLATLAND on is good for more specialized cases where you
    want to use the XY projection of many coordinates to 'trace'
    projections of existing 3D geometry.

    For example, if you set the current UCS in the flatland.dwg
    file, so the UCS XY plane is parallel to the side of the
    birdhouse, you can draw an elevation of same by simply
    turning FLATLAND on and using the LINE command to trace the
    various profile  elements in the 3D model. As you draw,
    osnapped-to coordinates will be projected into the UCS XY
    plane.

    Caveats:  When using FLATLAND to draw 2D polylines, the
    results may be unpredictable, because the vertices of 
    2D polylines are constrained to be coplanar.

3.  The OSNAPLAYERS command

    This command allows you to selectively disable running
    object snap for all objects on one or more layers.

    To disable running object snap for objects on a given
    layer or layers, issue the OSNAPLAYERS command, and
    enter a wildcard pattern that specifies the name of
    one or more layers to disable object snap for (this can
    be any valid wildcard that is accepted by the AutoCAD
    -LAYER command).  For example, you can specify the name
    of one or more layers separated by commas, or one or
    more wildcard patterns separated by commas.

    After entering the pattern, object snap is disabled for
    all entities that reside on any layer whose name matches
    the wildcard pattern. To re enable osnap for all layers,
    issue OSNAPLAYERS and enter a period (.).

    To see how OSNAPLAYERS works, open the included sample
    drawing (flatland.dwg), and issue the OSNAPLAYERS
    command. Enter "ROOF" at the prompt for the layer name
    pattern. Set running object snap to ENDPOINT and turn
    it on, then start the LINE command, and notice that
    no osnap markers appear when you move the cursor over
    the red roof geometry (which is on the layer "ROOF").

4.  The OSNAPOBJECTS command.

    The OSNAPOBJECTS command allows you to selectively disable
    running object snap for one or more entity types (such as 
    LINE, CIRCLE, HATCH, and so on). 
    
    To disable object snap for one or more types of entities, 
    enter the OSNAPOBJECTS command, then enter the name of one or 
    more entity types at the "Object type specification" prompt. 
    
    If you wish to specify more then one type of object, enter 
    the name of each separated by a comma, or you can enter a 
    wildcard pattern. For example entering the wildcard pattern 
    "AEC*" disables running object snap for all objects whose
    name begins with the characters "AEC".  
    
    Notes: 
    
		The OSNAPOBJECTS command has replaced the OSNAPHATCH 
		command. To disable running object snap for all hatch 
		line geometry, use the OSNAPOBJECTS command and include 
		the string 'HATCH' in the object type specification.

		To disable running object snap for old-style "heavy" 
		polylines, specify "VERTEX" as the object name.
	  
5.  The OSNAPGROUPS command.

	The OSNAPGROUPS command allows you to disable running object
	snap for all entities that are members of one or more groups.
	To disable running object snap for entities in one or more
	groups, issue the OSNAPGROUPS command and enter the name of
	a group, or a wildcard pattern that matches the name of one 
	or more groups. After doing this, entities that are members of
	any named group whose name matches the value entered will not
	be eligible for running object snap.
	
	This function can be applied only to named groups in the 
	current drawing. Unnamed groups or groups that are from 
	external references are ignored.
	
	To reenable	running object snap for all group members, enter 
	a period (.).
	
	The OSNAPGROUPS command also satisfies a more general desire
	to selectively disable object snap for individual entities.
	This is achived by simply creating a named, non-selectable
	group containing the entities that should not be elegible
	for object snap, and specifying the name of the group in the
	OSNAPGROUPS command.
	
6.	The OSNAPCMDS command.

	The OSNAPCMDS command allows you to disable running object
	snap while one or more specified commands are active. To
	disable running object snap for a given command or commands,
	issue OSNAPCMDS, and specify the name of the command or a
	wildcard character matching the name of one or more commands.
	
	When any of the commands specified are active, running object
	snap will be disabled.
	
 	General notes for all OSNAPXXXXX commands:
 	
 	Each document maintains its own distinct settings for all of
 	the above OSNAPXXXX commands. To set the default values used 
 	when a document is opened, use the OSNAPFILTER command's Set 
 	defaults option. 
 	
7.	The OSNAPBLOCKS command.

	The OSNAPBLOCKS command allows you to disable running object
	snap for all entities residing in one or more blocks. To 
	disable running object snap for one or more entities in a
	given block, issue the OSNAPBLOCKS command, and enter the
	name of the block, or a wildcard pattern matching the name
	of one or more blocks. OSNAPBLOCKS is useful when a drawing
	contains old-style hatching blocks (whose names always begin
	with "*X").  To disable running object snap for all old-style
	hatching blocks, use the block name specification "`*X*", as
	follows:
	
	   Command: OSNAPBLOCKS
	   Block name specification: `*X*
	   Command:
	
8.  The OSNAPFILTER command.

    This command consolidates all of the OSNAPxxxx commands
    described above into a single command. In addition, this
    command provides a means to set the default values for
    all OSNAPxxxxx commands, that are used when a drawing is
    first opened.
    
    The default values saved include the pattern for Layer;
    Group; Command; Block and Object type. To use the current 
    value of each as the default when a drawing is opened, use 
    the 'Set defaults' option of the OSNAPFILTER command. When 
    you do this, the current values for all filter patterns are
    saved in the registry, and will be used as the initial 
    values each time a drawing is opened. Remember to set the
    values for each of the five filter types prior to using
    the Set defaults option.
    
    These values are stored in the following registry location:
    
	HKEY_CURRENT_USER\Software\caddzone.com\FlatLand\Osnap Filters\

Questions, comments, flames, and bug reports should be
sent to tony.tanzillo@caddzone.com
