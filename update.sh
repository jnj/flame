#!/bin/sh

SRCDIR=classes.cs.uchicago.edu:/stage/classes/current/CS326/
DSTDIR=$HOME/flame

# Copy all the class stuff into my home
for i in Tests Tools; do \
    echo "Copying $i/* to $DSTDIR..."; \
    scp -r $SRCDIR/$i $DSTDIR; \
done

# Copy needed files
echo "Updating $DSTDIR/plex.py..."
scp $DSTDIR/Tools/plex.py $DSTDIR
echo "Updating $DSTDIR/pyson.py..."
scp $DSTDIR/Tools/pyson.py $DSTDIR

# Force a rebuild of parsing tables
echo "Removing .pyc and parse tables..."
rm -f $DSTDIR/parsetab.* $DSTDIR/*.pyc



