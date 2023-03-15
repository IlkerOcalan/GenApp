# This is a sample .profile defining login environment parameters for
# an individual user. This should be copied to the user's $HOME/.profile
# and modified to their individual taste. More information may be
# found in the MVS/ESA OpenEdition User's Guide, in the chapter on
# customizing the shell.
#
# You may add or remove a # to disable or enable the settings below.

# The following variable may be set to point to a login script.
# In this case it is set to a file called .setup in the home
# directory. Refer to the User's Guide for more information on how to
# use this variable.

# ENV=$HOME/.setup
# export ENV

# This line appends your home directory to the current path.
PATH=$PATH:$HOME:

# This line sets the default editor to ed.
EDITOR=ed

# This line sets the prompt to display your login name, and current
# directory.
# added nice PS1 to know where you are, who you are, and what your
# current directory is
export HOSTNAME=$(uname -n)
export PS1='$LOGNAME @ $HOSTNAME:$PWD>'

# This line exports the variable settings so that they are known to the
# system.
export PATH EDITOR

# Set Timezone
TZ=EST5EDT

# Run all SPAWN'd processes in the same address space
# export _BPX_SHAREAS="YES"
export _BPX_SHAREAS="NO"

# Ihs LIBPATH
export STEPLIB=CICSTS41.CICS.SDFHEXCI:CICSTS41.CICS.SDFHLOAD
export JAVA_HOME=/usr/lpp/java/J8.0_64
export PATH=/usr/lpp/java/J8.0_64/bin:$PATH

# dbb

export DBB_HOME=/usr/lpp/IBM/dbbv2
export DBB_CONF=/usr/lpp/IBM/dbbv2/conf
export PATH=/usr/lpp/IBM/dbbv2/bin:$PATH

export PATH=/usr/lpp/Rocket/rsusr/ported/bin:$PATH
