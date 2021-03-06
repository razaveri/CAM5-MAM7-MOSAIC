#!/bin/csh -f
setenv CIMEROOT `./xmlquery CIMEROOT    -value`

./Tools/check_lockedfiles || exit -1

# NOTE - Are assumming that are already in $CASEROOT here
set CASE     = `./xmlquery CASE    -value`
set EXEROOT  = `./xmlquery EXEROOT -value`

# Reset all previous env_mach_pes settings
if ( -e env_mach_pes.xml.1 )  then
  cp -f env_mach_pes.xml.1 env_mach_pes.xml
else
  cp -f env_mach_pes.xml env_mach_pes.xml.1
endif
if ( -e env_build.xml.1 )  then
  cp -f env_build.xml.1 env_build.xml
else
  cp -f env_build.xml env_build.xml.1
endif

./case.setup -clean -testmode
./case.setup 

#-----------------------------------------------------
# Build with default PE layout
#-----------------------------------------------------

./xmlchange -file env_build.xml -id SMP_BUILD -val 0

./case.build -testmode
if ($status != 0) then
   echo "Error: build for default PE layout failed" >! ./TestStatus
   echo "CFAIL $CASE" > ./TestStatus
   exit -1    
endif 

mv -f $EXEROOT/cesm.exe $EXEROOT/cesm.exe.1
cp -f env_build.xml    env_build.xml.1

#-----------------------------------------------------
# Halve the number of tasks and threads and ROOTPE and build again,
#-----------------------------------------------------

set NTASKS_ATM  = `./xmlquery NTASKS_ATM        -value`
set NTASKS_LND  = `./xmlquery NTASKS_LND        -value`
set NTASKS_ROF  = `./xmlquery NTASKS_ROF        -value`
set NTASKS_WAV  = `./xmlquery NTASKS_WAV        -value`
set NTASKS_OCN  = `./xmlquery NTASKS_OCN        -value`
set NTASKS_ICE  = `./xmlquery NTASKS_ICE        -value`
set NTASKS_GLC  = `./xmlquery NTASKS_GLC        -value`
set NTASKS_ESP  = `./xmlquery NTASKS_ESP        -value`
set NTASKS_CPL  = `./xmlquery NTASKS_CPL        -value`

set NTHRDS_ATM  = `./xmlquery NTHRDS_ATM        -value`
set NTHRDS_LND  = `./xmlquery NTHRDS_LND        -value`
set NTHRDS_ROF  = `./xmlquery NTHRDS_ROF        -value`
set NTHRDS_WAV  = `./xmlquery NTHRDS_WAV        -value`
set NTHRDS_OCN  = `./xmlquery NTHRDS_OCN        -value`
set NTHRDS_ICE  = `./xmlquery NTHRDS_ICE        -value`
set NTHRDS_GLC  = `./xmlquery NTHRDS_GLC        -value`
set NTHRDS_ESP  = `./xmlquery NTHRDS_ESP        -value`
set NTHRDS_CPL  = `./xmlquery NTHRDS_CPL        -value`

set ROOTPE_ATM  = `./xmlquery ROOTPE_ATM        -value`
set ROOTPE_LND  = `./xmlquery ROOTPE_LND        -value`
set ROOTPE_ROF  = `./xmlquery ROOTPE_ROF        -value`
set ROOTPE_WAV  = `./xmlquery ROOTPE_WAV        -value`
set ROOTPE_OCN  = `./xmlquery ROOTPE_OCN        -value`
set ROOTPE_ICE  = `./xmlquery ROOTPE_ICE        -value`
set ROOTPE_GLC  = `./xmlquery ROOTPE_GLC        -value`
set ROOTPE_ESP  = `./xmlquery ROOTPE_ESP        -value`
set ROOTPE_CPL  = `./xmlquery ROOTPE_CPL        -value`

if ( $NTHRDS_ATM > 1 || $NTHRDS_LND > 1 || $NTHRDS_ROF > 1 || $NTHRDS_WAV > 1 || $NTHRDS_OCN > 1 || $NTHRDS_ICE > 1 || $NTHRDS_GLC > 1 || $NTHRDS_CPL > 1 ) then
  ./xmlchange -file env_build.xml -id BUILD_THREADED  -val TRUE
endif

# Halve the number of tasks 
if ( $NTASKS_ATM > 1 ) then
  @ ntask = $NTASKS_ATM / 2
  ./xmlchange -file env_mach_pes.xml -id NTASKS_ATM  -val $ntask
endif
if ( $NTASKS_LND > 1 ) then
  @ ntask = $NTASKS_LND / 2
  ./xmlchange -file env_mach_pes.xml -id NTASKS_LND  -val $ntask
endif
if ( $NTASKS_ROF > 1 ) then
  @ ntask = $NTASKS_ROF / 2
  ./xmlchange -file env_mach_pes.xml -id NTASKS_ROF  -val $ntask
endif
if ( $NTASKS_WAV > 1 ) then
  @ ntask = $NTASKS_WAV / 2
  ./xmlchange -file env_mach_pes.xml -id NTASKS_WAV  -val $ntask
endif
if ( $NTASKS_OCN > 1 ) then
  @ ntask = $NTASKS_OCN / 2
  ./xmlchange -file env_mach_pes.xml -id NTASKS_OCN  -val $ntask
endif
if ( $NTASKS_ICE > 1 ) then
  @ ntask = $NTASKS_ICE / 2
  ./xmlchange -file env_mach_pes.xml -id NTASKS_ICE  -val $ntask
endif
if ( $NTASKS_GLC > 1 ) then
  @ ntask = $NTASKS_GLC / 2
  ./xmlchange -file env_mach_pes.xml -id NTASKS_GLC  -val $ntask
endif
if ( $NTASKS_ESP > 1 ) then
  @ ntask = $NTASKS_ESP / 2
  ./xmlchange -file env_mach_pes.xml -id NTASKS_ESP  -val $ntask
endif
if ( $NTASKS_CPL > 1 ) then
  @ ntask = $NTASKS_CPL / 2
  ./xmlchange -file env_mach_pes.xml -id NTASKS_CPL  -val $ntask
endif

# Halve the rootpe
  @ rootpe = $ROOTPE_ATM / 2
  ./xmlchange -file env_mach_pes.xml -id ROOTPE_ATM  -val $rootpe
  @ rootpe = $ROOTPE_LND / 2
  ./xmlchange -file env_mach_pes.xml -id ROOTPE_LND  -val $rootpe
  @ rootpe = $ROOTPE_ROF / 2
  ./xmlchange -file env_mach_pes.xml -id ROOTPE_ROF  -val $rootpe
  @ rootpe = $ROOTPE_WAV / 2
  ./xmlchange -file env_mach_pes.xml -id ROOTPE_WAV  -val $rootpe
  @ rootpe = $ROOTPE_OCN / 2
  ./xmlchange -file env_mach_pes.xml -id ROOTPE_OCN  -val $rootpe
  @ rootpe = $ROOTPE_ICE / 2
  ./xmlchange -file env_mach_pes.xml -id ROOTPE_ICE  -val $rootpe
  @ rootpe = $ROOTPE_GLC / 2
  ./xmlchange -file env_mach_pes.xml -id ROOTPE_GLC  -val $rootpe
  @ rootpe = $ROOTPE_ESP / 2
  ./xmlchange -file env_mach_pes.xml -id ROOTPE_ESP  -val $rootpe
  @ rootpe = $ROOTPE_CPL / 2
  ./xmlchange -file env_mach_pes.xml -id ROOTPE_CPL  -val $rootpe


# Halve the number of threads
if ( $NTHRDS_ATM > 1 ) then
  @ nthrd = $NTHRDS_ATM / 2
  ./xmlchange -file env_mach_pes.xml -id NTHRDS_ATM  -val $nthrd
endif
if ( $NTHRDS_LND > 1 ) then
  @ nthrd = $NTHRDS_LND / 2
  ./xmlchange -file env_mach_pes.xml -id NTHRDS_LND  -val $nthrd
endif
if ( $NTHRDS_ROF > 1 ) then
  @ nthrd = $NTHRDS_ROF / 2
  ./xmlchange -file env_mach_pes.xml -id NTHRDS_ROF  -val $nthrd
endif
if ( $NTHRDS_WAV > 1 ) then
  @ nthrd = $NTHRDS_WAV / 2
  ./xmlchange -file env_mach_pes.xml -id NTHRDS_WAV  -val $nthrd
endif
if ( $NTHRDS_OCN > 1 ) then
  @ nthrd = $NTHRDS_OCN / 2
  ./xmlchange -file env_mach_pes.xml -id NTHRDS_OCN  -val $nthrd
endif
if ( $NTHRDS_ICE > 1 ) then
  @ nthrd = $NTHRDS_ICE / 2
  ./xmlchange -file env_mach_pes.xml -id NTHRDS_ICE  -val $nthrd
endif
if ( $NTHRDS_GLC > 1 ) then
  @ nthrd = $NTHRDS_GLC / 2
  ./xmlchange -file env_mach_pes.xml -id NTHRDS_GLC  -val $nthrd
endif
if ( $NTHRDS_ESP > 1 ) then
  @ nthrd = $NTHRDS_ESP / 2
  ./xmlchange -file env_mach_pes.xml -id NTHRDS_ESP  -val $nthrd
endif
if ( $NTHRDS_CPL > 1 ) then
  @ nthrd = $NTHRDS_CPL / 2
  ./xmlchange -file env_mach_pes.xml -id NTHRDS_CPL  -val $nthrd
endif

# Build with half the tasks and threads
./case.setup -clean -testmode
./case.setup

./xmlchange -file env_build.xml -id SMP_BUILD -val 0

./case.build -testmode
if ($status != 0) then
   echo "Error: build for half tasks/threads failed" >! ./TestStatus
   echo "CFAIL $CASE" > ./TestStatus
   exit -1    
endif 

mv -f $EXEROOT/cesm.exe $EXEROOT/cesm.exe.2
cp -f env_build.xml env_build.xml.2
cp -f env_mach_pes.xml env_mach_pes.xml.2
#
# Because mira/cetus interprets its run script differently than
# other systems we need to copy the original env_mach_pes.xml
# back 
#
cp -f env_mach_pes.xml.1 env_mach_pes.xml
cp -f env_mach_pes.xml LockedFiles/env_mach_pes.xml
