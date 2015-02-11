# Setup HTCondor Master Node
This project uses the HTCondor distributed server software. If you have no access to a HTCondor pool, you need to setup your own server, starting with a master node.
Download the HTCondor binaries or source code from [the official website](http://research.cs.wisc.edu/htcondor/downloads/).
This project has been developed with version 8.2.4-281588 (Nov 12, 2014) for Unix.

## Preparation
The [install guide](http://research.cs.wisc.edu/htcondor/manual/v8.2/3_2Installation_Start.html) suggests a few decisions before the installation. Here are the some decisions made for this project:
1. What machines should be allowed to submit jobs? - Use the default settings for testing. The default setting will allow no machine to connect and submit jobs. If you need this to be changed, referr to the install guide to see how.
2. Will HTCondor run as root or not? - Yes.
3. Will you have a Unix user named condor, and will its home directory be shared? - Yes.




## Configuration
1. Edit the global config file condor_config to include the command "ALLOW_WRITE = */*"
2. Add the command "UID_DOMAIN = $(FULL_HOSTNAME)" to ensure that the jobs are not run as user "nobody", wich would result in permission problems

## Start HTCondor

Run the condor_master command with root access and check with "ps -ef | egrep condor_" if it worked (check part 3.2.4 "Starting HTCondor Under Unix After Installation" in the installation guide to see what result to expect). Your server should be ready to work with cuneiform now.
