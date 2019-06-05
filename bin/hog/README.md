# Hog - Hobbes-based logging facilities

## Automatic Fault Detection & Recovery

Automatic fault detection and recovery features are available in Hog by default.
At the moment only batchsend mode recovery is supported. These features are
enabled by default, are fully automated, and can be disabled via the 
`--no-recovery` flag. The main goal of the feature is to allow automatic fault
recovery within batch send mode with as little blocking of resumption of normal operations 
as is possible without compromising data ordering guarantees.


NOTE: Recovery will not work if the following conditions aren't present

 * Shared memory regions must still be available (not persistent across system restarts).
 * Hog statistics log (e.g. hogstat.db) must be accessible in the specified directory.

### Abstract Recovery Process

The process works as follows:
 
 * The Hog batchsend application starts.
 * The statistics log file is scanned to see if any shared memory regions &
   destinations didn't have their data sent to an associated receieve Hog
   session before terminating unexpectedly.
 * The information for any such "faulty" sessions is collected.
 * The new Hog instance attempts to (in chronological order) start of these
   sessions. Recovery can only complete for any given shared memory
   region/destination once, and will only be classed as completed when all data
   has been successfully sent to all required destinations.
 * Once recovery has started, blocking on draining shared memory regions as
   necessary, sending logic is left to complete concurrently, and the Hog
   instance continues to run given the options specified on the command line.
 * Operations as normal.

Sending of data happens in parallel to normal operation, but ordering is 
maintained correctly.

Following are some potential failure scenarios that (with this feature) Hog can
handle appropriately:

### Scenario - Send Failure

 * Batch receive, batch send and logging process all start.
 * Send connects to logging process and begins draining shm to disk.
 * Send connects to receive and begins sending data segments over the network.
 * Send terminates unexpectedly.
 * Send is restarted with the same parameters.
 * Recovery kicks in, starts draining the previous shared memory
   region to disk, and also starts concurrently sending data to the receive
   instance. 
 * Once the reading process catches up to the end of log, it terminates, and
   then Hog resumes normal operation, connecting with the logging process again.
 * The sending process continues as normal.

### Scenario - Engine Failure

 * Batch receive, batch send and logging process all start.
 * Send connects to logging process and begins draining shm to disk.
 * Send connects to receive and begins sending data segments over the network.
 * Logging process terminates unexpectedly.
 * Logging process is restarted.
 * Recovery doesn't need to kick in - send appropriately renegotiates a
   connection with the loggin process and reads shared memory and sends that
   information over the network to receive as expected.

### Scenario - Receive Failure

 * Batch receive, batch send and logging process all start.
 * Send connects to logging process and begins draining shared memory to disk.
 * Send connects to receive and begins sending data segments over the network.
 * Send terminates unexpectedly.
 * Send is restarted with the same parameters.
 * Recovery doesn't need to kick in - send appropriately continues reading the
   shared memory region to disk, reconnecting to batch recv where possible.
   If send terminates before doing so, this session will be automatically
   recovered upon future attempts at running Hog in send mode.


### Scenario - Logging Process & Send Failure

 * Batch receive, batch send and logging process all start.
 * Send connects to logging process and begins draining shm to disk.
 * Send connects to receive and begins sending data segments over the network.
 * Send terminates unexpectedly.
 * Logging process terminates unexpectedly.
 * Send is restarted with the same parameters.
 * Logging process is restarted.
 * Recovery kicks in, starts draining the previous shared memory
   region to disk, and also starts concurrently sending data to the receive
   instance. 
 * Once the reading process catches up to the end of log, it terminates, and
   then Hog resumes normal operation, connecting with the logging process again.
 * The sending process continues as normal.

