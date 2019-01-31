/*
 * recovery.H : provides utilities to recover information from the hogstat log,
 * detect potential faults, and to attempt to complete/recover operations
 */

#ifndef HOG_RECOVERY_H_INCLUDED
#define HOG_RECOVERY_H_INCLUDED

namespace hog {

void detectFaultAndRecover();

}

#endif

