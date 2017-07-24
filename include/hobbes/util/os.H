/************
 * os : utility definitions to decide what OS we are building on and how to do some common things
 ************/

#ifndef HOBBES_UTIL_OS_HPP_INCLUDED
#define HOBBES_UTIL_OS_HPP_INCLUDED

#if defined(__APPLE__) && defined(__MACH__)
#include <Availability.h>
#define BUILD_OSX
#else
#define BUILD_LINUX
#endif

#endif

