!
! Definition of types used by aster
!
#ifndef ASTER_TYPES_H
#define ASTER_TYPES_H
!
#include "asterf_config.h"
!
#define aster_int_kind  ASTER_INT_SIZE
#define aster_int       integer(kind=aster_int_kind)
#define to_aster_int(a) int(a, ASTER_INT_SIZE)
!
#endif

