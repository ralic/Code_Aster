!
! Definition of types used by med
!
#ifndef MED_TYPES_H
#define MED_TYPES_H
!
#include "asterf_config.h"
!
#define med_int_kind    MED_INT_SIZE
#define med_int         integer(kind=med_int_kind)
#define to_med_int(a)   int(a, MED_INT_SIZE)
!
#endif

