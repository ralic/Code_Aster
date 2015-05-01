! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
!
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
! person_in_charge: mathieu.courtois@edf.fr
!
#ifndef ASTERF_DEBUG_H
#define ASTERF_DEBUG_H

#include "asterf_config.h"
!
! Here are defined some flags to add debugging informations.
!
! If the flag is defined, a function prints informations on stdout starting
! with the MARKER (to make grep easy).
! If the flag is not defined, the function must be empty macro.
!
! to add all traces, define __DEBUG_ALL__
#ifdef __DEBUG_ALL__
#   define __DEBUG_ALLOCATE__
#   define __DEBUG_MPI__
#   define __DEBUG_LOC__
#endif

! all prints should start with the same marker
#define MARKER "DEBUG: "

! trace AS_ALLOCATE / AS_DEALLOCATE
#ifdef __DEBUG_ALLOCATE__
#   define DEBUG_ALLOCATE(a, b, c) print *, MARKER, a, ':', b, c
#else
#   define DEBUG_ALLOCATE(a, b, c) continue
#endif

! trace MPI communications
#ifdef __DEBUG_MPI__
#   define DEBUG_MPI(a, b, c) print *, MARKER, a, ':', b, c
#else
#   define DEBUG_MPI(a, b, c) continue
#endif

! print localization
#ifdef __DEBUG_LOC__
#   define DEBUG_LOC(label, a, b) write(6,"(1X,A,A,'@',A,':',I4)") MARKER, label, a, b
#else
#   define DEBUG_LOC(label, a, b) continue
#endif

#endif
