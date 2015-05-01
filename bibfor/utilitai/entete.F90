subroutine entete()
! person_in_charge: mathieu.courtois at edf.fr
!
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
    implicit none
! ----------------------------------------------------------------------
!     ECRITURE DE L'ENTETE
! ----------------------------------------------------------------------
#include "asterf_types.h"
#include "asterf.h"
#include "asterc/asmpi_comm.h"
#include "asterc/lihdfv.h"
#include "asterc/limedv.h"
#include "asterc/liscov.h"
#include "asterc/mlnbpr.h"
#include "asterc/prhead.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/utmess.h"
    mpi_int :: rank, size
    integer :: vali(3)
!
#ifdef _USE_OPENMP
    integer :: nbth
#endif
! ----------------------------------------------------------------------
! --- INFORMATIONS GLOBALES
    call prhead(1)
! --- CONFIGURATION MPI
    call asmpi_info(rank=rank, size=size)
#ifdef _USE_MPI
    vali(1) = to_aster_int(rank)
    vali(2) = to_aster_int(size)
    call utmess('I', 'SUPERVIS2_11', ni=2, vali=vali)
#else
    call utmess('I', 'SUPERVIS2_12')
#endif
! --- CONFIGURATION OPENMP
#ifdef _USE_OPENMP
    nbth = mlnbpr()
    call utmess('I', 'SUPERVIS2_13', si=nbth)
#endif
! --- LIBRARIES HDF5 ET MED
#ifndef _DISABLE_HDF5
    call lihdfv(vali(1), vali(2), vali(3))
    call utmess('I', 'SUPERVIS2_14', ni=3, vali=vali)
#else
    call utmess('I', 'SUPERVIS2_15')
#endif
#ifndef _DISABLE_MED
    call limedv(vali(1), vali(2), vali(3))
    call utmess('I', 'SUPERVIS2_16', ni=3, vali=vali)
#else
    call utmess('I', 'SUPERVIS2_17')
#endif
! --- LIBRARIES SOLVEURS
! for backward compatibility
#ifdef _HAVE_MUMPS
#   ifndef ASTER_MUMPS_VERSION
#       define ASTER_MUMPS_VERSION MUMPS_VERSION
#   endif
!   to avoid C1510, use vers1
#   define vers1 ASTER_MUMPS_VERSION
    call utmess('I', 'SUPERVIS2_18', sk=vers1)
#else
    call utmess('I', 'SUPERVIS2_19')
#endif
#ifdef _HAVE_PETSC
!   to avoid C1510, use vers2
#   define vers2 ASTER_PETSC_VERSION
    call utmess('I', 'SUPERVIS2_25', sk=vers2)
#else
    call utmess('I', 'SUPERVIS2_26')
#endif
#ifndef _DISABLE_SCOTCH
    call liscov(vali(1), vali(2), vali(3))
    call utmess('I', 'SUPERVIS2_20', ni=3, vali=vali)
#else
    call utmess('I', 'SUPERVIS2_21')
#endif
!     SAUT DE LIGNE
    call utmess('I', 'VIDE_1')
end subroutine
