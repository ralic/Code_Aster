function gtstat(istat)
! person_in_charge: mathieu.courtois at edf.fr
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
#include "asterf_debug.h"
#include "asterf_types.h"
#include "asterf_constant.h"
    aster_logical :: gtstat
!     ARGUMENT IN
    integer :: istat
!-----------------------------------------------------------------------
!     FONCTION "GeT STATus" : DIT SI LE STATUT ISTAT EST ACTUELLEMENT
!     ACTIVE OU NON.
!
!     LA VALEUR DU STATUT GLOBAL IGLBST EST STOCKE DANS LE COMMON CGLBST
!-----------------------------------------------------------------------
    integer :: iglbst
    common  / cglbst / iglbst
!
    if (istat .eq. ST_OK) then
        gtstat = iglbst .eq. ST_OK
    else
        gtstat = iand(istat, iglbst) .eq. istat
    endif
    DEBUG_MPI('get status: in/returned', istat, gtstat)
end function
