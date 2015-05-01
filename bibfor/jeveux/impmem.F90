subroutine impmem()
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
!     RENVOIE LA VALEUR EN MEGA OCTETS DE LA MEMOIRE UTILISEE PAR JEVEUX
!
! ======================================================================
#include "asterfort/assert.h"
#include "asterfort/r8inir.h"
#include "asterfort/utgtme.h"
#include "asterfort/utmess.h"
    real(kind=8) :: rval(4)
    character(len=8) :: k8tab(4)
    integer :: iret
!
    call r8inir(4, -1.d0, rval, 1)
    k8tab(1) = 'VMPEAK'
    k8tab(2) = 'VMSIZE'
    k8tab(3) = 'CMAX_JV'
    k8tab(4) = 'CMXU_JV'
    call utgtme(4, k8tab, rval, iret)
    if (iret .eq. 0) then
        if (rval(1) .gt. 0.d0) then
            call utmess('I', 'SUPERVIS2_77', nr=4, valr=rval)
        else
            call utmess('I', 'SUPERVIS2_78', nr=4, valr=rval)
        endif
    else
        ASSERT(.false.)
    endif
!
end subroutine
