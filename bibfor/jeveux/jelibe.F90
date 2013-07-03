subroutine jelibe(nomlu)
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "asterfort/jjlide.h"
#include "asterfort/jjvern.h"
#include "asterfort/u2mesk.h"
    character(len=*) :: nomlu
!     ==================================================================
    character(len=6) :: pgma
    common /kappje/  pgma
!     ==================================================================
    character(len=32) :: noml32
    integer :: icre, iret
!     ==================================================================
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    pgma = 'JELIBE'
    if (len(nomlu) .le. 0) then
        call u2mesk('F', 'JEVEUX1_08', 1, nomlu)
    endif
    noml32 = nomlu(1:min(32,len(nomlu)))
!
    icre = 0
    call jjvern(noml32, icre, iret)
!
    if (iret .eq. 0) then
        call u2mesk('F', 'JEVEUX_26', 1, noml32(1:24))
    else
        call jjlide('JELIBE', noml32, iret)
    endif
end subroutine
