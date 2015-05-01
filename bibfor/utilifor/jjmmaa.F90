subroutine jjmmaa(ct, aut)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
#include "asterc/kloklo.h"
#include "asterfort/codent.h"
    character(len=12) :: aut
    character(len=4) :: ct(3)
    integer :: t(9)
!
!  ----------- FIN DECLARATIONS _____________
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    aut='INTERF_ST/TF'
!
!
    call kloklo(t)
    t(1)=t(2)
    t(2)=t(3)
    t(3)=t(4)
    if (t(1) .le. 9) then
        ct(1)='0   '
        call codent(t(1), 'G', ct(1)(2:2))
    else
        ct(1)='    '
        call codent(t(1), 'G', ct(1)(1:2))
    endif
    if (t(2) .le. 9) then
        ct(2)='0   '
        call codent(t(2), 'G', ct(2)(2:2))
    else
        ct(2)='    '
        call codent(t(2), 'G', ct(2)(1:2))
    endif
    ct(3)='    '
    call codent(t(3), 'G', ct(3))
!
end subroutine
