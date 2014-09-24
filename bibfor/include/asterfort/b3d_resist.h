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
interface 
    subroutine b3d_resist(ssr6, rap6, t33, n33, vt33,&
                          local, rt, beta1, epic, fr,&
                          gf, young0, xnu0, reg, src3,&
                          srt3, vrap33, vrap33t)
#include "asterf_types.h"
        real(kind=8) :: ssr6(6)
        real(kind=8) :: rap6(6)
        real(kind=8) :: t33(3, 3)
        real(kind=8) :: n33(3, 3)
        real(kind=8) :: vt33(3, 3)
        aster_logical :: local
        real(kind=8) :: rt
        real(kind=8) :: beta1
        real(kind=8) :: epic
        real(kind=8) :: fr
        real(kind=8) :: gf
        real(kind=8) :: young0
        real(kind=8) :: xnu0
        real(kind=8) :: reg
        real(kind=8) :: src3(3)
        real(kind=8) :: srt3(3)
        real(kind=8) :: vrap33(3, 3)
        real(kind=8) :: vrap33t(3, 3)
    end subroutine b3d_resist
end interface 
