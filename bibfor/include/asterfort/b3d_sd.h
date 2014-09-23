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
interface 
    subroutine b3d_sd(ss6, t33, n33, l3, vt33,&
                      young0, xnu0, gf0, fr, rt,&
                      epic, beta1, gama1, reg, erreur,&
                      d03, dt3, st3, vss33, vss33t,&
                      local, e23, nfid1, rrr, rapp6,&
                      dpic0, istep)
#include "asterf_types.h"
        real(kind=8) :: ss6(6)
        real(kind=8) :: t33(3, 3)
        real(kind=8) :: n33(3, 3)
        real(kind=8) :: l3(3)
        real(kind=8) :: vt33(3, 3)
        real(kind=8) :: young0
        real(kind=8) :: xnu0
        real(kind=8) :: gf0
        real(kind=8) :: fr
        real(kind=8) :: rt
        real(kind=8) :: epic
        real(kind=8) :: beta1
        real(kind=8) :: gama1
        real(kind=8) :: reg
        integer :: erreur
        real(kind=8) :: d03(3)
        real(kind=8) :: dt3(3)
        real(kind=8) :: st3(3)
        real(kind=8) :: vss33(3, 3)
        real(kind=8) :: vss33t(3, 3)
        aster_logical :: local
        real(kind=8) :: e23(3)
        real(kind=8) :: nfid1
        aster_logical :: rrr
        real(kind=8) :: rapp6(6)
        real(kind=8) :: dpic0
        integer :: istep
    end subroutine b3d_sd
end interface 
