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
#include "asterf_types.h"
!
interface
    subroutine lcafyd(comp, materd, materf, nbcomm, cpmono,&
                      nmat, mod, nvi, vind, vinf,&
                      sigd, nr, yd, bnews, mtrac)
        integer :: nvi
        integer :: nmat
        character(len=16) :: comp(*)
        real(kind=8) :: materd(nmat, 2)
        real(kind=8) :: materf(nmat, 2)
        integer :: nbcomm(nmat, 3)
        character(len=24) :: cpmono(5*nmat+1)
        character(len=8) :: mod
        real(kind=8) :: vind(*)
        real(kind=8) :: vinf(nvi)
        real(kind=8) :: sigd(6)
        integer :: nr
        real(kind=8) :: yd(*)
        aster_logical :: bnews(3)
        aster_logical :: mtrac
    end subroutine lcafyd
end interface
