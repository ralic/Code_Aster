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
    subroutine cntmat(lambda, deuxmu, lamf, deumuf, alf,&
                      alfmc, emp, efp, eps, vmp,&
                      vfp, tr2d, trot, treps, gmt,&
                      gmc, gf, da1, da2, ksi2d,&
                      qff, cof1, q2d, de33d1, de33d2,&
                      elas, elas1, elas2, coup, rigi,&
                      resi, option, dsidep, sig, cof2,&
                      dq2d)
        real(kind=8) :: lambda
        real(kind=8) :: deuxmu
        real(kind=8) :: lamf
        real(kind=8) :: deumuf
        real(kind=8) :: alf
        real(kind=8) :: alfmc
        real(kind=8) :: emp(2)
        real(kind=8) :: efp(2)
        real(kind=8) :: eps(2)
        real(kind=8) :: vmp(2, 2)
        real(kind=8) :: vfp(2, 2)
        real(kind=8) :: tr2d
        real(kind=8) :: trot
        real(kind=8) :: treps
        real(kind=8) :: gmt
        real(kind=8) :: gmc
        real(kind=8) :: gf
        real(kind=8) :: da1
        real(kind=8) :: da2
        real(kind=8) :: ksi2d
        real(kind=8) :: qff(2)
        real(kind=8) :: cof1(2)
        real(kind=8) :: q2d(2)
        real(kind=8) :: de33d1
        real(kind=8) :: de33d2
        aster_logical :: elas
        aster_logical :: elas1
        aster_logical :: elas2
        aster_logical :: coup
        aster_logical :: rigi
        aster_logical :: resi
        character(len=16) :: option
        real(kind=8) :: dsidep(6, 6)
        real(kind=8) :: sig(6)
        real(kind=8) :: cof2(2)
        real(kind=8) :: dq2d(2)
    end subroutine cntmat
end interface
