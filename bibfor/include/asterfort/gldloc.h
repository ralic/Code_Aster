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
interface
    subroutine gldloc(lambda, deuxmu, deumuf, seuil, alf,&
                      alfmc, gmt, gmc, gf, cof1,&
                      vim, q2d, qff, tr2d, eps33,&
                      de33d1, de33d2, ksi2d, dksi1, dksi2,&
                      da1, da2, kdmax, told, codret,&
                      emp)
        real(kind=8) :: lambda
        real(kind=8) :: deuxmu
        real(kind=8) :: deumuf
        real(kind=8) :: seuil
        real(kind=8) :: alf
        real(kind=8) :: alfmc
        real(kind=8) :: gmt
        real(kind=8) :: gmc
        real(kind=8) :: gf
        real(kind=8) :: cof1(2)
        real(kind=8) :: vim(*)
        real(kind=8) :: q2d(2)
        real(kind=8) :: qff(2)
        real(kind=8) :: tr2d
        real(kind=8) :: eps33
        real(kind=8) :: de33d1
        real(kind=8) :: de33d2
        real(kind=8) :: ksi2d
        real(kind=8) :: dksi1
        real(kind=8) :: dksi2
        real(kind=8) :: da1
        real(kind=8) :: da2
        integer :: kdmax
        real(kind=8) :: told
        integer :: codret
        real(kind=8) :: emp(2)
    end subroutine gldloc
end interface
