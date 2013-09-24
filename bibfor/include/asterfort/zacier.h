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
    subroutine zacier(matos, nbhist, ftrc, trc, coef,&
                      fmod, ckm, nbtrc, tpg0, tpg1,&
                      tpg2, dt10, dt21, tamp, metapg)
        integer :: nbtrc
        integer :: nbhist
        integer :: matos
        real(kind=8) :: ftrc((3*nbhist), 3)
        real(kind=8) :: trc((3*nbhist), 5)
        real(kind=8) :: coef(*)
        real(kind=8) :: fmod(*)
        real(kind=8) :: ckm(6*nbtrc)
        real(kind=8) :: tpg0
        real(kind=8) :: tpg1
        real(kind=8) :: tpg2
        real(kind=8) :: dt10
        real(kind=8) :: dt21
        real(kind=8) :: tamp(7)
        real(kind=8) :: metapg(7)
    end subroutine zacier
end interface
