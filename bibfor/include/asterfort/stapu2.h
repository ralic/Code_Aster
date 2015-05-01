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
    subroutine stapu2(nbobst, nbpt, nbpair, temps, fcho,&
                      vgli, dloc, coef, ang, wk1,&
                      wk2, wk3, wk4, wk5, wk6,&
                      idebut, nbloc, nbval, inoe, isupp,&
                      nbinst, tmpvu, pusurn, vustub, vusob,&
                      pus, pmoye, pourpu, poupre)
        integer :: nbinst
        integer :: nbpair
        integer :: nbobst
        integer :: nbpt
        real(kind=8) :: temps(*)
        real(kind=8) :: fcho(*)
        real(kind=8) :: vgli(*)
        real(kind=8) :: dloc(*)
        real(kind=8) :: coef(*)
        real(kind=8) :: ang(*)
        real(kind=8) :: wk1(*)
        real(kind=8) :: wk2(*)
        real(kind=8) :: wk3(*)
        real(kind=8) :: wk4(*)
        real(kind=8) :: wk5(*)
        real(kind=8) :: wk6(*)
        integer :: idebut
        integer :: nbloc
        integer :: nbval
        integer :: inoe
        integer :: isupp
        real(kind=8) :: tmpvu(*)
        real(kind=8) :: pusurn
        real(kind=8) :: vustub(nbpair, nbinst)
        real(kind=8) :: vusob(nbpair, nbinst)
        real(kind=8) :: pus(*)
        real(kind=8) :: pmoye
        real(kind=8) :: pourpu(*)
        real(kind=8) :: poupre(*)
    end subroutine stapu2
end interface
