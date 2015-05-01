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
    subroutine statch(nbobst, nbpt, temps, dloc, fcho,&
                      vgli, iadh, wk1, wk2, wk3,&
                      iwk4, tdebut, tfin, nbloc, offset,&
                      trepos, noecho, intitu, nomres)
        integer :: nbobst
        integer :: nbpt
        real(kind=8) :: temps(*)
        real(kind=8) :: dloc(*)
        real(kind=8) :: fcho(*)
        real(kind=8) :: vgli(*)
        integer :: iadh(*)
        real(kind=8) :: wk1(*)
        real(kind=8) :: wk2(*)
        real(kind=8) :: wk3(*)
        integer :: iwk4(*)
        real(kind=8) :: tdebut
        real(kind=8) :: tfin
        integer :: nbloc
        real(kind=8) :: offset
        real(kind=8) :: trepos
        character(len=8) :: noecho(*)
        character(len=8) :: intitu(*)
        character(len=*) :: nomres
    end subroutine statch
end interface
