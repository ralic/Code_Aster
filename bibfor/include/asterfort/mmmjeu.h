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
    subroutine mmmjeu(ndim  ,jeusup,norm  ,geome ,geomm , &
                      ddeple,ddeplm,mprojt,jeu   ,djeu  , &
                      djeut ,iresog)
        integer :: ndim
        integer :: iresog
        real(kind=8) :: jeusup
        real(kind=8) :: norm(3)
        real(kind=8) :: geome(3)
        real(kind=8) :: geomm(3)
        real(kind=8) :: ddeple(3)
        real(kind=8) :: ddeplm(3)
        real(kind=8) :: mprojt(3, 3)
        real(kind=8) :: jeu
        real(kind=8) :: djeu(3)
        real(kind=8) :: djeut(3)
    end subroutine mmmjeu
end interface
