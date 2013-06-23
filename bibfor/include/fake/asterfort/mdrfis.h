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
    subroutine mdrfis(nbmode, depgen, fexgen, nbnli, nbrfis,&
                      dplmod, fk, dfk, parcho, angini,&
                      vrotat, foncp, temps)
        integer :: nbnli
        integer :: nbmode
        real(kind=8) :: depgen(*)
        real(kind=8) :: fexgen(*)
        integer :: nbrfis
        real(kind=8) :: dplmod(nbnli, nbmode, *)
        character(len=8) :: fk(2)
        character(len=8) :: dfk(2)
        real(kind=8) :: parcho(nbnli, *)
        real(kind=8) :: angini
        real(kind=8) :: vrotat
        character(len=8) :: foncp
        real(kind=8) :: temps
    end subroutine mdrfis
end interface
