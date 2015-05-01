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
    subroutine lkdgde(val, vintr, dt, seuive, ucrim,&
                      im, sm, vinm, nbmat, mater,&
                      depsv, dgamv, retcom)
        integer :: nbmat
        integer :: val
        real(kind=8) :: vintr
        real(kind=8) :: dt
        real(kind=8) :: seuive
        real(kind=8) :: ucrim
        real(kind=8) :: im
        real(kind=8) :: sm(6)
        real(kind=8) :: vinm(7)
        real(kind=8) :: mater(nbmat, 2)
        real(kind=8) :: depsv(6)
        real(kind=8) :: dgamv
        integer :: retcom
    end subroutine lkdgde
end interface
