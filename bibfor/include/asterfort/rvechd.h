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
    subroutine rvechd(dim, epsi, ssch19, nbcp, nbco,&
                      nbsp, ror, rex, ma1, ma2,&
                      for, fex, n, ptadr, val)
        character(len=2) :: dim
        real(kind=8) :: epsi
        character(len=19) :: ssch19
        integer :: nbcp
        integer :: nbco
        integer :: nbsp
        real(kind=8) :: ror(*)
        real(kind=8) :: rex(*)
        integer :: ma1(*)
        integer :: ma2(*)
        integer :: for(*)
        integer :: fex(*)
        integer :: n
        integer :: ptadr
        real(kind=8) :: val(*)
    end subroutine rvechd
end interface
