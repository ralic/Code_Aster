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
    subroutine aceat3(noma, nomu, nbtuy, nbpart, nbmap,&
                      elpar, nopar, ivr, ifm, nbzk,&
                      nozk, cozk, isens, coor, epsi,&
                      crit, nno, nmmt)
        integer :: nno
        integer :: nbzk
        integer :: nbpart
        integer :: nbtuy
        character(len=8) :: noma
        character(len=8) :: nomu
        integer :: nbmap(nbpart)
        integer :: elpar(nbpart, nbtuy)
        integer :: nopar(nbpart, nno, nbtuy)
        integer :: ivr(3)
        integer :: ifm
        integer :: nozk(nbzk)
        real(kind=8) :: cozk(3*nbzk)
        integer :: isens(nbpart)
        real(kind=8) :: coor(*)
        real(kind=8) :: epsi
        character(len=8) :: crit
        integer :: nmmt(*)
    end subroutine aceat3
end interface
