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
    subroutine hujcvg(nmat, mater, nvi, vind, vinf,&
                      vins, nr, yd, dy, r,&
                      indi, iter, itmax, intg, toler,&
                      bnews, mtrac, ye, lreli, iret)
        integer :: nr
        integer :: nvi
        integer :: nmat
        real(kind=8) :: mater(nmat, 2)
        real(kind=8) :: vind(nvi)
        real(kind=8) :: vinf(nvi)
        real(kind=8) :: vins(nvi)
        real(kind=8) :: yd(nr)
        real(kind=8) :: dy(nr)
        real(kind=8) :: r(nr)
        integer :: indi(7)
        integer :: iter
        integer :: itmax
        integer :: intg
        real(kind=8) :: toler
        logical :: bnews(3)
        logical :: mtrac
        real(kind=8) :: ye(nr)
        logical :: lreli
        integer :: iret
    end subroutine hujcvg
end interface
