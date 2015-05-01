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
    subroutine zzcalb(igr, iel, npg, nno, wi,&
                      desc, sig, x, y, xmin,&
                      xmax, ymin, ymax, f)
        integer :: igr
        integer :: iel
        integer :: npg
        integer :: nno
        real(kind=8) :: wi(1)
        integer :: desc(1)
        real(kind=8) :: sig(1)
        real(kind=8) :: x(1)
        real(kind=8) :: y(1)
        real(kind=8) :: xmin
        real(kind=8) :: xmax
        real(kind=8) :: ymin
        real(kind=8) :: ymax
        real(kind=8) :: f(9, 4)
    end subroutine zzcalb
end interface
