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
    subroutine xnewto(elrefp, name, n, ndime,&
                       ptxx, ndim, tabco, tabls,&
                       ipp, ip, itemax,&
                       epsmax, ksi, dekker)
        integer :: ndime
        integer :: ndim
        character(len=8) :: elrefp
        character(len=6) :: name
        integer :: n(3)
        real(kind=8) :: ptxx(*)
        real(kind=8) :: tabls(*)
        real(kind=8) :: tabco(*)
        integer :: ipp
        integer :: ip
        integer :: itemax
        real(kind=8) :: epsmax
        real(kind=8) :: ksi(ndime)
        integer, intent(in), optional :: dekker
    end subroutine xnewto
end interface
