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
    subroutine xmifis(ndim, ndime, elrefp, geom, lsn, &
                  n, ip1, ip2, pinref, miref, mifis, &
                  pintt, exit, jonc, u, v)
        integer :: ndime
        integer :: ndim
        integer :: n(3)
        character(len=8) :: elrefp
        real(kind=8) :: geom(*)
        real(kind=8) :: lsn(*)
        integer :: ip1
        integer :: ip2
        real(kind=8) :: pinref(*)
        real(kind=8) :: miref(ndime)
        real(kind=8) :: mifis(ndim)
        real(kind=8) :: pintt(*)
        aster_logical :: jonc
        integer :: exit(2)
        real(kind=8), intent(in), optional :: u(ndime)
        real(kind=8), intent(in), optional :: v(ndime)
    end subroutine xmifis
end interface
