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
    subroutine uthk(nomte, geom, hk, ndim, niv,&
                    noe, nsomm, tymvol, ifa)
        integer, parameter :: l1=9, l2=6, l3=4
        character(len=16), intent(in) :: nomte
        real(kind=8), intent(in) :: geom(*)
        real(kind=8), intent(out) :: hk
        integer, intent(in) :: ndim
        integer, intent(in) :: niv
        integer, intent(in), optional :: noe(l1*l2*l3)
        integer, intent(in), optional :: nsomm
        integer, intent(in), optional :: tymvol
        integer, intent(in), optional :: ifa
    end subroutine uthk
end interface
