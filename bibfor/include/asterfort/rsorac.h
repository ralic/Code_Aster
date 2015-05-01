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
    subroutine rsorac(nomsd, acces, ival, rval, kval,&
                      cval, epsi, crit, nutrou, ndim,&
                      nbtrou)
        character(len=*), intent(in) :: nomsd
        character(len=*), intent(in) :: acces
        integer, intent(in) :: ival
        real(kind=8), intent(in) :: rval
        character(len=*), intent(in) :: kval
        complex(kind=8), intent(in) :: cval
        real(kind=8), intent(in) :: epsi
        character(len=*), intent(in) :: crit
        integer, intent(out) :: nutrou(*)
        integer, intent(in) :: ndim
        integer, intent(out) :: nbtrou
    end subroutine rsorac
end interface
