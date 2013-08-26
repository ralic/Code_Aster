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
    subroutine extrac(interp, prec, crit, nbinst, ti,&
                      temps, y, neq, xtract, ier)
        character(*), intent(in) :: interp
        real(kind=8), intent(in) :: prec
        character(*), intent(in) :: crit
        integer, intent(in) :: nbinst
        real(kind=8), intent(in) :: ti(*)
        real(kind=8), intent(in) :: temps
        integer, intent(in) :: neq
        real(kind=8), intent(in) :: y(nbinst*neq)
        real(kind=8), intent(out) :: xtract(neq)
        integer, intent(out) :: ier
    end subroutine extrac
end interface
