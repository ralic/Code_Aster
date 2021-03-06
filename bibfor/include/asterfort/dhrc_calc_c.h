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
    subroutine dhrc_calc_c(c0, ac, gc,&
                      vint, c, cp1, cp2, cs1,&
                      cs2)
        real(kind=8), intent(in) :: c0(2, 2, 2)
        real(kind=8), intent(in) :: ac(2, 2, 2)
        real(kind=8), intent(in) :: gc(2, 2, 2)
        real(kind=8), intent(in) :: vint(*)
        real(kind=8), intent(out) :: c(2, 2, 2)
        real(kind=8), intent(out) :: cp1(2, 2)
        real(kind=8), intent(out) :: cp2(2, 2)
        real(kind=8), intent(out) :: cs1(2, 2)
        real(kind=8), intent(out) :: cs2(2, 2)
    end subroutine dhrc_calc_c
end interface 
