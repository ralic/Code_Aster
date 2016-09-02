!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine romLineicIndexSurf(nb1 , tab11, tab12,&
                                  nb2 , tab21, tab22,&
                                  tab3, epsi)
        integer, intent(in) :: nb1
        real(kind=8), intent(in) :: tab11(nb1)
        real(kind=8), intent(in) :: tab12(nb1)
        integer, intent(in) :: nb2
        real(kind=8), intent(in) :: tab21(nb2)
        real(kind=8), intent(in) :: tab22(nb2)
        integer, intent(out) :: tab3(nb1)
        real(kind=8), intent(in) :: epsi
    end subroutine romLineicIndexSurf
end interface
