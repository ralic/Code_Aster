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
#include "asterf_types.h"
!
interface
    subroutine tresu_champ_cmp(chamgd, typtes, typres, nbref, tbtxt,&
                      refi, refr, refc, epsi, lign1,&
                      lign2, crit, ific, nbcmp, nocmp,&
                      llab, ssigne, ignore, compare)
        character(len=*), intent(in) :: chamgd
        character(len=8), intent(in) :: typtes
        character(len=*), intent(in) :: typres
        integer, intent(in) :: nbref
        character(len=16), intent(in) :: tbtxt(2)
        integer, intent(in) :: refi(nbref)
        real(kind=8), intent(in) :: refr(nbref)
        complex(kind=8), intent(in) :: refc(nbref)
        real(kind=8), intent(in) :: epsi
        character(len=200), intent(inout) :: lign1
        character(len=200), intent(inout) :: lign2
        character(len=*), intent(in) :: crit
        integer, intent(in) :: ific
        integer, intent(in) :: nbcmp
        character(len=8), intent(in) :: nocmp(*)
        aster_logical, intent(in) :: llab
        character(len=*), intent(in) :: ssigne
        aster_logical, intent(in), optional :: ignore
        real(kind=8), intent(in), optional :: compare
    end subroutine tresu_champ_cmp
end interface
