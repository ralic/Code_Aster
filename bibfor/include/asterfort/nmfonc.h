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
#include "asterf_types.h"
!
interface
    subroutine nmfonc(parcri, parmet, method, solveu, modele,&
                      defico, lischa, lcont, lunil, sdnume,&
                      sddyna, sdcriq, mate, compoz, result,&
                      carcri, fonact)
        real(kind=8), intent(in) :: parcri(*)
        real(kind=8), intent(in) :: parmet(*)
        character(len=16), intent(in) :: method(*)
        character(len=19), intent(in) :: solveu
        character(len=24), intent(in) :: modele
        character(len=24), intent(in) :: defico
        character(len=19), intent(in) :: lischa
        aster_logical, intent(in) :: lcont
        aster_logical, intent(in) :: lunil
        character(len=19), intent(in) :: sdnume
        character(len=19), intent(in) :: sddyna
        character(len=24), intent(in) :: sdcriq
        character(len=24), intent(in) :: mate
        character(len=*), intent(in) :: compoz
        character(len=8), intent(in) :: result
        character(len=24), intent(in) :: carcri
        integer, intent(inout) :: fonact(*)
    end subroutine nmfonc
end interface
