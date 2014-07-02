!
! COPYRIGHT (C) 1991 - 2014  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
! person_in_charge: mathieu.courtois at edf.fr
!
#include "asterf_types.h"
!
interface
    subroutine tresu_print_all(refer, legend, llab, typres, nbref, &
                               rela, tole, ssigne, refr, valr, &
                               refi, vali, refc, valc, ignore, &
                               compare)
        implicit none
        character(len=16), intent(in) :: refer
        character(len=16), intent(in) :: legend
        aster_logical, intent(in) :: llab
        character(len=*), intent(in) :: typres
        integer, intent(in) :: nbref
        character(len=*), intent(in) :: rela
        real(kind=8), intent(in) :: tole
        character(len=*), intent(in) :: ssigne
        real(kind=8), intent(in) :: refr(nbref)
        real(kind=8), intent(in) :: valr
        integer, intent(in) :: refi(nbref)
        integer, intent(in) :: vali
        complex(kind=8), intent(in) :: refc(nbref)
        complex(kind=8), intent(in) :: valc
        aster_logical, intent(in), optional :: ignore
        real(kind=8), intent(in), optional :: compare
    end subroutine tresu_print_all
end interface
