subroutine comp_meca_exc2(l_cristal, l_prot_comp,&
                          l_excl   , vari_excl)
!
implicit none
!
#include "asterf_types.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    aster_logical, intent(in) :: l_cristal
    aster_logical, intent(in) :: l_prot_comp
    aster_logical, intent(out) :: l_excl
    character(len=16), intent(out) :: vari_excl
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Exception for name of internal variables
!
! --------------------------------------------------------------------------------------------------
!
! In  l_cristal        : .true. if *CRISTAL comportment
! In  l_prot_comp      : .true. if external computing for comportment (MFront, UMAT)
! Out l_excl           : .true. if exception case (no names for internal variables)
! Out vari_excl        : name of internal variables if l_excl
!
! --------------------------------------------------------------------------------------------------
!
    l_excl    = .false.
    vari_excl = ' '
!
! - Multiple comportment
!
    if (l_cristal) then
        l_excl    = .true.
        vari_excl = '&&MULT_COMP'
    endif
!
! - External comportment
!
    if (l_prot_comp) then
        l_excl    = .true.
        vari_excl = '&&PROT_COMP'
    endif
!
end subroutine
