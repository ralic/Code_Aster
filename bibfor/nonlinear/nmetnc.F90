subroutine nmetnc(field_name_algo, field_algo)
!
implicit none
!
#include "asterfort/assert.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=*), intent(in) :: field_name_algo
    character(len=*), intent(out) :: field_algo
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Input/output datastructure
!
! Get name of datastructure for field
!
! This utiliy is required for "hat" variables
!
! --------------------------------------------------------------------------------------------------
!
! In  field_name_algo : name of field in algorithme
! Out field_algo      : name of datastructure for field
!
! --------------------------------------------------------------------------------------------------
!
    character(len=6) :: hat_type, hat_vari
!
! --------------------------------------------------------------------------------------------------
!
    field_algo = ' '
!
    if (field_name_algo(1:5) .eq. 'CHAP#') then
        hat_type = field_name_algo(6:11)
        hat_vari = field_name_algo(13:18)
        if (hat_type .eq. 'VALINC') then
            if (hat_vari .eq. 'TEMP') then
                field_algo = '&&NXLECTVAR_____'
            else
                field_algo = '&&NMCH1P.'//hat_vari
            endif
        else if (hat_type.eq.'VEASSE') then
            field_algo = '&&NMCH5P.'//hat_vari
        else
            ASSERT(.false.)
        endif
    else
        field_algo = field_name_algo
    endif
!
end subroutine
