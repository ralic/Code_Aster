function isdiri(list_load, load_type_2)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/ischar.h"
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
    aster_logical :: isdiri
    character(len=19), intent(in) :: list_load
    character(len=4), intent(in) :: load_type_2
!
! --------------------------------------------------------------------------------------------------
!
! List of loads - Utility
!
! Return .true. if Dirichlet loads exist
!
! --------------------------------------------------------------------------------------------------
!
! In  list_load      : name of datastructure for list of loads
! In  load_type_1    : second level of type
!                'DUAL' - AFFE_CHAR_MECA
!                'ELIM' - AFFE_CHAR_CINE
!                '    ' - All types
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: lelim, ldual
!
! --------------------------------------------------------------------------------------------------
!
    lelim = ischar(list_load, 'DIRI', 'ELIM')
    ldual = ischar(list_load, 'DIRI', 'DUAL')
    if (load_type_2 .eq. '    ') then
        isdiri = lelim.or.ldual
    else if (load_type_2.eq.'ELIM') then
        isdiri = lelim
    else if (load_type_2.eq.'DUAL') then
        isdiri = ldual
    else
        ASSERT(.false.)
    endif

end function
