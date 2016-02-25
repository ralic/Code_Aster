subroutine CreateVoidColumn(column)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/ismaem.h"
#include "asterc/r8vide.h"
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
    type(NL_DS_Column), intent(out) :: column
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Create void column
!
! --------------------------------------------------------------------------------------------------
!
! Out column           : column
!
! --------------------------------------------------------------------------------------------------
!
    column%name          = ' '
    column%l_vale_affe   = .false._1
    column%l_vale_inte   = .false._1
    column%l_vale_real   = .false._1
    column%l_vale_cplx   = .false._1
    column%l_vale_strg   = .false._1
    column%vale_inte     = ismaem()
    column%vale_real     = r8vide()
    column%vale_cplx     = dcmplx(r8vide(),r8vide())
    column%vale_strg     = ' '
    column%mark          = ' '
    column%title(1:3)    = ' '
!
end subroutine
