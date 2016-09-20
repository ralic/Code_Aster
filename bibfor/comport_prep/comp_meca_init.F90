subroutine comp_meca_init(ds_compor)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterc/getfac.h"
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
! aslint: disable=W1403
! person_in_charge: mickael.abbas at edf.fr
!
    type(NL_DS_Compor), intent(out) :: ds_compor
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Init datastructure to describe comportement
!
! --------------------------------------------------------------------------------------------------
!
! Out ds_compor        : datastructure to describe comportement
!
! --------------------------------------------------------------------------------------------------
!
    ds_compor%rela_comp       = 'VIDE'
    ds_compor%defo_comp       = 'VIDE'
    ds_compor%type_comp       = 'VIDE'
    ds_compor%type_cpla       = 'VIDE'
    ds_compor%kit_comp(:)     = 'VIDE'
    ds_compor%mult_comp       = 'VIDE'
    ds_compor%type_matg       = 'VIDE'
    ds_compor%post_iter       = 'VIDE'
    ds_compor%nb_vari         = 0
    ds_compor%nb_vari_comp(:) = 0
    ds_compor%nume_comp(:)    = 0
!
end subroutine
