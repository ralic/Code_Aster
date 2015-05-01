subroutine nmextl(mesh      , model    , keyw_fact, i_keyw_fact, field_type,&
                  field_disc, list_node, list_elem, nb_node    , nb_elem   ,&
                  type_extr)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/getvtx.h"
#include "asterfort/getelem.h"
#include "asterfort/getnode.h"
#include "asterfort/utmess.h"
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
    character(len=*), intent(in) :: mesh
    character(len=*), intent(in) :: model
    character(len=16), intent(in) :: keyw_fact
    integer, intent(in) :: i_keyw_fact
    character(len=24), intent(in) :: field_type
    character(len=4), intent(in) :: field_disc
    integer, intent(out) :: nb_node
    integer, intent(out) :: nb_elem
    character(len=24), intent(in) :: list_node
    character(len=24), intent(in) :: list_elem
    character(len=8), intent(out) :: type_extr
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Extraction (OBSERVATION/SUIVI_DDL) utilities 
!
! Get topology (nodes or elements) and type of extraction for field
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  sd_inout         : datastructure for input/output parameters
! In  keyw_fact        : factor keyword to read extraction parameters
! In  i_keyw_fact      : index of keyword to read extraction parameters
! In  field_type       : type of field (name in results datastructure)
! In  field_disc       : localization of field (discretization: NOEU or ELGA)
! In  list_node        : name of object contains list of nodes
! Out nb_node          : number of nodes
! In  list_elem        : name of object contains list of elements
! Out nb_elem          : number of elements
! Out type_extr        : type of extraction
!                'MIN'      VALEUR MINI SUR TOUTES LES MAILLES/NOEUDS
!                'MAX'      VALEUR MAXI SUR TOUTES LES MAILLES/NOEUDS
!                'MOY'      VALEUR MOYENNE TOUTES LES MAILLES/NOEUDS
!                'MINI_ABS' VALEUR MINI EN ABSOLU SUR TOUTES LES
!                          MAILLES/NOEUDS
!                'MAXI_ABS' VALEUR MAXI EN ABSOLU SUR TOUTES LES
!                          MAILLES/NOEUDS
!                'VALE'     VALEUR TOUTES LES MAILLES/NOEUDS
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nocc
!
! --------------------------------------------------------------------------------------------------
!
    nb_node   = 0
    nb_elem   = 0
    type_extr = 'VALE'
!
! - Type of extraction on field
!
    call getvtx(keyw_fact, 'EVAL_CHAM', iocc=i_keyw_fact, scal=type_extr, nbret=nocc)
    if (nocc .eq. 0) then
        type_extr = 'VALE'
        call utmess('A', 'EXTRACTION_5', sk=field_type)
    endif
!
! - Get list of nodes
!
    if (field_disc .eq. 'NOEU') then
        call getnode(mesh   , keyw_fact, i_keyw_fact, ' ', list_node,&
                     nb_node, model)
        if (nb_node .eq. 0) then
            call utmess('F', 'EXTRACTION_3', sk=field_type)
        endif
    endif
!
! - Get list of elements
!
    if (field_disc .eq. 'ELGA') then
        call getelem(mesh   , keyw_fact, i_keyw_fact, ' ', list_elem,&
                     nb_elem, model = model)
        if (nb_elem .eq. 0) then
            call utmess('F', 'EXTRACTION_4', sk=field_type)
        endif
    endif
!
end subroutine
