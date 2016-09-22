subroutine nmobse(meshz     , sd_obsv  , time,&
                  cara_elemz, modelz   , matez    , ds_constitutive, disp_curr,&
                  strx_curr , varc_curr, varc_refe)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/impfoi.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmextr_comp.h"
#include "asterfort/nmext0.h"
#include "asterfort/nmext1.h"
#include "asterfort/nmobs2.h"
#include "asterfort/utmess.h"
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
    character(len=*), intent(in) :: meshz
    character(len=19), intent(in) :: sd_obsv
    real(kind=8), intent(in) :: time
    character(len=*), optional, intent(in) :: cara_elemz
    character(len=*), optional, intent(in) :: matez
    character(len=*), optional, intent(in) :: modelz
    type(NL_DS_Constitutive), optional, intent(in) :: ds_constitutive
    character(len=*), optional, intent(in) :: disp_curr
    character(len=*), optional, intent(in) :: strx_curr
    character(len=*), optional, intent(in) :: varc_curr
    character(len=*), optional, intent(in) :: varc_refe
!
! --------------------------------------------------------------------------------------------------
!
! Non-linear operators - Observation
!
! Make observation
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  time             : current time
! In  sd_obsv          : datastructure for observation parameters
! In  model            : name of model
! In  cara_elem        : name of datastructure for elementary parameters (CARTE)
! In  mate             : name of material characteristics (field)
! In  ds_constitutive  : datastructure for constitutive laws management
! In  disp_curr        : current displacements
! In  varc_curr        : command variable for current time
! In  varc_refe        : command variable for reference
! In  strx_curr        : fibers information for current time
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: list_node, list_elem, list_poin, list_spoi, list_cmp, list_vari
    character(len=14) :: sdextr_obsv
    character(len=19) :: tabl_name
    character(len=16) :: title
    integer :: nb_cmp, nb_node, nb_elem, nb_field, nb_field_comp
    integer :: nb_poin, nb_spoi
    integer :: i_keyw_fact, nb_keyw_fact, nb_obsf_effe, i_field, i_field_comp
    character(len=2) :: chaine
    character(len=24) :: field_type, field_s
    character(len=4) :: field_disc
    character(len=19) :: field, ligrel, field_comp
    character(len=8) :: type_extr_cmp, type_extr, type_extr_elem, type_sele_cmp, mesh
    character(len=19) :: work_poin, work_node, work_elem
    aster_logical :: l_obsv
    character(len=24) :: obsv_titl
    character(len=16), pointer :: v_obsv_titl(:) => null()
    character(len=24) :: obsv_tabl
    character(len=24), pointer :: v_obsv_tabl(:) => null()
    character(len=24) :: extr_info, extr_type, extr_flag, extr_field, extr_comp
    integer, pointer :: v_extr_info(:) => null()
    character(len=8), pointer :: v_extr_type(:) => null()
    aster_logical, pointer :: v_extr_flag(:) => null()
    character(len=24), pointer :: v_extr_field(:) => null()
    character(len=24), pointer :: v_extr_comp(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    mesh = meshz
!
! - Get name of observation table
!
    obsv_tabl = sd_obsv(1:14)//'     .TABL'
    call jeveuo(obsv_tabl, 'L', vk24 = v_obsv_tabl)
    tabl_name = v_obsv_tabl(1)(1:19)
!
! - Get vector for title
!
    obsv_titl = sd_obsv(1:14)//'     .TITR'
    call jeveuo(obsv_titl, 'L', vk16 = v_obsv_titl)
!
! - Access to extraction datastructure
!
    sdextr_obsv = sd_obsv(1:14)
!
! - Get information vector
!
    extr_info    = sdextr_obsv(1:14)//'     .INFO'
    call jeveuo(extr_info, 'L', vi = v_extr_info)
    nb_keyw_fact  = v_extr_info(1)
    nb_field      = v_extr_info(6)
    nb_field_comp = v_extr_info(7)
    ASSERT(nb_keyw_fact.le.99)
!
! - Get extraction field vector
!
    extr_field = sdextr_obsv(1:14)//'     .CHAM'
    call jeveuo(extr_field, 'L', vk24 = v_extr_field)
!
! - Get extraction type vector
!
    extr_type = sdextr_obsv(1:14)//'     .EXTR'
    call jeveuo(extr_type, 'L', vk8 = v_extr_type)
!
! - Get extraction flag vector
!
    extr_flag = sdextr_obsv(1:14)//'     .ACTI'
    call jeveuo(extr_flag, 'L', vl = v_extr_flag)
!
! - Get computed fields
!
    extr_comp = sdextr_obsv(1:14)//'     .COMP'
!
! - Fields to compute (not a default in nonlinear operator)
!
    if (nb_field_comp.ne.0) then
        call jeveuo(extr_comp , 'L' , vk24 = v_extr_comp)
        do i_field_comp = 1, nb_field_comp
            field_comp = v_extr_comp(4*(i_field_comp-1)+1)(1:19)
            field_disc = v_extr_comp(4*(i_field_comp-1)+2)(1:4) 
            field_type = v_extr_comp(4*(i_field_comp-1)+3)
            ligrel     = v_extr_comp(4*(i_field_comp-1)+4)(1:19)
            call nmextr_comp(field_comp, field_disc, field_type     , meshz    , modelz   ,&
                             cara_elemz, matez     , ds_constitutive, disp_curr, strx_curr,&
                             varc_curr , varc_refe , time           , ligrelz = ligrel)
        end do
    endif
!
    nb_obsf_effe = 0
!
    do i_keyw_fact = 1, nb_keyw_fact
!
        l_obsv = v_extr_flag(i_keyw_fact)
        if (l_obsv) then
!
! --------- Datastructure name generation
!
            call impfoi(0, 2, i_keyw_fact, chaine)
            list_node = sdextr_obsv(1:14)//chaine(1:2)//'   .NOEU'
            list_elem = sdextr_obsv(1:14)//chaine(1:2)//'   .MAIL'
            list_poin = sdextr_obsv(1:14)//chaine(1:2)//'   .POIN'
            list_spoi = sdextr_obsv(1:14)//chaine(1:2)//'   .SSPI'
            list_cmp  = sdextr_obsv(1:14)//chaine(1:2)//'   .CMP '
            list_vari = sdextr_obsv(1:14)//chaine(1:2)//'   .VARI'
!
! --------- Type of field
!
            i_field      = v_extr_info(7+7*(i_keyw_fact-1)+7)
            field_type   = v_extr_field(4*(i_field-1)+1)
            field_s      = v_extr_field(4*(i_field-1)+2)
            if (field_type .ne. 'NONE') then
!
! ------------- Get localization of field (discretization: NOEU or ELGA)
!
                field_disc = v_extr_field(4*(i_field-1)+3)(1:4)
!
! ------------- Get field
!
                field = v_extr_field(4*(i_field-1)+4)(1:19)
!
! ------------- Get length of lists
!
                nb_cmp  = v_extr_info(7+7*(i_keyw_fact-1)+1)
                nb_node = v_extr_info(7+7*(i_keyw_fact-1)+2)
                nb_elem = v_extr_info(7+7*(i_keyw_fact-1)+3)
                nb_poin = v_extr_info(7+7*(i_keyw_fact-1)+4)
                nb_spoi = v_extr_info(7+7*(i_keyw_fact-1)+5)
!
! ------------- Extraction types
!
                type_extr      = v_extr_type(4*(i_keyw_fact-1)+1)
                type_extr_elem = v_extr_type(4*(i_keyw_fact-1)+2)
                type_extr_cmp  = v_extr_type(4*(i_keyw_fact-1)+3)
                type_sele_cmp  = v_extr_type(4*(i_keyw_fact-1)+4)
!
! ------------- Create temporary vectors for extraction
!
                work_elem = '&&NMOBSE.VALE.ELGA'
                work_poin = '&&NMOBSE.VALE.GAUS'
                work_node = '&&NMOBSE.VALE.NOEU'
                call nmext0(field_disc, nb_elem   , nb_node   , nb_poin   , nb_spoi       ,&
                            nb_cmp    , work_node , work_poin , work_elem , type_extr_elem,&
                            type_extr)
!
! ------------- Compute extraction values and store them
!
                call nmext1(mesh          , field    , field_disc   , field_type, field_s,&
                            nb_elem       , nb_node  , nb_poin      , nb_spoi   , nb_cmp,&
                            type_extr_elem, type_extr, type_extr_cmp, list_node , list_elem,&
                            list_poin     , list_spoi, list_cmp     , work_node , work_poin,&
                            work_elem)
!
! ------------- Get title of observation
!
                title = v_obsv_titl(i_keyw_fact)
!
! ------------- Save extraction values in table
!
                call nmobs2(mesh          , sd_obsv   , tabl_name, time          , title,&
                            field_disc    , field_type, field_s  ,&
                            nb_elem       , nb_node   , nb_poin      , nb_spoi      , nb_cmp   ,&
                            type_extr_elem, type_extr , type_extr_cmp, type_sele_cmp,&
                            list_node     , list_elem , list_poin     , list_spoi,&
                            list_cmp      , list_vari ,&
                            field         , work_node, work_elem     , nb_obsf_effe)
!
                call jedetr(work_poin)
                call jedetr(work_node)
                call jedetr(work_elem)
            endif
        endif
    end do
!
! - Print
!
    if (nb_obsf_effe .eq. 0) then
        call utmess('I', 'OBSERVATION_39')
    else if (nb_obsf_effe.eq.1) then
        call utmess('I', 'OBSERVATION_38')
    else
        call utmess('I', 'OBSERVATION_37', si=nb_obsf_effe)
    endif
!
! - Cleanig
!
    do i_field = 1, nb_field
        field_s = v_extr_field(4*(i_field-1)+2)
        call jedetr(field_s)
    end do
!
end subroutine
