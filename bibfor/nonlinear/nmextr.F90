subroutine nmextr(meshz       , modelz    , sdextrz   , ds_inout , keyw_fact,&
                  nb_keyw_fact, nb_extr   ,&
                  cara_elemz  , matez     , compor    , disp_curr, strx_curr,&
                  varc_curr   , varc_refe , time      )
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/detrsd.h"
#include "asterfort/getvtx.h"
#include "asterfort/impfoi.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmextr_read_1.h"
#include "asterfort/nmextr_read_2.h"
#include "asterfort/nmextr_crsd.h"
#include "asterfort/nmextr_ligr.h"
#include "asterfort/nmextc.h"
#include "asterfort/nmextd.h"
#include "asterfort/nmextf.h"
#include "asterfort/nmextk.h"
#include "asterfort/nmextl.h"
#include "asterfort/nmextn.h"
#include "asterfort/nmextp.h"
#include "asterfort/nmextt.h"
#include "asterfort/nmextr_comp.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
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
    character(len=*), intent(in) :: meshz
    character(len=*), intent(in) :: modelz
    character(len=*), intent(in) :: sdextrz
    type(NL_DS_InOut), intent(in) :: ds_inout
    integer, intent(in) :: nb_keyw_fact
    character(len=16), intent(in) :: keyw_fact
    integer, intent(out) :: nb_extr  
    character(len=*), optional, intent(in) :: cara_elemz
    character(len=*), optional, intent(in) :: matez
    character(len=19), optional, intent(in) :: compor
    character(len=*), optional, intent(in) :: disp_curr
    character(len=*), optional, intent(in) :: strx_curr
    character(len=*), optional, intent(in) :: varc_curr
    character(len=*), optional, intent(in) :: varc_refe
    real(kind=8), optional, intent(in) :: time
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Field extraction datastructure
!
! Read parameters
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  sdextr           : name of datastructure for extraction
! In  ds_inout         : datastructure for input/output management
! In  keyw_fact        : factor keyword to read extraction parameters
! In  nb_keyw_fact     : number of factor keyword to read extraction parameters
! Out nb_extr          : total number of extraction points
! In  cara_elem        : name of datastructure for elementary parameters (CARTE)
! In  mate             : name of material characteristics (field)
! In  compor           : name of <CARTE> COMPOR
! In  disp_curr        : current displacements
! In  varc_curr        : command variable for current time
! In  varc_refe        : command variable for reference
! In  time             : current time
! In  strx_curr        : fibers information for current time
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_keyw_fact, i_field, i_field_comp
    integer :: nb_extr_keyw, nb_field, nb_field_comp
    integer :: nb_node, nb_elem, nb_poin, nb_spoi, nb_cmp
    character(len=2) :: chaine
    character(len=24) :: field_type, field_s, field_comp
    character(len=4) :: field_disc
    character(len=24) :: list_node, list_elem, list_poin, list_spoi, list_cmp
    character(len=19) :: field
    character(len=8) :: type_extr_cmp, type_extr, type_extr_elem
    character(len=14) :: sdextr
    character(len=24) :: extr_info, extr_type, extr_flag, extr_field, extr_comp
    integer, pointer :: v_extr_info(:) => null()
    character(len=8), pointer :: v_extr_type(:) => null()
    aster_logical, pointer :: v_extr_flag(:) => null()
    character(len=24), pointer :: v_extr_field(:) => null()
    character(len=24), pointer :: v_extr_comp(:) => null()
    character(len=24), pointer :: list_field(:) => null()
    integer, pointer :: rela_field_keyw(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nb_extr    = 0
    nb_field   = 0
    sdextr     = sdextrz
!
! - List of fields to extract
!
    call nmextr_read_1(ds_inout, keyw_fact, nb_keyw_fact, list_field, rela_field_keyw,&
                       nb_field, nb_field_comp)
!
! - Create datastructure
!
    call nmextr_crsd(sdextr, nb_keyw_fact, nb_field, nb_field_comp)
    extr_info  = sdextr(1:14)//'     .INFO'
    call jeveuo(extr_info , 'E' , vi   = v_extr_info) 
    if (nb_keyw_fact.eq.0) then
        goto 99
    endif
!
! - Set datastructure for fields to compute
!
    if (nb_field_comp.ne.0) then
        call nmextr_read_2(sdextrz      , ds_inout, nb_keyw_fact, list_field, rela_field_keyw,&
                           nb_field_comp)
    endif
!
! - Access to datastructure
!
    extr_field = sdextr(1:14)//'     .CHAM'
    call jeveuo(extr_field, 'E' , vk24 = v_extr_field)
    extr_type  = sdextr(1:14)//'     .EXTR'
    call jeveuo(extr_type , 'E' , vk8  = v_extr_type)
    extr_flag  = sdextr(1:14)//'     .ACTI'
    call jeveuo(extr_flag , 'E' , vl   = v_extr_flag)
    extr_comp  = sdextr(1:14)//'     .COMP'
!
! - Pre-compute fields
!
    if (nb_field_comp.ne.0) then
        call jeveuo(extr_comp , 'L' , vk24 = v_extr_comp)
        do i_field_comp = 1, nb_field_comp
            field_comp = v_extr_comp(4*(i_field_comp-1)+1)
            field_disc = v_extr_comp(4*(i_field_comp-1)+2)(1:4) 
            field_type = v_extr_comp(4*(i_field_comp-1)+3)
            call nmextr_comp(field_comp, field_disc, field_type, meshz    , modelz   ,&
                             cara_elemz, matez     , compor    , disp_curr, strx_curr,&
                             varc_curr , varc_refe , time      )
        end do
    endif
!
! - Prepare extraction data
!
    do i_keyw_fact = 1, nb_keyw_fact
!
        nb_extr_keyw   = 0
        type_extr_elem = 'NONE'
        type_extr_cmp  = 'NONE'
        type_extr      = 'NONE'
!
! ----- Datastructure name generation
!
        call impfoi(0, 2, i_keyw_fact, chaine)
        list_node = sdextr(1:14)//chaine(1:2)//'   .NOEU'
        list_elem = sdextr(1:14)//chaine(1:2)//'   .MAIL'
        list_poin = sdextr(1:14)//chaine(1:2)//'   .POIN'
        list_spoi = sdextr(1:14)//chaine(1:2)//'   .SSPI'
        list_cmp  = sdextr(1:14)//chaine(1:2)//'   .CMP '
!
! ----- Get field index
!
        i_field = rela_field_keyw(i_keyw_fact)
        i_field = abs(i_field)
!
! ----- Type of field
!
        field_type   = list_field(i_field)
        if (field_type .eq. 'NONE') then
            call getvtx(keyw_fact, 'NOM_CHAM', iocc=i_keyw_fact, scal=field_type)
            call utmess('A', 'EXTRACTION_99', sk=field_type)
        else   
!
! --------- Get localization of field (discretization: NOEU or ELGA)
!
            call nmextt(ds_inout, field_type, field_disc)   
!
! --------- Get field
!
            call nmextd(field_type, ds_inout, field)
!
! --------- Get reduced field
!
            field_s      = field_type(1:18)//'S'
!
! --------- Get topology (nodes or elements) and type of extraction for field
!
            call nmextl(meshz     , modelz   , keyw_fact, i_keyw_fact, field_type,&
                        field_disc, list_node, list_elem, nb_node    , nb_elem   ,&
                        type_extr)
!
! --------- Get topology (point and subpoints) and type of extraction for element
!
            if (field_disc .eq. 'ELGA') then
                call nmextp(keyw_fact, i_keyw_fact, field_type, field  , field_s       ,&
                            list_poin, list_spoi  , nb_poin   , nb_spoi, type_extr_elem)
            endif
!
! --------- Get component(s)
!
            call nmextk(meshz    , keyw_fact , i_keyw_fact, field    , field_type,&
                        field_s  , field_disc, list_node  , list_elem, list_poin ,&
                        list_spoi, nb_node   , nb_elem    , nb_poin  , nb_spoi   ,&
                        list_cmp , nb_cmp)
!
! --------- Get type of extraction for components
!
            call nmextf(keyw_fact, i_keyw_fact, type_extr_cmp)
!
! --------- Count number of extractions
!
            call nmextn(field_disc, type_extr_cmp, type_extr_elem, type_extr, nb_node,&
                        nb_elem   , nb_cmp       , nb_poin       , nb_spoi  , nb_extr_keyw)
!
! --------- Save
!
            v_extr_field(4*(i_field-1)+1) = field_type
            v_extr_field(4*(i_field-1)+2) = field_s 
            v_extr_field(4*(i_field-1)+3) = field_disc
            v_extr_field(4*(i_field-1)+4) = field
            v_extr_type(3*(i_keyw_fact-1)+1) = type_extr
            v_extr_type(3*(i_keyw_fact-1)+2) = type_extr_elem
            v_extr_type(3*(i_keyw_fact-1)+3) = type_extr_cmp
            v_extr_info(7+7*(i_keyw_fact-1)+1) = nb_cmp
            v_extr_info(7+7*(i_keyw_fact-1)+2) = nb_node
            v_extr_info(7+7*(i_keyw_fact-1)+3) = nb_elem
            v_extr_info(7+7*(i_keyw_fact-1)+4) = nb_poin
            v_extr_info(7+7*(i_keyw_fact-1)+5) = nb_spoi
            v_extr_info(7+7*(i_keyw_fact-1)+6) = nb_extr_keyw
            v_extr_info(7+7*(i_keyw_fact-1)+7) = i_field
!
        endif
!
        nb_extr = nb_extr + nb_extr_keyw
!
    end do
!
! - Cleaning CHAM_ELEM_S
!
    do i_field = 1, nb_field
        field_s    = v_extr_field(4*(i_field-1)+2)
        field_disc = v_extr_field(4*(i_field-1)+3)(1:4)
        if (field_disc.eq.'ELGA') then
            call detrsd('CHAM_ELEM_S', field_s)
        elseif (field_disc.eq.'NOEU') then
            call detrsd('CHAM_NO_S', field_s)
        else
            ASSERT(.false.)
        endif
    end do
 99 continue
!
! - Set information vector
!
    v_extr_info(1) = nb_keyw_fact
    v_extr_info(2) = nb_extr
    v_extr_info(3) = 1
    v_extr_info(4) = 0
    v_extr_info(5) = 0
    v_extr_info(6) = nb_field
    v_extr_info(7) = nb_field_comp
!
! - Create LIGREL for fields not a default in nonlinear operator
!
    if (nb_field_comp.ne.0) then
        call nmextr_ligr(meshz, modelz, sdextrz, nb_keyw_fact, nb_field_comp)
    endif
!
    AS_DEALLOCATE(vk24 = list_field)
    AS_DEALLOCATE(vi   = rela_field_keyw)
!
end subroutine
