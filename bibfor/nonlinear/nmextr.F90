subroutine nmextr(meshz       , modelz , sdextrz, sd_inout, keyw_fact,&
                  nb_keyw_fact, nb_extr)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/getvtx.h"
#include "asterfort/impfoi.h"
#include "asterfort/jedetr.h"
#include "asterfort/nmextc.h"
#include "asterfort/nmextd.h"
#include "asterfort/nmextf.h"
#include "asterfort/nmextk.h"
#include "asterfort/nmextl.h"
#include "asterfort/nmextn.h"
#include "asterfort/nmextp.h"
#include "asterfort/nmextt.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=24), intent(in) :: sd_inout
    integer, intent(in) :: nb_keyw_fact
    character(len=16), intent(in) :: keyw_fact
    integer, intent(out) :: nb_extr
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
! In  sd_inout         : datastructure for input/output parameters
! In  keyw_fact        : factor keyword to read extraction parameters
! In  nb_keyw_fact     : number of factor keyword to read extraction parameters
! Out nb_extr          : total number of extraction points
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_keyw_fact, i_list_field, i_field
    integer :: nb_extr_keyw, nb_field
    integer :: nb_node, nb_elem, nb_poin, nb_spoi, nb_cmp
    character(len=2) :: chaine
    character(len=24) :: oldcha, field_type, field_s
    character(len=4) :: field_disc
    aster_logical :: l_extr, l_find
    character(len=24) :: list_node, list_elem, list_poin, list_spoi, list_cmp
    character(len=19) :: field
    character(len=8) :: type_extr_cmp, type_extr, type_extr_elem
    character(len=14) :: sdextr
    character(len=24) :: extr_info, extr_type, extr_flag, extr_field
    integer, pointer :: v_extr_info(:) => null()
    character(len=8), pointer :: v_extr_type(:) => null()
    aster_logical, pointer :: v_extr_flag(:) => null()
    character(len=24), pointer :: v_extr_field(:) => null()
    character(len=24), pointer :: v_list_field(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nb_extr    = 0
    nb_field   = 0
    sdextr     = sdextrz
!
! - Create information vector
!
    extr_info = sdextr(1:14)//'     .INFO'
    call wkvect(extr_info, 'V V I', 7+7*nb_keyw_fact, vi = v_extr_info)
    if (nb_keyw_fact .eq. 0) goto 99
!
! - Create extraction type vector
!
    extr_type = sdextr(1:14)//'     .EXTR'
    call wkvect(extr_type, 'V V K8', 3*nb_keyw_fact, vk8 = v_extr_type)
!
! - Create extraction flag vector
!
    extr_flag = sdextr(1:14)//'     .ACTI'
    call wkvect(extr_flag, 'V V L', nb_keyw_fact, vl = v_extr_flag)
!
! - List of field to extract
!
    AS_ALLOCATE(vk24 = v_list_field, size = nb_keyw_fact)
    do i_keyw_fact = 1, nb_keyw_fact
!
! ----- Read field type
!
        call nmextc(sd_inout, keyw_fact, i_keyw_fact, field_type, l_extr)
        if (.not.l_extr) then
            field_type = 'NONE'
        endif
!
! ----- Add field in list to extract
!
        l_find = .false.
        do i_list_field = 1, nb_keyw_fact - 1
            oldcha = v_list_field(i_list_field)
            if (oldcha .eq. field_type) then
                i_field = i_list_field
                l_find  = .true.
            endif
        end do
        if (.not.l_find) then
            nb_field = nb_field + 1
            i_field  = nb_field
            v_list_field(i_field) = field_type
        endif
        v_extr_info(7+7*(i_keyw_fact-1)+7) = i_field
    end do
!
! - Create extraction field vector
!
    extr_field = sdextr(1:14)//'     .CHAM'
    call wkvect(extr_field, 'V V K24', 2*nb_field, vk24 = v_extr_field)
    do i_field = 1, nb_field
        field_type   = v_list_field(i_field)
        field_s = field_type(1:18)//'S'
        v_extr_field(2*(i_field-1)+1) = field_type
        v_extr_field(2*(i_field-1)+2) = field_s
    end do
!
! - Prepare extraction data
!
    do i_keyw_fact = 1, nb_keyw_fact
!
        nb_extr_keyw = 0
        type_extr_elem = 'NONE'
        type_extr_cmp = 'NONE'
        type_extr = 'NONE'
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
! ----- Type of field
!
        i_field      = v_extr_info(7+7*(i_keyw_fact-1)+7)
        field_type   = v_extr_field(2*(i_field-1)+1)
        field_s      = v_extr_field(2*(i_field-1)+2)
        if (field_type .eq. 'NONE') then
            call getvtx(keyw_fact, 'NOM_CHAM', iocc=i_keyw_fact, scal=field_type)
            call utmess('A', 'EXTRACTION_99', sk=field_type)
            goto 999
        endif
!
! ----- Get localization of field (discretization: NOEU or ELGA)
!
        call nmextt(sd_inout, field_type, field_disc)
!
! ----- Get field
!
        call nmextd(field_type, sd_inout, field)
!
! ----- Get topology (nodes or elements) and type of extraction for field
!
        call nmextl(meshz     , modelz   , keyw_fact, i_keyw_fact, field_type,&
                    field_disc, list_node, list_elem, nb_node    , nb_elem   ,&
                    type_extr)
!
! ----- Get topology (point and subpoints) and type of extraction for element
!
        if (field_disc .eq. 'ELGA') then
            call nmextp(keyw_fact, i_keyw_fact, field_type, field  , field_s       ,&
                        list_poin, list_spoi  , nb_poin   , nb_spoi, type_extr_elem)
        endif
!
! ----- Get component(s)
!
        call nmextk(meshz    , keyw_fact , i_keyw_fact, field    , field_type,&
                    field_s  , field_disc, list_node  , list_elem, list_poin ,&
                    list_spoi, nb_node   , nb_elem    , nb_poin  , nb_spoi   ,&
                    list_cmp , nb_cmp)
!
! ----- Get type of extraction for components
!
        call nmextf(keyw_fact, i_keyw_fact, type_extr_cmp)
!
! ----- Count number of extractions
!
        call nmextn(field_disc, type_extr_cmp, type_extr_elem, type_extr, nb_node,&
                    nb_elem   , nb_cmp       , nb_poin       , nb_spoi  , nb_extr_keyw)
!
! ----- Save
!
        v_extr_type(3*(i_keyw_fact-1)+1) = type_extr
        v_extr_type(3*(i_keyw_fact-1)+2) = type_extr_elem
        v_extr_type(3*(i_keyw_fact-1)+3) = type_extr_cmp
        v_extr_info(7+7*(i_keyw_fact-1)+1) = nb_cmp
        v_extr_info(7+7*(i_keyw_fact-1)+2) = nb_node
        v_extr_info(7+7*(i_keyw_fact-1)+3) = nb_elem
        v_extr_info(7+7*(i_keyw_fact-1)+4) = nb_poin
        v_extr_info(7+7*(i_keyw_fact-1)+5) = nb_spoi
        v_extr_info(7+7*(i_keyw_fact-1)+6) = nb_extr_keyw
!
999     continue
!
        nb_extr = nb_extr + nb_extr_keyw
!
    end do
!
!
! --- DESTRUCTION DES CHAM_ELEM_S
!
    do i_field = 1, nb_field
        field_s = v_extr_field(2*(i_field-1)+2)
        call jedetr(field_s)
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
    v_extr_info(7) = 0
!
    AS_DEALLOCATE(vk24 = v_list_field)
!
end subroutine
