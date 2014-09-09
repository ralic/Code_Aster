subroutine nmsuiv(meshz    , sd_suiv, sd_prnt, cara_elemz, modelz,&
                  matez    , compor , valinc , varc_refe , sddisc,&
                  nume_inst)
!
implicit none
!
#include "asterfort/impfoi.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/diinst.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmext0.h"
#include "asterfort/nmext1.h"
#include "asterfort/nmsui3.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmextr_comp.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=24), intent(in) :: sd_suiv
    character(len=24), intent(in) :: sd_prnt
    character(len=19), intent(in) :: sddisc
    character(len=*), intent(in) :: cara_elemz
    character(len=*), intent(in) :: matez
    character(len=*), intent(in) :: modelz
    character(len=19), intent(in) :: compor
    character(len=*), intent(in) :: varc_refe
    integer, intent(in) :: nume_inst
    character(len=19), intent(in) :: valinc(*)
!
! --------------------------------------------------------------------------------------------------
!
! Non-linear operators - DOF monitor
!
! Make monitoring
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  sd_prnt          : datastructure for print informations
! In  sd_suiv          : datastructure for dof monitor parameters
! In  sddisc           : datastructure for discretization
! In  model            : name of model
! In  cara_elem        : name of datastructure for elementary parameters (CARTE)
! In  mate             : name of material characteristics (field)
! In  compor           : name of <CARTE> COMPOR
! In  valinc           : hat variable for algorithm fields
! In  nume_inst        : index of current time step
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: list_node, list_elem, list_poin, list_spoi, list_cmp
    character(len=14) :: sdextr_suiv
    integer :: nb_cmp, nb_node, nb_elem, nb_field, nb_field_comp
    integer :: nb_poin, nb_spoi
    integer :: i_keyw_fact, nb_keyw_fact
    integer :: i_dof_monitor, i_field, i_field_comp
    real(kind=8) :: time
    character(len=2) :: chaine
    character(len=19) :: disp_curr, strx_curr, varc_curr
    character(len=24) :: field_type, field_s
    character(len=4) :: field_disc
    character(len=19) :: field, field_comp, ligrel
    character(len=8) :: type_extr_cmp, type_extr, type_extr_elem, mesh
    character(len=19) :: work_poin, work_node, work_elem
    character(len=24) :: extr_info, extr_type, extr_field, extr_comp
    integer, pointer :: v_extr_info(:) => null()
    character(len=8), pointer :: v_extr_type(:) => null()
    character(len=24), pointer :: v_extr_field(:) => null()
    character(len=24), pointer :: v_extr_comp(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    mesh          = meshz
    i_dof_monitor = 1
    time          = diinst(sddisc,nume_inst)
!
! - Get fields
!
    call nmchex(valinc, 'VALINC', 'DEPPLU', disp_curr)
    call nmchex(valinc, 'VALINC', 'STRPLU', strx_curr)
    call nmchex(valinc, 'VALINC', 'COMPLU', varc_curr)
!
! - Access to extraction datastructure
!
    sdextr_suiv = sd_suiv(1:14)
!
! - Get information vector
!
    extr_info     = sdextr_suiv(1:14)//'     .INFO'
    call jeveuo(extr_info, 'L', vi = v_extr_info)
    nb_keyw_fact  = v_extr_info(1)
    nb_field      = v_extr_info(6)
    nb_field_comp = v_extr_info(7)
    ASSERT(nb_keyw_fact.le.99)
    if (nb_keyw_fact .eq. 0) goto 999
!
! - Get extraction field vector
!
    extr_field = sdextr_suiv(1:14)//'     .CHAM'
    call jeveuo(extr_field, 'L', vk24 = v_extr_field)
!
! - Get extraction type vector
!
    extr_type  = sdextr_suiv(1:14)//'     .EXTR'
    call jeveuo(extr_type, 'L', vk8 = v_extr_type)
!
! - Get computed fields
!
    extr_comp  = sdextr_suiv(1:14)//'     .COMP'
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
            call nmextr_comp(field_comp, field_disc, field_type, meshz    , modelz   ,&
                             cara_elemz, matez     , compor    , disp_curr, strx_curr,&
                             varc_curr , varc_refe , time      , ligrelz = ligrel)
        end do
    endif
!
    do i_keyw_fact = 1, nb_keyw_fact
!
! ----- Datastructure name generation
!
        call impfoi(0, 2, i_keyw_fact, chaine)
        list_node = sdextr_suiv(1:14)//chaine(1:2)//'   .NOEU'
        list_elem = sdextr_suiv(1:14)//chaine(1:2)//'   .MAIL'
        list_poin = sdextr_suiv(1:14)//chaine(1:2)//'   .POIN'
        list_spoi = sdextr_suiv(1:14)//chaine(1:2)//'   .SSPI'
        list_cmp  = sdextr_suiv(1:14)//chaine(1:2)//'   .CMP '
!
! ----- Type of field
!
        i_field      = v_extr_info(7+7*(i_keyw_fact-1)+7)
        field_type   = v_extr_field(4*(i_field-1)+1)
        field_s      = v_extr_field(4*(i_field-1)+2)
        if (field_type .ne. 'NONE') then
!
! --------- Get localization of field (discretization: NOEU or ELGA)
!
            field_disc = v_extr_field(4*(i_field-1)+3)(1:4)
!
! --------- Get field
!
            field = v_extr_field(4*(i_field-1)+4)(1:19)
!
! --------- Get length of lists
!
            nb_cmp  = v_extr_info(7+7*(i_keyw_fact-1)+1)
            nb_node = v_extr_info(7+7*(i_keyw_fact-1)+2)
            nb_elem = v_extr_info(7+7*(i_keyw_fact-1)+3)
            nb_poin = v_extr_info(7+7*(i_keyw_fact-1)+4)
            nb_spoi = v_extr_info(7+7*(i_keyw_fact-1)+5)
!
! --------- Extraction types
!
            type_extr      = v_extr_type(3*(i_keyw_fact-1)+1)
            type_extr_elem = v_extr_type(3*(i_keyw_fact-1)+2)
            type_extr_cmp  = v_extr_type(3*(i_keyw_fact-1)+3)
!
! --------- Create temporary vectors for extraction
!
            work_elem = '&&NMSUIV.VALE.ELGA'
            work_poin = '&&NMSUIV.VALE.GAUS'
            work_node = '&&NMSUIV.VALE.NOEU'
            call nmext0(field_disc, nb_elem   , nb_node   , nb_poin   , nb_spoi       ,&
                        nb_cmp    , work_node , work_poin , work_elem , type_extr_elem,&
                        type_extr)
!
! --------- Compute extraction values and store them
!
            call nmext1(mesh          , field    , field_disc   , field_type, field_s,&
                        nb_elem       , nb_node  , nb_poin      , nb_spoi   , nb_cmp,&
                        type_extr_elem, type_extr, type_extr_cmp, list_node , list_elem,&
                        list_poin     , list_spoi, list_cmp     , work_node , work_poin,&
                        work_elem)
!
! --------- Print monitored values in table
!
            call nmsui3(sd_prnt      , field_disc, nb_elem  , nb_node      , nb_poin       ,&
                        nb_spoi      , nb_cmp    , type_extr, type_extr_cmp, type_extr_elem,&
                        list_elem    , work_node , work_elem, field        , field_s       ,&
                        i_dof_monitor)
!
            call jedetr(work_poin)
            call jedetr(work_node)
            call jedetr(work_elem)
        end if
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
999 continue
!
end subroutine
