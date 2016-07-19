subroutine nmextk(mesh     , keyw_fact , i_keyw_fact, field        , field_type,&
                  field_s  , field_disc, list_node  , list_elem    , list_poin ,&
                  list_spoi, nb_node   , nb_elem    , nb_poin      , nb_spoi   ,&
                  compor   , list_cmp  , list_vari  , nb_cmp     , type_sele_cmp)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedetr.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/lxliis.h"
#include "asterfort/posddl.h"
#include "asterfort/utmess.h"
#include "asterfort/varinonu.h"
#include "asterfort/wkvect.h"
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
    character(len=8), intent(in) :: mesh
    character(len=16), intent(in) :: keyw_fact
    integer, intent(in) :: i_keyw_fact
    character(len=19), intent(in) :: field
    character(len=24), intent(in) :: field_type
    character(len=24), intent(in) :: field_s
    character(len=4), intent(in) :: field_disc
    integer, intent(in) :: nb_node
    integer, intent(in) :: nb_elem
    integer, intent(in) :: nb_poin
    integer, intent(in) :: nb_spoi
    character(len=24), intent(in) :: list_node
    character(len=24), intent(in) :: list_elem
    character(len=24), intent(in) :: list_poin
    character(len=24), intent(in) :: list_spoi
    character(len=19), optional, intent(in) :: compor
    character(len=24), intent(in) :: list_cmp
    character(len=24), intent(in) :: list_vari
    integer, intent(out) :: nb_cmp
    character(len=8), intent(out) :: type_sele_cmp
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Extraction (OBSERVATION/SUIVI_DDL) utilities
!
! Get component(s)
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  keyw_fact        : factor keyword to read extraction parameters
! In  i_keyw_fact      : index of keyword to read extraction parameters
! In  field            : name of field
! In  field_type       : type of field (name in results datastructure)
! In  field_disc       : localization of field (discretization: NOEU or ELGA)
! In  field_s          : name of reduced field (CHAM_ELEM_S)
! In  list_node        : name of object contains list of nodes
! In  nb_node          : number of nodes
! In  list_elem        : name of object contains list of elements
! In  nb_elem          : number of elements
! In  list_poin        : name of object contains list of points (Gauss)
! In  nb_poin          : number of points (Gauss)
! In  list_spoi        : name of object contains list of subpoints
! In  nb_spoi          : number of subpoints
! In  compor           : name of <CARTE> COMPOR
! In  list_cmp         : name of object contains list of components (NOM_CMP)
! In  list_vari        : name of object contains list of components (NOM_VARI)
! Out nb_cmp           : number of components
! Out type_sele_cmp    : type of selection for components NOM_CMP or NOM_VARI
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_para_maxi
    parameter    (nb_para_maxi=20)
    integer :: n1
    integer :: iret, iad
    integer :: i_node, i_elem, i_cmp, ipi, ispi, ipar, i_cmp_maxi
    integer :: nb_cmp_maxi, nuno, nuddl
    integer :: nb_elem_poin, nb_elem_spoi, npi, nspi
    integer :: node_nume, elem_nume, num, snum
    character(len=8) :: node_name, elem_name, cmp_name
    character(len=8) :: cmp_vari_name
    integer :: i_vari
    character(len=16) :: valk(2)
    integer :: jcesd, jcesl, jcesv
    integer :: vali(4)
    character(len=8), pointer :: cesc(:) => null()
    character(len=8), pointer :: v_list_cmp(:) => null()
    character(len=16), pointer :: v_list_vari(:) => null()
    integer, pointer :: v_list_node(:) => null()
    integer, pointer :: v_list_elem(:) => null()
    integer, pointer :: v_list_poin(:) => null()
    integer, pointer :: v_list_spoi(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nb_cmp        = 0
    type_sele_cmp = ' '
!
! - Get reduced field (CHAM_ELEM_S)
!
    if (field_disc .eq. 'ELGA') then
        call jeveuo(field_s(1:19)//'.CESD', 'L', jcesd)
        call jeveuo(field_s(1:19)//'.CESL', 'L', jcesl)
        call jeveuo(field_s(1:19)//'.CESV', 'L', jcesv)
        call jeveuo(field_s(1:19)//'.CESC', 'L', vk8=cesc)
        nb_cmp_maxi = zi(jcesd+4)
    endif
!
! - Number and name of components
!
    call getvtx(keyw_fact, 'NOM_CMP', iocc=i_keyw_fact, nbval=0, nbret=n1)
    if (n1.lt.0) then
        nb_cmp = -n1
        if ((nb_cmp.lt.1) .or. (nb_cmp.gt.nb_para_maxi)) then
            vali(1) = nb_para_maxi
            vali(2) = nb_cmp
            call utmess('F', 'EXTRACTION_12', ni=2, vali=vali)
        endif

        call wkvect(list_cmp, 'V V K8', nb_cmp, vk8 = v_list_cmp)
        call getvtx(keyw_fact, 'NOM_CMP', iocc=i_keyw_fact, nbval=nb_cmp, vect=v_list_cmp,&
                    nbret=iret)
        type_sele_cmp = 'NOM_CMP'
    else
        call getvtx(keyw_fact, 'NOM_VARI', iocc=i_keyw_fact, nbval=0, nbret=n1)
        ASSERT(n1.lt.0)
        ASSERT(field_type.eq.'VARI_ELGA')
        nb_cmp = -n1
        call wkvect(list_cmp , 'V V K8', nb_cmp, vk8 = v_list_cmp)
        call wkvect(list_vari, 'V V K16', nb_cmp, vk16 = v_list_vari)
        call getvtx(keyw_fact, 'NOM_VARI', iocc=i_keyw_fact, nbval=nb_cmp, vect=v_list_vari,&
                    nbret=iret)
        call jeveuo(list_elem, 'L', vi = v_list_elem)
        call varinonu(compor, ' ', nb_elem, v_list_elem, nb_cmp, v_list_vari, v_list_cmp)
        type_sele_cmp = 'NOM_VARI'
    endif
!
! - Check components
!
    if (field_disc .eq. 'NOEU') then
!
! ----- For nodes
!
        call jeveuo(list_node, 'L', vi = v_list_node)
        do i_node = 1, nb_node
!
! --------- Current node
!
            node_nume = v_list_node(i_node)
            call jenuno(jexnum(mesh(1:8)//'.NOMNOE', node_nume), node_name)
            do i_cmp = 1, nb_cmp
                cmp_name = v_list_cmp(i_cmp)
                call posddl('CHAM_NO', field, node_name, cmp_name, nuno,&
                            nuddl)
                if ((nuno.eq.0) .or. (nuddl.eq.0)) then
                    valk(1) = node_name
                    valk(2) = cmp_name
                    call utmess('F', 'EXTRACTION_20', nk=2, valk=valk)
                endif
            end do
        end do
    else if (field_disc.eq.'ELGA') then
!
! ----- For elements
!
        call jeveuo(list_elem, 'L', vi = v_list_elem)
        call jeveuo(list_poin, 'L', vi = v_list_poin)
        call jeveuo(list_spoi, 'L', vi = v_list_spoi)
        do i_elem = 1, nb_elem
!
! --------- Current element
!
            elem_nume = v_list_elem(i_elem)
            call jenuno(jexnum(mesh(1:8)//'.NOMMAI', elem_nume), elem_name)
!
! --------- Number of points/subpoints on current element
!
            nb_elem_poin = zi(jcesd+5+4*(elem_nume-1))
            nb_elem_spoi = zi(jcesd+5+4*(elem_nume-1)+1)
!
! --------- Check
!
            npi = nb_poin
            nspi = nb_spoi
            if (npi .gt. nb_elem_poin) npi = nb_elem_poin
            if (nspi .gt. nb_elem_spoi) nspi = nb_elem_spoi
!
            nb_cmp_maxi = zi(jcesd+4)
            do ipar = 1, nb_cmp
                cmp_name = v_list_cmp(ipar)
!
! ------------- For VARI_ELGA field
!
                if (field_type(1:4) .eq. 'VARI') then
                    cmp_vari_name = cmp_name(2:8)//' '
                    call lxliis(cmp_vari_name, i_vari, iret)
                    if (iret.ne.0) then
                        call utmess('F', 'EXTRACTION_22', sk=cmp_name)
                    endif
                else
                    i_vari = 0
                endif

                if (field_type(1:4) .eq. 'VARI') then
                    i_cmp = i_vari
                else
                    do i_cmp_maxi = 1, nb_cmp_maxi
                        if (cmp_name .eq. cesc(i_cmp_maxi)) then
                            i_cmp=i_cmp_maxi
                        endif
                    end do
                endif
                do ipi = 1, npi
                    num = v_list_poin(ipi)
                    ASSERT(num.ne.0)
                    do ispi = 1, nspi
                        snum = v_list_spoi(ispi)
                        ASSERT(snum.ne.0)
                        call cesexi('C', jcesd, jcesl, elem_nume, num,&
                                    snum, i_cmp, iad)
                        if (iad .eq. 0) then
                            valk(1) = elem_name
                            valk(2) = cmp_name
                            vali(1) = num
                            vali(2) = snum
                            call utmess('F', 'EXTRACTION_21', nk=2, valk=valk, ni=2,&
                                        vali=vali)
                        endif
                    end do
                end do
            end do
        end do
    else
        ASSERT(.false.)
    endif
!
end subroutine
