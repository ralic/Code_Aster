subroutine ccchuc(sdresu_in, sdresu_out, field_type, nume_field_out, type_comp,&
                  crit, norm, nb_form, name_form, list_ordr,&
                  nb_ordr)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/ccchci.h"
#include "asterfort/ccchuc_chamel.h"
#include "asterfort/ccchuc_chamno.h"
#include "asterfort/ccchuc_ligr.h"
#include "asterfort/ccchuc_norm.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/cescrm.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnscno.h"
#include "asterfort/cnscre.h"
#include "asterfort/codent.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exlim1.h"
#include "asterfort/gnomsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
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
! person_in_charge: mathieu.courtois at edf.fr
!
    character(len=8), intent(in) :: sdresu_in
    character(len=8), intent(in) :: sdresu_out
    character(len=16), intent(in) :: field_type
    character(len=16), intent(in) :: type_comp
    character(len=16), intent(in) :: crit
    character(len=16), intent(in) :: norm
    integer, intent(in) :: nb_form
    character(len=8), intent(in) :: name_form(nb_form)
    integer, intent(in) :: nume_field_out
    character(len=19), intent(in) :: list_ordr
    integer, intent(in) :: nb_ordr
!
! --------------------------------------------------------------------------------------------------
!
! Command CALC_CHAMP
!
! Compute CHAM_UTIL for one occurrence
!
! --------------------------------------------------------------------------------------------------
!
! In  sdresu_in      : name of input result data-structure
! In  sdresu_out     : name of output result data-structure
! In  field_type     : type of field in result data-structure
! In  nume_field_out : order index for output field
! In  type_comp      : type of computation (CRITERE, NORME or FORMULE)
! In  crit           : type of criterion
! In  norm           : type of norm
! In  nb_form        : number of formulas
! In  name_form      : names of formulas
! In  nb_ordr        : number of order index in list_ordr
! In  list_ordr      : name of list of order
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jordr, numord
    integer :: iord, icmp
    integer :: jchsd
    integer :: ibid, jcmp, ichk, iret
    integer :: nb_node_new, nb_elem_new
    integer :: nb_elem, nb_node
    integer :: nb_cmp
    integer :: nb_cmp_resu
    integer :: vali(3)
    character(len=2) :: cnum
    character(len=4) :: type_field_in, type_field_out
    character(len=8) :: ma, model, nomgd
    character(len=16) :: typs, valk(3), name_field_out
    character(len=19) :: field_in_s, field_out_s
    character(len=19) :: ligrel_old, ligrel_new
    character(len=24) :: list_elem_new, work_out_val, wkcmp, list_elem_stor
    integer :: j_elem, j_resu
    character(len=24) :: field_in, field_out, field_out_sd
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    type_field_in = ' '
    type_field_out = ' '
    nomgd = ' '
    field_in_s = '&&CCCHUC.CHSIN'
    field_out_s = '&&CCCHUC.CHSOUT'
    field_out = '&&CCCHUC.CHOUT'
    wkcmp = '&&CCCHUC.CMPS'
    work_out_val = '&&CCCHUC.VAL'
    list_elem_new = '&&CCCHUC.LIST_NEW'
    list_elem_stor = '&&CCCHUC.LIST_STO'
    ligrel_old = 'NOT_INIT'
!
! - Size of output field
!
    call ccchci('NBCMP', type_comp, crit, norm, nb_form,&
                nb_cmp_resu)
    call wkvect(work_out_val, 'V V R', nb_cmp_resu, j_resu)
!
! - Access to result
!
    call jeveuo(list_ordr, 'L', jordr)
!
    do iord = 1, nb_ordr
!
! ----- Get input field
!
        numord = zi(jordr-1+iord)
        call rsexch(' ', sdresu_in, field_type, numord, field_in,&
                    iret)
        if (iret .ne. 0) then
            if (sdresu_in .eq. sdresu_out) then
                valk(1) = field_type
                valk(2) = sdresu_in
                vali(1) = numord
                call utmess('F', 'CHAMPS_6', nk=2, valk=valk, si=vali(1))
            else
                call rsexch(' ', sdresu_out, field_type, numord, field_in,&
                            iret)
                if (iret .ne. 0) then
                    valk(1) = field_type
                    valk(2) = sdresu_in
                    valk(3) = sdresu_out
                    vali(1) = numord
                    call utmess('F', 'CHAMPS_9', nk=3, valk=valk, si=vali(1))
                endif
            endif
        endif
!
! ----- Get input field properties
!
        if (iord .eq. 1) then
            call dismoi('NOM_GD', field_in, 'CHAMP', repk=nomgd)
            call dismoi('TYPE_CHAMP', field_in, 'CHAMP', repk=type_field_in)
            if (type_field_in .ne. 'NOEU') then
                call dismoi('NOM_LIGREL', field_in, 'CHAMP', repk=ligrel_old)
                call dismoi('NOM_MODELE', field_in, 'CHAMP', repk=model)
            endif
            ASSERT(type_field_in.ne.'CART' .and. type_field_in.ne.'RESL')
            call codent(nume_field_out, 'D0', cnum)
            name_field_out = 'UT'//cnum//'_'//type_field_in
            call dismoi('NOM_MAILLA', field_in, 'CHAMP', repk=ma)
        endif
!
! ----- Type of output field
!
        type_field_out = type_field_in
        if (type_comp .eq. 'NORME') then
            if (type_field_in .eq. 'NOEU') then
                call utmess('F', 'CHAMPS_17')
            endif
            ASSERT(type_field_in(1:2) .eq. 'EL')
            type_field_out = 'ELEM'
            name_field_out = 'UT'//cnum//'_ELEM'
        endif
!
! ----- Compute CHAM_UTIL
!
        if (type_field_in .eq. 'NOEU') then
!
            typs = 'CHAM_NO_S'
!
! --------- Create <CHAM_NO_S> from input field
!
            call cnocns(field_in, 'V', field_in_s)
            call jeveuo(field_in_s//'.CNSD', 'L', jchsd)
            nb_node = zi(jchsd-1+1)
            nb_cmp = zi(jchsd-1+2)
!
! --------- Create output field
!
            call wkvect(wkcmp, 'V V K8', nb_cmp_resu, jcmp)
            do icmp = 1, nb_cmp_resu
                call codent(icmp, 'G', cnum)
                zk8(jcmp-1+icmp) = 'X'//cnum
            enddo
            ASSERT(type_field_out .eq. 'NOEU')
            call cnscre(ma, 'NEUT_R', nb_cmp_resu, zk8(jcmp), 'V',&
                        field_out_s)
!
! --------- Compute on <CHAM_NO>
!
            call ccchuc_chamno(field_in_s, field_out_s, nb_node, nb_cmp, type_comp,&
                               crit, nb_form, name_form, nomgd, nb_cmp_resu,&
                               work_out_val, nb_node_new, ichk)
!
! --------- Print
!
            if (ichk .eq. 0) then
                vali(1) = numord
                vali(2) = nb_node_new
                vali(3) = nb_node
                call utmess('I', 'CHAMPS_10', ni=3, vali=vali)
            else
                call utmess('A', 'CHAMPS_15', si=numord)
            endif
!
        else
!
            typs = 'CHAM_ELEM_S'
            if (type_comp .eq. 'NORME') then
                call ccchuc_norm(norm, model, nomgd, field_in, type_field_in,&
                                 field_out)
            else
!
! ------------- Create <CHAM_ELEM_S> from input field
!
                call celces(field_in, 'V', field_in_s)
                call jeveuo(field_in_s//'.CESD', 'L', jchsd)
                nb_elem = zi(jchsd-1+1)
                nb_cmp = zi(jchsd-1+2)
!
! ------------- Work vector for element in output field
!
                call wkvect(list_elem_new, 'V V I', nb_elem, j_elem)
!
! ------------- Create output field
!
                call cescrm('V', field_out_s, type_field_out, 'NEUT_R', nb_cmp_resu,&
                            ' ', field_in_s)
!
! ------------- Compute on <CHAM_ELEM>
!
                call ccchuc_chamel(field_in_s, field_out_s, nb_elem, nb_cmp, type_comp,&
                                   crit, nb_form, name_form, nomgd, nb_cmp_resu,&
                                   work_out_val, list_elem_new, nb_elem_new, ichk)
!
! ------------- Print
!
                if (ichk .eq. 0) then
                    vali(1) = numord
                    vali(2) = nb_elem_new
                    vali(3) = nb_elem
                    call utmess('I', 'CHAMPS_8', ni=3, vali=vali)
                else
                    call utmess('A', 'CHAMPS_15', si=numord)
                endif
!
! ------------- Manage <LIGREL> - Create new if necessary
!
                call ccchuc_ligr(list_elem_stor, nb_elem, nb_elem_new, list_elem_new, ligrel_old,&
                                 ligrel_new)
            endif
        endif
!
! ----- Save in result
!
        call rsexch(' ', sdresu_out, name_field_out, numord, field_out_sd,&
                    iret)
        if (iret .ne. 100) then
            valk(1) = name_field_out
            valk(2) = sdresu_out
            call utmess('F', 'CHAMPS_14', nk=2, valk=valk)
        endif
        if (type_field_in .eq. 'NOEU') then
            call cnscno(field_out_s, ' ', 'UNUSED', 'G', field_out_sd,&
                        'F', iret)
        else
            if (type_comp .eq. 'NORME') then
                iret = 0
                call copisd('CHAMP_GD', 'G', field_out, field_out_sd)
                call detrsd('CHAMP', field_out)
            else
                call cescel(field_out_s, ligrel_new, ' ', ' ', 'NAN',&
                            ibid, 'G', field_out_sd, 'F', iret)
            endif
        endif
        ASSERT(iret.eq.0)
        call rsnoch(sdresu_out, name_field_out, numord)
!
        call detrsd(typs, field_in_s)
        call detrsd(typs, field_out_s)
        call jedetr(wkcmp)
        call jedetr(list_elem_new)
!
    end do
!
    call jedetr(list_elem_stor)
    call jedetr(work_out_val)
!
    call jedema()
!
end subroutine
