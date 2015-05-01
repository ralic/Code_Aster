subroutine ccchuc_chamno(field_in_s, field_out_s, nb_node, nb_cmp, type_comp, &
                         crit, nb_form, name_form, name_gd, nb_cmp_resu, work_out_val,&
                         nb_node_out, ichk)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/ccchcf.h"
#include "asterfort/ccchcr.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeundf.h"
#include "asterfort/jeveuo.h"
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
!
    character(len=19), intent(in) :: field_in_s
    character(len=19), intent(in) :: field_out_s
    integer, intent(in) :: nb_node 
    integer, intent(in) :: nb_cmp
    character(len=16), intent(in) :: type_comp
    character(len=16), intent(in) :: crit
    integer, intent(in) :: nb_form
    character(len=8), intent(in) :: name_form(nb_form)
    character(len=8), intent(in) :: name_gd
    integer, intent(in) :: nb_cmp_resu
    character(len=24), intent(in) :: work_out_val
    integer, intent(out) :: ichk
    integer, intent(out) :: nb_node_out
!
! --------------------------------------------------------------------------------------------------
!
! CALC_CHAMP - CHAM_UTIL
!
! Compute CHAM_UTIL on <CHAM_NO>
!
! --------------------------------------------------------------------------------------------------
!
! In  field_in_s   : name of <CHAM_NO_S> input field FROM which extract values
! In  field_out_s  : name of <CHAM_NO_S> output field IN which compute values
! In  nb_node      : number of nodes in input field
! In  nb_cmp       : number of components in input field
! In  type_comp    : type of computation (CRITERE or FORMULE)
! In  crit         : type of criterion
! In  nb_form      : number of formulas
! In  name_form    : names of formulas
! In  name_gd      : name of <GRANDEUR> of input field
! In  nb_cmp_resu  : number of components in output field
! In  work_out_val : working vector for output field (values)
! Out ichk         : 0 if OK
! Out nb_node_out  : number of nodes in output field
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ino, icmp, nb_val_in
    integer :: j_resu
    character(len=19) :: work_val, work_cmp
    integer :: j_val, j_cmp
    integer ::   jchsl
    integer :: jchrl
    character(len=8), pointer :: cnsc(:) => null()
    real(kind=8), pointer :: chrv(:) => null()
    real(kind=8), pointer :: chsv(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    ichk = 0
    nb_node_out = 0
    work_val = '&&CCCHUC_CHAMNO.VAL'
    work_cmp = '&&CCCHUC_CHAMNO.CMP'
!
! - Access to input field
!
    call jeveuo(field_in_s//'.CNSC', 'L', vk8=cnsc)
    call jeveuo(field_in_s//'.CNSV', 'L', vr=chsv)
    call jeveuo(field_in_s//'.CNSL', 'L', jchsl)
!
! - Access to output field
!
    call jeveuo(field_out_s//'.CNSL', 'E', jchrl)
    call jeveuo(field_out_s//'.CNSV', 'E', vr=chrv)
!
! - Access to output working vector
!
    call jeveuo(work_out_val, 'E', j_resu)
!
! - Create working vectors
!
    call wkvect(work_val, 'V V R' , nb_cmp, j_val)
    call wkvect(work_cmp, 'V V K8', nb_cmp, j_cmp)
!
    do ino = 1, nb_node
!
! ----- Undefine values
!
        call jeundf(work_val)
        call jeundf(work_cmp)
!
! ----- Set values
!
        nb_val_in = 0
        do icmp = 1, nb_cmp
            if (zl(jchsl-1+(ino-1)*nb_cmp+icmp)) then
                nb_val_in = nb_val_in + 1
                zr(j_val-1+nb_val_in)  = chsv((ino-1)*nb_cmp+icmp)
                zk8(j_cmp-1+nb_val_in) = cnsc(icmp)
            endif
        enddo
!
! ----- Compute result
!
        if (type_comp .eq. 'CRITERE') then
            ASSERT(nb_cmp_resu .eq. 1)
            call ccchcr(crit, name_gd, nb_val_in, zr(j_val), zk8(j_cmp),&
                        nb_cmp_resu, zr(j_resu), ichk)
        elseif (type_comp .eq. 'FORMULE') then
            ASSERT(nb_cmp_resu .eq. nb_form)
            call ccchcf(name_form, nb_val_in, zr(j_val), zk8(j_cmp), nb_cmp_resu,&
                        zr(j_resu), ichk)
        else
            ASSERT(.false.)
        endif
!
! ----- Copy to output field
! 
        if (ichk.eq.0) then
            nb_node_out = nb_node_out + 1
            do icmp = 1, nb_cmp_resu
                zl(jchrl-1+(ino-1)*nb_cmp_resu+icmp) = .true.
                chrv((ino-1)*nb_cmp_resu+icmp) = zr(j_resu-1+icmp)
            enddo
        endif
    enddo
!
    call jedetr(work_val)
    call jedetr(work_cmp)
!
    call jedema()
!
end subroutine
