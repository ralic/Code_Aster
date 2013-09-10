subroutine ccchuc_chamel(field_in_s, field_out_s, nb_elem, nb_cmp, type_comp, &
                         crit, nb_form, name_form, name_gd, nb_cmp_resu, &
                         work_out_val, work_out_ele, nb_elem_out, ichk)
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
!
    character(len=19), intent(in) :: field_in_s
    character(len=19), intent(in) :: field_out_s
    integer, intent(in) :: nb_elem
    integer, intent(in) :: nb_cmp
    character(len=16), intent(in) :: type_comp
    character(len=16), intent(in) :: crit
    integer, intent(in) :: nb_form
    character(len=8), intent(in) :: name_form(nb_form)
    character(len=8), intent(in) :: name_gd
    integer, intent(in) :: nb_cmp_resu
    character(len=24), intent(in) :: work_out_val
    character(len=24), intent(in) :: work_out_ele
    integer, intent(out) :: ichk
    integer, intent(out) :: nb_elem_out
!
! --------------------------------------------------------------------------------------------------
!
! CALC_CHAMP - CHAM_UTIL
!
! Compute CHAM_UTIL on <CHAM_ELEM>
!
! --------------------------------------------------------------------------------------------------
!
! In  field_in_s   : name of <CHAM_ELEM_S> input field FROM which extract values
! In  field_out_s  : name of <CHAM_ELEM_S> output field IN which compute values
! In  nb_elem      : number of elements in input field
! In  nb_cmp       : number of components in input field
! In  type_comp    : type of computation (CRITERE or FORMULE)
! In  crit         : type of criterion
! In  nb_form      : number of formulas
! In  name_form    : names of formulas
! In  name_gd      : name of <GRANDEUR> of input field
! In  nb_cmp_resu  : number of components in output field
! In  work_out_val : working vector for output field (values)
! In  work_out_ele : working vector for output field (elements)
! Out ichk         : 0 if OK
! Out nb_elem_out  : number of elements in output field
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ima, ipt, isp, icmp, iad, nb_val_in
    integer :: nbpt, nbsp, nbcmp
    integer :: j_resu, j_elem
    character(len=24) :: work_val, work_cmp
    integer :: j_val, j_cmp
    integer :: jchsc, jchsv, jchsl, jchsd
    integer :: jchrd, jchrl, jchrv
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    ichk = 0
    nb_elem_out = 0
    work_val = '&&CCCHUC_CHAMEL.VAL'
    work_cmp = '&&CCCHUC_CHAMEL.CMP'
!
! - Access to input field
!
    call jeveuo(field_in_s//'.CESL', 'L', jchsl)
    call jeveuo(field_in_s//'.CESV', 'L', jchsv)
    call jeveuo(field_in_s//'.CESC', 'L', jchsc)
    call jeveuo(field_in_s//'.CESD', 'L', jchsd)
!
! - Access to output field
!
    call jeveuo(field_out_s//'.CESL', 'E', jchrl)
    call jeveuo(field_out_s//'.CESD', 'E', jchrd)
    call jeveuo(field_out_s//'.CESV', 'E', jchrv)
!
! - Access to output working vector
!
    call jeveuo(work_out_val, 'E', j_resu)
!
! - Access to work vector for element in out field
!
    call jeveuo(work_out_ele, 'E', j_elem)
!
! - Create working vectors
!
    call wkvect(work_val, 'V V R' , nb_cmp, j_val)
    call wkvect(work_cmp, 'V V K8', nb_cmp, j_cmp)
!
    do ima = 1, nb_elem
        ichk   = -1
        nbpt   = zi(jchsd-1+5+4*(ima-1)+1)
        nbsp   = zi(jchsd-1+5+4*(ima-1)+2)
        nbcmp  = zi(jchsd-1+5+4*(ima-1)+3)
        do ipt = 1, nbpt
            do isp = 1, nbsp
!
! ------------- Undefine values
!
                call jeundf(work_val)
                call jeundf(work_cmp)
!
! ------------- Set values
!
                nb_val_in = 0
                do icmp = 1, nbcmp
                    call cesexi('S', jchsd, jchsl, ima, ipt,&
                                isp, icmp, iad)
                    if (iad .gt. 0) then
                        nb_val_in = nb_val_in + 1
                        zr(j_val-1+nb_val_in)  = zr(jchsv-1+iad)
                        zk8(j_cmp-1+nb_val_in) = zk8(jchsc-1+icmp)
                    endif                 
                enddo
!
! ------------- Compute result
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
! ------------- Copy to output field
! 
                if (ichk.eq.0) then
                    do icmp = 1, nb_cmp_resu
                        call cesexi('S', jchrd, jchrl, ima, ipt,&
                                    isp, icmp, iad)
                        iad = -iad
                        zl(jchrl-1+iad) = .true.
                        zr(jchrv-1+iad) = zr(j_resu-1+icmp)
                    enddo
                endif
            enddo
        enddo
!
! ----- Add element computed
! 
        if (ichk.eq.0) then
            nb_elem_out = nb_elem_out + 1
            zi(j_elem-1+nb_elem_out) = ima
        endif    
    enddo
!
    call jedetr(work_val)
    call jedetr(work_cmp)
!
    call jedema()
!
end subroutine
