subroutine ccchuc_norm(norm, model, name_gd, field_in, type_field_in,&
                       field_out)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/calc_coor_elga.h"
#include "asterfort/calc_norm_coef.h"
#include "asterfort/calc_norm_elem.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/chpchd.h"
#include "asterfort/chsut1.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/exisd.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/megeom.h"
#include "asterfort/nopar2.h"
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
!
    character(len=16), intent(in) :: norm
    character(len=8), intent(in) :: model
    character(len=8), intent(in) :: name_gd
    character(len=19), intent(in) :: field_in
    character(len=4), intent(in) :: type_field_in
    character(len=19), intent(in) :: field_out
!
! --------------------------------------------------------------------------------------------------
!
! CALC_CHAMP - CHAM_UTIL - NORME
!
! Compute NORME 
!
! --------------------------------------------------------------------------------------------------
!
! In  norm          : type of norm
! In  model         : name of model
! In  name_gd       : name of <GRANDEUR> of input field
! In  type_field_in : type of input field
! In  field_in      : name of <CHAM_ELEM> input field FROM which extract values
! In  field_out     : name of <CHAM_ELEM> output field IN which compute values
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_cmp_max
    parameter (nb_cmp_max=30)
    integer :: iexist
    character(len=19) :: ligrel, celmod
    character(len=19) :: field_in_s, field_neut_s, field_neut, field_neut_mod
    integer ::  jchsc
    integer :: nb_elem, nb_cmp, nb_cmp_act
    character(len=24) :: list_cmp, list_cmp_neut, valk(3)
    integer :: j_liscmp_in, j_liscmp_ne
    character(len=24) :: chcoef, chgaus, chgeom, chcalc
    integer :: nb_coef_user
    real(kind=8) :: coef_user(1)
    character(len=4) :: ki
    integer :: icmp, nncp, iret, ibid
    character(len=16) :: option
    character(len=8) :: nopar
    integer, pointer :: cesd(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    ligrel = model//'.MODELE'
    field_in_s = '&&CCCHUC_NORM.FIELS'
    field_neut_s = '&&CCCHUC_NORM.NEUTS'
    field_neut = '&&CCCHUC_NORM.NEUTR'
    field_neut_mod = '&&CCCHUC_NORM.NEUTM'
    chcoef = '&&CCCHUC_NORM.CHCOEF'
    chgaus = '&&CCCHUC_NORM.CHGAUS'
    chcalc = '&&CCCHUC_NORM.CHCALC'
    list_cmp_neut = '&&CCCHUC_NORM.CMPN'
    nb_coef_user = 0
    coef_user(1) = 0.d0
!
! - Compute <CARTE> with informations on Gauss points 
!
    call exisd('CHAMP', chgaus, iexist)
    if (iexist .eq. 0) then
        call megeom(model, chgeom)
        call calc_coor_elga(ligrel, chgeom, chgaus)
    endif
!
! - Create <CHAM_ELEM_S> from input field
!
    call celces(field_in, 'V', field_in_s)
    call jeveuo(field_in_s//'.CESD', 'L', vi=cesd)
    call jeveuo(field_in_s//'.CESC', 'L', jchsc)
    nb_elem = cesd(1)
    nb_cmp = cesd(2)
    list_cmp = field_in_s//'.CESC'
!
! - <NEUT_R> components
!
    call wkvect(list_cmp_neut, 'V V K8', nb_cmp, j_liscmp_ne)
    do icmp = 1, nb_cmp
        call codent(icmp, 'G', ki)
        zk8(j_liscmp_ne-1+icmp)='X'//ki(1:len(ki))
    enddo
!
! - Construction of <CARTE> of <NEUT_R> by selection of components
!
    call calc_norm_coef(model, name_gd, nb_cmp_max, nb_cmp, norm,&
                        'NORM', list_cmp, nb_coef_user, coef_user, chcoef,&
                        chcalc, nb_cmp_act)
!
! - Transform input field in NEUT_R
!
    call jeveuo(list_cmp, 'L', j_liscmp_in)
    call chsut1(field_in_s, 'NEUT_R', nb_cmp, zk8(j_liscmp_in), zk8(j_liscmp_ne),&
                'V', field_neut_s)
!
! - Convert CHAMELEM_S field to CHAMELEM field
!
    if (type_field_in .eq. 'ELNO') then
        option = 'TOU_INI_ELNO'
    else if (type_field_in .eq. 'ELGA') then
        option = 'TOU_INI_ELGA'
    else
        ASSERT(.false.)
    endif
    nopar = nopar2(option,'NEUT_R','OUT')
    call cescel(field_neut_s, ligrel, option, nopar, 'OUI',&
                nncp, 'V', field_neut, 'F', iret)
    ASSERT(iret.eq.0) 
!
! - Change type of field
!
    if (norm .eq. 'L2') then
        option = 'NORME_L2'
    else if (norm.eq.'FROBENIUS') then
        option = 'NORME_FROB'
    else
        ASSERT(.false.)
    endif
    if (type_field_in .eq. 'ELGA') then
        field_neut_mod = field_neut
    else
        nopar = 'PCHAMPG'
        celmod = '&&PENORM.CELMOD'
        call alchml(ligrel, option, nopar, 'V', celmod,&
                    ibid, ' ')
        if (ibid .ne. 0) then
            valk(1) = ligrel
            valk(2) = nopar
            valk(3) = option
            call utmess('F', 'UTILITAI3_23', nk=3, valk=valk)
        endif
        call chpchd(field_neut, 'ELGA', celmod, 'OUI', 'V',&
                    field_neut_mod)
        call detrsd('CHAMP', celmod)
    endif
!
! - Compute Norm (integration on finite element)
!
    call calc_norm_elem(norm, ligrel, chcoef, chgaus, chcalc,&
                        field_neut_mod, field_out)
!
    call jedetr(chcoef)
    call jedetr(list_cmp_neut)
    call detrsd('CHAM_ELEM_S', field_in_s)
    call detrsd('CHAM_ELEM_S', field_neut_s)
    call detrsd('CHAM_ELEM', field_neut)
    call detrsd('CHAM_ELEM', field_neut_mod)
!
end subroutine
