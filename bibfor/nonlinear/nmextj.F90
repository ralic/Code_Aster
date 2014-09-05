subroutine nmextj(field_type, nb_cmp , list_cmp , type_extr_cmp, poin_nume,&
                  spoi_nume , nb_vale, elem_nume, jcesd        , jcesv    ,&
                  jcesl     , jcesc  , vale_resu)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxliis.h"
#include "asterfort/nmextv.h"
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
    character(len=24), intent(in) :: field_type
    integer, intent(in) :: nb_cmp
    character(len=24), intent(in) :: list_cmp
    character(len=8), intent(in) :: type_extr_cmp
    integer, intent(in) :: poin_nume
    integer, intent(in):: elem_nume
    integer, intent(in) :: spoi_nume
    integer, intent(in) :: jcesd
    integer, intent(in) :: jcesv
    integer, intent(in) :: jcesl
    integer, intent(in) :: jcesc
    integer, intent(out) :: nb_vale
    real(kind=8), intent(out) :: vale_resu(*)
!
! --------------------------------------------------------------------------------------------------
!
! *_NON_LINE - Field extraction datastructure
!
! Extract value(s) at Gauss point
!
! --------------------------------------------------------------------------------------------------
!
! In  elem_nume        : index of element
! In  poin_nume        : index of point
! In  spoi_nume        : index of subpoint
! In  jcesd            : Jeveux adress to CHAM_ELEM_S.CESD object
! In  jcesv            : Jeveux adress to CHAM_ELEM_S.CESV object
! In  jcesl            : Jeveux adress to CHAM_ELEM_S.CESL object
! In  jcesc            : Jeveux adress to CHAM_ELEM_S.CESC object
! In  field_type       : type of field (name in results datastructure)
! In  nb_cmp           : number of components
! In  list_cmp         : name of object contains list of components
! In  type_extr_cmp    : type of extraction for components
! Out vale_resu        : list of result values
! Out nb_vale          : number of result values (one if function)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_para_maxi
    parameter    (nb_para_maxi=20)
    character(len=8) :: v_cmp_name(nb_para_maxi)
    real(kind=8) :: v_cmp_vale(nb_para_maxi)
    integer :: nb_cmp_vale, nb_cmp_maxi
    integer :: i_cmp, iret, i_cmp_vale, i_cmp_maxi, i_cmp_r, i_vari
    integer :: iad
    character(len=8) :: cmp_name, vari_name
    character(len=8), pointer :: v_list_cmp(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    i_cmp_vale = 1
    nb_cmp_maxi = zi(jcesd+4)
    ASSERT(nb_cmp.le.nb_para_maxi)
!
! - Get name of components
!
    call jeveuo(list_cmp, 'L', vk8 = v_list_cmp)
    do i_cmp = 1, nb_cmp
        v_cmp_name(i_cmp) = v_list_cmp(i_cmp)
    end do
!
! - Get value of components
!
    do i_cmp = 1, nb_cmp
        cmp_name = v_cmp_name(i_cmp)
        if (field_type(1:4) .eq. 'VARI') then
            vari_name = cmp_name(2:8)//' '
            call lxliis(vari_name, i_vari, iret)
        else
            i_vari = 0
        endif
        if (field_type(1:4) .eq. 'VARI') then
            i_cmp_r = i_vari
        else
            do i_cmp_maxi = 1, nb_cmp_maxi
                if (cmp_name .eq. zk8(jcesc-1+i_cmp_maxi)) then
                    i_cmp_r = i_cmp_maxi
                endif
            end do
        endif
        call cesexi('C', jcesd, jcesl, elem_nume, poin_nume,&
                    spoi_nume, i_cmp_r, iad)
        if (iad .gt. 0) then
            v_cmp_vale(i_cmp_vale) = zr(jcesv+iad-1)
            i_cmp_vale = i_cmp_vale + 1
        endif
    end do
    nb_cmp_vale = i_cmp_vale - 1
!
! - Evaluation
!
    call nmextv(nb_cmp_vale, type_extr_cmp, v_cmp_name, v_cmp_vale, nb_vale,&
                vale_resu)
    ASSERT(nb_vale.le.nb_cmp)
!
end subroutine
