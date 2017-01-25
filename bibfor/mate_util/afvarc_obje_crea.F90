subroutine afvarc_obje_crea(jv_base, chmate, mesh, varc_cata, varc_affe)
!
use Material_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/wkvect.h"
#include "asterfort/alcart.h"
#include "asterfort/exisd.h"
#include "asterfort/jeveuo.h"
#include "asterfort/codent.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=1), intent(in) :: jv_base
    character(len=8), intent(in) :: chmate
    character(len=8), intent(in) :: mesh
    type(Mat_DS_VarcListCata), intent(in) :: varc_cata
    type(Mat_DS_VarcListAffe), intent(in) :: varc_affe
!
! --------------------------------------------------------------------------------------------------
!
! Material - External state variables (VARC)
!
! Create objects
!
! --------------------------------------------------------------------------------------------------
!
! In  jv_base          : JEVEUX base where to create objects
! In  chmate           : name of material field (CHAM_MATER)
! In  mesh             : name of mesh
! In  varc_cata        : datastructure for catalog of external state variables
! In  varc_affe        : datastructure for external state variables affected
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_varc_cmp, nb_affe_varc
    integer :: i_affe_varc, i_cmp
    integer :: indx_cata, iret
    integer, parameter :: nmxcmp=20
    character(len=8) :: varc_name, cmp_name
    character(len=24) :: cvnom, cvvar, cvgd, cvcmp
    character(len=19) :: cart1, cart2
    character(len=8), pointer :: v_cart_ncmp(:) => null()
    character(len=8), pointer :: v_cvnom(:) => null()
    character(len=8), pointer :: v_cvvar(:) => null()
    character(len=8), pointer :: v_cvgd(:) => null()
    character(len=8), pointer :: v_cvcmp(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nb_varc_cmp  = varc_affe%nb_varc_cmp
    nb_affe_varc = varc_affe%nb_affe_varc
    ASSERT(nb_affe_varc .ne. 0)
!
! - Create main objects
!
    cvnom = chmate//'.CVRCNOM'
    cvvar = chmate//'.CVRCVARC'
    cvgd  = chmate//'.CVRCGD'
    cvcmp = chmate//'.CVRCCMP'
    call wkvect(cvnom, jv_base//' V K8', nb_varc_cmp, vk8 = v_cvnom)
    call wkvect(cvvar, jv_base//' V K8', nb_varc_cmp, vk8 = v_cvvar)
    call wkvect(cvgd , jv_base//' V K8', nb_varc_cmp, vk8 = v_cvgd)
    call wkvect(cvcmp, jv_base//' V K8', nb_varc_cmp, vk8 = v_cvcmp)
!
! - Create fields
!
    do i_affe_varc = 1, nb_affe_varc
        indx_cata = varc_affe%list_affe_varc(i_affe_varc)%indx_cata
        varc_name = varc_cata%list_cata_varc(indx_cata)%name
        cart1     = chmate//'.'//varc_name//'.1'
        call exisd('CARTE', cart1, iret)
        if (iret .eq. 0) then
            call alcart(jv_base, cart1, mesh, 'NEUT_R')
        endif
        call jeveuo(cart1//'.NCMP', 'E', vk8  = v_cart_ncmp)
        cmp_name = 'X'
        do i_cmp = 1, nmxcmp
            call codent(i_cmp, 'G', cmp_name(2:8))
            v_cart_ncmp(i_cmp) = cmp_name
        end do
!
        cart2     = chmate//'.'//varc_name//'.2'
        call exisd('CARTE', cart2, iret)
        if (iret .eq. 0) then
            call alcart(jv_base, cart2, mesh, 'NEUT_K16')
        endif
        call jeveuo(cart2//'.NCMP', 'E', vk8  = v_cart_ncmp)
        cmp_name = 'Z'
        do i_cmp = 1, 7
            call codent(i_cmp, 'G', cmp_name(2:8))
            v_cart_ncmp(i_cmp) = cmp_name
        end do
    end do
!
end subroutine
