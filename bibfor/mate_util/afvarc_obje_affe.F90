subroutine afvarc_obje_affe(jv_base, chmate, mesh, model, varc_cata, varc_affe)
!
use Material_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8nnem.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/wkvect.h"
#include "asterfort/alcart.h"
#include "asterfort/exisd.h"
#include "asterfort/jeveuo.h"
#include "asterfort/codent.h"
#include "asterfort/gcncon.h"
#include "asterfort/mecact.h"
#include "asterfort/reliem.h"
#include "asterfort/nocart.h"
#include "asterfort/jedetr.h"
#include "asterfort/xvarc_temp.h"
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
    character(len=8), intent(in) :: model
    type(Mat_DS_VarcListCata), intent(in) :: varc_cata
    type(Mat_DS_VarcListAffe), intent(in) :: varc_affe
!
! --------------------------------------------------------------------------------------------------
!
! Material - External state variables (VARC)
!
! Affect values in objects
!
! --------------------------------------------------------------------------------------------------
!
! In  jv_base          : JEVEUX base where to create objects
! In  chmate           : name of material field (CHAM_MATER)
! In  mesh             : name of mesh
! In  model            : name of model
! In  varc_cata        : datastructure for catalog of external state variables
! In  varc_affe        : datastructure for external state variables affected
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8), parameter :: keywf_type(2) = (/'GROUP_MA', 'MAILLE  '/)
    character(len=16), parameter :: keywf_name(2)= (/'GROUP_MA', 'MAILLE  '/)
    character(len=24), parameter :: list_elem = '&&AFVARC.MES_MAILLES'
    integer :: nb_elem, nb_varc_cmp, nb_affe_varc, nb_cmp, nb_varc_cata
    integer :: i_affe_varc, i_cmp, i_varc_cmp
    integer :: indx_cata, jv_list_elem
    character(len=8) :: varc_name, phys_para
    character(len=24) :: cvnom, cvvar, cvgd, cvcmp
    character(len=19) :: cart1, cart2, cart_empty
    real(kind=8) :: vale_refe, empty_vale(10)
    character(len=8) :: empty_name(10), answer
    integer :: nbtou, nbgm1, nbm1
    real(kind=8), pointer :: v_cart_valv1(:) => null()
    character(len=16), pointer :: v_cart_valv2(:) => null()
    character(len=8), pointer :: v_cvnom(:) => null()
    character(len=8), pointer :: v_cvvar(:) => null()
    character(len=8), pointer :: v_cvgd(:) => null()
    character(len=8), pointer :: v_cvcmp(:) => null()
    character(len=8) :: type_affe, knumer, type_phys_para
    character(len=8) :: evol, evol_func
    character(len=16) :: evol_prol_l, evol_prol_r, vale_phys_para
    aster_logical, pointer :: v_active(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nb_varc_cmp  = varc_affe%nb_varc_cmp
    nb_affe_varc = varc_affe%nb_affe_varc
    nb_varc_cata = varc_cata%nb_varc
    ASSERT(nb_affe_varc .ne. 0)
!
! - Access to main objects
!
    cvnom = chmate//'.CVRCNOM'
    cvvar = chmate//'.CVRCVARC'
    cvgd  = chmate//'.CVRCGD'
    cvcmp = chmate//'.CVRCCMP'
    call jeveuo(cvnom, 'E', vk8 = v_cvnom)
    call jeveuo(cvvar, 'E', vk8 = v_cvvar)
    call jeveuo(cvgd , 'E', vk8 = v_cvgd)
    call jeveuo(cvcmp, 'E', vk8 = v_cvcmp)
!
! - Fill main objects
!
    AS_ALLOCATE(vl = v_active, size = nb_varc_cata)
    i_varc_cmp = 0
    do i_affe_varc = 1, nb_affe_varc
        indx_cata = varc_affe%list_affe_varc(i_affe_varc)%indx_cata
        varc_name = varc_cata%list_cata_varc(indx_cata)%name
        nb_cmp    = varc_cata%list_cata_varc(indx_cata)%nb_cmp
        phys_para = varc_cata%list_cata_varc(indx_cata)%type_phys_para
        if (.not.v_active(indx_cata)) then
            do i_cmp = 1, nb_cmp
                v_cvnom(i_cmp+i_varc_cmp) = &
                    varc_cata%list_cata_varc(indx_cata)%list_cmp(i_cmp)%varc_cmp
                v_cvcmp(i_cmp+i_varc_cmp) = &
                    varc_cata%list_cata_varc(indx_cata)%list_cmp(i_cmp)%phys_para_cmp
                v_cvvar(i_cmp+i_varc_cmp) = &
                    varc_name
                v_cvgd(i_cmp+i_varc_cmp) = &
                    phys_para
            end do
            i_varc_cmp = i_varc_cmp + nb_cmp
            v_active(indx_cata) = .true._1
        endif
    end do
    ASSERT(i_varc_cmp .eq. nb_varc_cmp)
    AS_DEALLOCATE(vl = v_active)
!
! - Fill fields
!
    i_varc_cmp = 0
    do i_affe_varc = 1, nb_affe_varc
        indx_cata = varc_affe%list_affe_varc(i_affe_varc)%indx_cata
        varc_name = varc_cata%list_cata_varc(indx_cata)%name
        nb_cmp    = varc_cata%list_cata_varc(indx_cata)%nb_cmp
        vale_refe = varc_affe%list_affe_varc(i_affe_varc)%vale_refe
        cart1     = chmate//'.'//varc_name//'.1'  
        cart2     = chmate//'.'//varc_name//'.2'
        call jeveuo(cart1//'.VALV', 'E', vr   = v_cart_valv1)
        call jeveuo(cart2//'.VALV', 'E', vk16 = v_cart_valv2)
        do i_cmp = 1, nb_cmp
            v_cart_valv1(i_cmp) = vale_refe
        end do
        type_affe      = varc_affe%list_affe_varc(i_affe_varc)%type_affe
        vale_phys_para = varc_affe%list_affe_varc(i_affe_varc)%vale_phys_para
        type_phys_para = varc_cata%list_cata_varc(indx_cata)%type_phys_para
        
        evol           = varc_affe%list_affe_varc(i_affe_varc)%evol
        evol_func      = varc_affe%list_affe_varc(i_affe_varc)%evol_func
        evol_prol_l    = varc_affe%list_affe_varc(i_affe_varc)%evol_prol_l
        evol_prol_r    = varc_affe%list_affe_varc(i_affe_varc)%evol_prol_r
        v_cart_valv2(1) = varc_name
        if (type_affe .eq. 'CHAMP') then
            v_cart_valv2(2) = 'CHAMP'
            v_cart_valv2(3) = vale_phys_para
            v_cart_valv2(4) = ' '
            v_cart_valv2(5) = ' '
            v_cart_valv2(6) = ' '
            v_cart_valv2(7) = ' '
        else if (type_affe .eq. 'EVOL') then
            v_cart_valv2(2) = 'EVOL'
            v_cart_valv2(3) = evol
            v_cart_valv2(4) = vale_phys_para
            v_cart_valv2(5) = evol_prol_l
            v_cart_valv2(6) = evol_prol_r
            v_cart_valv2(7) = evol_func
        else if (type_affe .eq. 'VIDE') then
            call gcncon('_', knumer)
            cart_empty = knumer
            ASSERT(nb_cmp .le. 10)
            empty_vale(1:nb_cmp) = r8nnem()
            empty_name(1:nb_cmp) = &
                varc_cata%list_cata_varc(indx_cata)%list_cmp(1:nb_cmp)%phys_para_cmp
            call mecact(jv_base, cart_empty, 'MAILLA', mesh, type_phys_para,&
                        ncmp=nb_cmp, lnomcmp=empty_name, vr=empty_vale)
            v_cart_valv2(2) = 'CHAMP'
            v_cart_valv2(3) = cart_empty(1:16)
            v_cart_valv2(4) = ' '
            v_cart_valv2(5) = ' '
            v_cart_valv2(6) = ' '
            v_cart_valv2(7) = ' '
        endif
! ----- Set values in CARTE
        call getvtx('AFFE_VARC', 'TOUT'    , iocc=i_affe_varc, scal=answer, nbret=nbtou)
        call getvtx('AFFE_VARC', 'GROUP_MA', iocc=i_affe_varc, nbval=0, nbret=nbgm1)
        call getvtx('AFFE_VARC', 'MAILLE'  , iocc=i_affe_varc, nbval=0, nbret=nbm1)
        if (nbgm1+nbm1 .eq. 0) then
            nbtou=1
        endif
        if (nbtou .ne. 0) then
            call nocart(cart1, 1, nb_cmp)
            call nocart(cart2, 1, 7)
        else
            call reliem(model, mesh, 'NU_MAILLE', 'AFFE_VARC', i_affe_varc,&
                        2, keywf_name, keywf_type, list_elem, nb_elem)
            if (nb_elem .ne. 0) then
                call jeveuo(list_elem, 'L', jv_list_elem)
                call nocart(cart1, 3, nb_cmp, mode='NUM', nma=nb_elem,&
                            limanu=zi(jv_list_elem))
                call nocart(cart2, 3, 7, mode='NUM', nma=nb_elem,&
                            limanu=zi(jv_list_elem))
                call jedetr(list_elem)
            endif
        endif
! ----- For XFEM
        call xvarc_temp(varc_name, type_affe   , evol, evol_prol_l, evol_prol_r,&
                        evol_func, nb_affe_varc, cart2)
    end do
!
end subroutine
