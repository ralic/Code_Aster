subroutine surfc2(sdcont, mesh)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/cfnumm.h"
#include "asterfort/jedetr.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/mminfm.h"
#include "asterfort/mminfr.h"
#include "asterfort/utmess.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: sdcont
    character(len=8), intent(in) :: mesh
!
! --------------------------------------------------------------------------------------------------
!
! DEFI_CONTACT
!
! Print debug for continue formulation
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont           : name of contact concept (DEFI_CONTACT)
! In  mesh             : name of mesh
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_cont_zone, nb_cont_node, nb_cont_elem
    real(kind=8) :: tole_interp
    aster_logical :: l_veri
    integer :: i_zone
    character(len=8) :: elem_name
    integer :: elem_nume, elem_indx, i_elem_slav, jdecme
    integer :: nb_elem_slav
    integer :: ndexfr, nb_poin_inte, vali(3)
    integer :: type_inte, algo_cont, algo_frot, cont_init
    integer :: i_axi, i_gliss, i_node_excl, i_frot_excl, dire_excl_frot_i
    real(kind=8) :: coef_cont, coef_frot, coef_coul_frot, seuil_init, seuil_auto
    character(len=24) :: sdcont_defi
    character(len=24) :: sdcont_paraci
    integer, pointer :: v_sdcont_paraci(:) => null()
    character(len=24) :: sdcont_caracf
    real(kind=8), pointer :: v_sdcont_caracf(:) => null()
    integer :: zcmcf
!
! --------------------------------------------------------------------------------------------------
!
    sdcont_defi = sdcont(1:8)//'.CONTACT'
!
! - Datastructure for contact definition
!
    sdcont_paraci = sdcont_defi(1:16)//'.PARACI'
    sdcont_caracf = sdcont_defi(1:16)//'.CARACF'
    call jeveuo(sdcont_paraci, 'L', vi=v_sdcont_paraci)
    call jeveuo(sdcont_caracf, 'L', vr=v_sdcont_caracf)
    zcmcf = cfmmvd('ZCMCF')
!
! - Parameters
!
    nb_cont_zone = cfdisi(sdcont_defi,'NZOCO')
    nb_cont_elem = cfdisi(sdcont_defi,'NMACO')
    nb_cont_node = cfdisi(sdcont_defi,'NNOCO')
!
! - Print
!
    call utmess('I', 'CONTACTDEFI0_10')
!
! - Parameters: constants
!
    call utmess('I', 'CONTACTDEFI0_11')
    i_axi = v_sdcont_paraci(16)
    call utmess('I', 'CONTACTDEFI0_12',si=i_axi)
!
! - Parameters: variables
!
    call utmess('I', 'CONTACTDEFI0_21')
    do i_zone = 1, nb_cont_zone
        call utmess('I', 'CONTACTDEFI0_22',si=i_zone)
        l_veri = mminfl(sdcont_defi,'VERIF', i_zone)
        if (l_veri) then
            call utmess('I', 'CONTACTDEFI0_23')
            tole_interp = mminfr(sdcont_defi, 'TOLE_INTERP', i_zone)
            call utmess('I', 'CONTACTDEFI0_24',sr=tole_interp)
        else
            call utmess('I', 'CONTACTDEFI0_25')
            type_inte        = nint(v_sdcont_caracf(zcmcf*(i_zone-1)+1))
            coef_cont        = v_sdcont_caracf(zcmcf*(i_zone-1)+2)
            algo_cont        = nint(v_sdcont_caracf(zcmcf*(i_zone-1)+3))
            coef_frot        = v_sdcont_caracf(zcmcf*(i_zone-1)+4)
            algo_frot        = nint(v_sdcont_caracf(zcmcf*(i_zone-1)+5))
            coef_coul_frot   = v_sdcont_caracf(zcmcf*(i_zone-1)+6)
            seuil_init       = v_sdcont_caracf(zcmcf*(i_zone-1)+7)
            seuil_auto       = v_sdcont_caracf(zcmcf*(i_zone-1)+13)
            cont_init        = nint(v_sdcont_caracf(zcmcf*(i_zone-1)+8))
            i_gliss          = nint(v_sdcont_caracf(zcmcf*(i_zone-1)+9))
            i_node_excl      = nint(v_sdcont_caracf(zcmcf*(i_zone-1)+10))
            i_frot_excl      = nint(v_sdcont_caracf(zcmcf*(i_zone-1)+11))
            dire_excl_frot_i = nint(v_sdcont_caracf(zcmcf*(i_zone-1)+12))
            call utmess('I', 'CONTACTDEFI0_26', si = type_inte)
            call utmess('I', 'CONTACTDEFI0_27', sr = coef_cont)
            call utmess('I', 'CONTACTDEFI0_28', si = algo_cont)
            call utmess('I', 'CONTACTDEFI0_29', sr = coef_frot)
            call utmess('I', 'CONTACTDEFI0_30', si = algo_frot)
            call utmess('I', 'CONTACTDEFI0_31', sr = coef_coul_frot)
            call utmess('I', 'CONTACTDEFI0_32', sr = seuil_init)
            call utmess('I', 'CONTACTDEFI0_33', sr = seuil_auto)
            call utmess('I', 'CONTACTDEFI0_34', si = cont_init)
            call utmess('I', 'CONTACTDEFI0_35', si = i_gliss)
            call utmess('I', 'CONTACTDEFI0_36', si = i_node_excl)
            call utmess('I', 'CONTACTDEFI0_37', si = i_frot_excl)
            call utmess('I', 'CONTACTDEFI0_38', si = dire_excl_frot_i)
        endif
    end do
!
! - Slave elements
!
    call utmess('I', 'CONTACTDEFI0_40')
    do i_zone = 1, nb_cont_zone
        call utmess('I', 'CONTACTDEFI0_22',si=i_zone)
        jdecme        = mminfi(sdcont_defi,'JDECME', i_zone)
        nb_elem_slav  = mminfi(sdcont_defi,'NBMAE' , i_zone)
        do i_elem_slav = 1, nb_elem_slav
            elem_indx = jdecme + i_elem_slav
            call mminfm(elem_indx, sdcont_defi, 'NPTM', nb_poin_inte)
            call cfnumm(sdcont_defi, elem_indx, elem_nume)
            call jenuno(jexnum(mesh(1:8)//'.NOMMAI', elem_nume), elem_name)
            call mminfm(elem_indx, sdcont_defi, 'NDEXFR', ndexfr)
            vali(1) = i_zone
            vali(2) = nb_poin_inte
            vali(3) = ndexfr
            call utmess('I', 'CONTACTDEFI0_41',sk=elem_name, ni=3, vali = vali)
        end do
    end do
!
end subroutine
