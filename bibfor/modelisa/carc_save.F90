subroutine carc_save(model           , mesh            , carcri, nb_cmp, &
                     info_carc_valk, info_carc_valr)
!
    implicit none
!
#include "asterc/getexm.h"
#include "asterc/getfac.h"
#include "asterfort/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/exicp.h"
#include "asterfort/jedetr.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmdocv.h"
#include "asterfort/nocart.h"
#include "asterfort/reliem.h"
#include "asterfort/utlcal.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: model
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: carcri
    integer, intent(in) :: nb_cmp
    character(len=16), intent(in) :: info_carc_valk(:)
    real(kind=8)     , intent(in) :: info_carc_valr(:)
!
! --------------------------------------------------------------------------------------------------
!
! <CARTE> CARCRI
!
! Save informations in <CARTE>
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  carcri           : name of <CARTE> CARCRI
! In  nb_cmp           : number of components in <CARTE> CARCRI
! In  info_carc_valk : carcri informations (character)
! In  info_carc_valr : carcri informations (real)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: list_elem_affe
    logical :: l_affe_all
    integer :: nb_elem_affe
    integer, pointer :: p_elem_affe(:) => null()
    character(len=16) :: keywordfact
    integer :: iocc, nbocc
    character(len=8) :: typmcl(2)
    character(len=16) :: motcle(2)
    integer :: nt
    real(kind=8), pointer :: p_carc_valv(:) => null()
    character(len=16) :: algo_inte, rela_comp
    real(kind=8) :: iter_inte_maxi, resi_inte_rela, parm_theta, vale_pert_rela, algo_inte_r
    real(kind=8) :: resi_cplan_maxi, seuil, amplitude, taux_retour, parm_alpha
    integer :: type_matr_t, iter_inte_pas, iter_cplan_maxi
    logical :: plane_stress
!
! --------------------------------------------------------------------------------------------------
!
    nbocc       = 0
    keywordfact = 'COMPORTEMENT'
    call getfac(keywordfact, nbocc)
    list_elem_affe = '&&CARCSAVE.LIST'
    motcle(1)   = 'GROUP_MA'
    motcle(2)   = 'MAILLE'
    typmcl(1)   = 'GROUP_MA'
    typmcl(2)   = 'MAILLE'
!
! - Access to <CARTE>
!
    call jeveuo(carcri//'.VALV', 'E', vr = p_carc_valv)
!
! - Loop on occurrences of COMPORTEMENT
!
    do iocc = 1, nbocc
!
! ----- Get infos
!
        type_matr_t     = int(info_carc_valr(13*(iocc-1) + 2))
        parm_theta      =     info_carc_valr(13*(iocc-1) + 4)
        iter_inte_pas   = int(info_carc_valr(13*(iocc-1) + 5))
        algo_inte_r     =     info_carc_valr(13*(iocc-1) + 6)
        vale_pert_rela  =     info_carc_valr(13*(iocc-1) + 7)
        resi_cplan_maxi =     info_carc_valr(13*(iocc-1) + 8)
        iter_cplan_maxi = int(info_carc_valr(13*(iocc-1) + 9))
        seuil           =     info_carc_valr(13*(iocc-1) + 10)
        amplitude       =     info_carc_valr(13*(iocc-1) + 11)
        taux_retour     =     info_carc_valr(13*(iocc-1) + 12)
        parm_alpha      =     info_carc_valr(13*(iocc-1) + 13)
        rela_comp       =     info_carc_valk(2*(iocc-1) + 1)
        algo_inte       =     info_carc_valk(2*(iocc-1) + 2)
!
! ----- Get mesh
!
        call getvtx(keywordfact, 'TOUT',  iocc = iocc, nbret = nt)
        if (nt .ne. 0) then
            l_affe_all = .true.
        else
            l_affe_all = .false.
            call reliem(' ', mesh, 'NU_MAILLE', keywordfact, iocc, &
                        2, motcle, typmcl, list_elem_affe, nb_elem_affe)
            if (nb_elem_affe.eq.0) l_affe_all = .true.
        endif
!
! ----- Get ALGO_INTE - Plane stress
!
        plane_stress = exicp(model, l_affe_all, list_elem_affe, nb_elem_affe)
        if (plane_stress) then
            if (rela_comp .eq. 'VMIS_ECMI_LINE' .or. rela_comp .eq. 'VMIS_ECMI_TRAC' .or. &
                rela_comp .eq. 'VMIS_ISOT_LINE' .or. rela_comp .eq. 'VMIS_ISOT_TRAC') then
                algo_inte = 'SECANTE'
            endif
        endif
        call utlcal('NOM_VALE', algo_inte, algo_inte_r)
!
! ----- Get RESI_INTE_RELA/ITER_INTE_MAXI
!
        call nmdocv(keywordfact, iocc, algo_inte, 'RESI_INTE_RELA', resi_inte_rela)
        call nmdocv(keywordfact, iocc, algo_inte, 'ITER_INTE_MAXI', iter_inte_maxi)
!
! ----- Set in <CARTE>
!
        p_carc_valv(1)  = iter_inte_maxi
        p_carc_valv(2)  = type_matr_t
        p_carc_valv(3)  = resi_inte_rela
        p_carc_valv(4)  = parm_theta
        p_carc_valv(5)  = iter_inte_pas
        p_carc_valv(6)  = algo_inte_r
        p_carc_valv(7)  = vale_pert_rela
        p_carc_valv(8)  = resi_cplan_maxi
        p_carc_valv(9)  = iter_cplan_maxi
        p_carc_valv(10) = seuil
        p_carc_valv(11) = amplitude
        p_carc_valv(12) = taux_retour
        p_carc_valv(13) = parm_alpha
!
! ----- Affect in <CARTE>
!
        if (l_affe_all) then
            call nocart(carcri, 1, nb_cmp)
        else
            call jeveuo(list_elem_affe, 'L', vi = p_elem_affe)
            call nocart(carcri, 3, nb_cmp, mode = 'NUM', nma = nb_elem_affe,&
                        limanu = p_elem_affe)
            call jedetr(list_elem_affe)
        endif
    enddo
!
    call jedetr(carcri//'.NCMP')
!
end subroutine

