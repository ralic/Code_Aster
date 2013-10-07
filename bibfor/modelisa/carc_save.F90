subroutine carc_save(model, mesh, carcri, nb_cmp, list_vale)
!
    implicit none
!
#include "jeveux.h"
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
    character(len=19), intent(in) :: list_vale
    character(len=19), intent(in) :: carcri
    integer, intent(in) :: nb_cmp
!
! --------------------------------------------------------------------------------------------------
!
! <CARTE> CARCRI
!
! Save informations in <CARTE>
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh        : name of mesh
! In  model       : name of model
! In  carcri      : name of <CARTE> CARCRI
! In  nb_cmp      : number of components in <CARTE> CARCRI
! In  list_vale   : list of informations to save
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: list_elem_affe
    logical :: l_affe_all
    integer :: nb_elem_affe
    integer :: j_elem_affe
    character(len=16) :: keywordfact
    integer :: iocc, nbocc
    character(len=8) :: typmcl(2)
    character(len=16) :: motcle(2)
    integer :: nt
    integer :: j_lvalr, j_lvalk, j_cart_val
    character(len=16) :: algo_inte, rela_comp
    real(kind=8) :: iter_inte_maxi, resi_inte_rela, parm_theta, vale_pert_rela, algo_inte_r
    real(kind=8) :: resi_cplan_maxi, seuil, amplitude, taux_retour, parm_alpha
    integer :: type_matr_t, iter_inte_pas, iter_cplan_maxi
    logical :: plane_stress
!
! --------------------------------------------------------------------------------------------------
!
!
! - Initializations
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
    call jeveuo(carcri//'.VALV', 'E', j_cart_val)
!
! - Access to list
!
    call jeveuo(list_vale(1:19)//'.VALR', 'L', j_lvalr)
    call jeveuo(list_vale(1:19)//'.VALK', 'L', j_lvalk)
!
! - Loop on occurrences of COMPORTEMENT
!
    do iocc = 1, nbocc
!
! ----- Get infos
!
        type_matr_t     = int(zr(j_lvalr+13*(iocc-1) -1 + 2))
        parm_theta      = zr(j_lvalr+13*(iocc-1) -1 + 4)
        iter_inte_pas   = int(zr(j_lvalr+13*(iocc-1) -1 + 5))
        algo_inte_r     = zr(j_lvalr+13*(iocc-1) -1 + 6)
        vale_pert_rela  = zr(j_lvalr+13*(iocc-1) -1 + 7)
        resi_cplan_maxi = zr(j_lvalr+13*(iocc-1) -1 + 8)
        iter_cplan_maxi = int(zr(j_lvalr+13*(iocc-1) -1 + 9))
        seuil           = zr(j_lvalr+13*(iocc-1) -1 + 10)
        amplitude       = zr(j_lvalr+13*(iocc-1) -1 + 11)
        taux_retour     = zr(j_lvalr+13*(iocc-1) -1 + 12)
        parm_alpha      = zr(j_lvalr+13*(iocc-1) -1 + 13)
        rela_comp       = zk24(j_lvalk+2*(iocc-1) -1 + 1)(1:16)
        algo_inte       = zk24(j_lvalk+2*(iocc-1) -1 + 2)(1:16)
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
        zr(j_cart_val-1+1)  = iter_inte_maxi
        zr(j_cart_val-1+2)  = type_matr_t
        zr(j_cart_val-1+3)  = resi_inte_rela
        zr(j_cart_val-1+4)  = parm_theta
        zr(j_cart_val-1+5)  = iter_inte_pas
        zr(j_cart_val-1+6)  = algo_inte_r
        zr(j_cart_val-1+7)  = vale_pert_rela
        zr(j_cart_val-1+8)  = resi_cplan_maxi
        zr(j_cart_val-1+9)  = iter_cplan_maxi
        zr(j_cart_val-1+10) = seuil
        zr(j_cart_val-1+11) = amplitude
        zr(j_cart_val-1+12) = taux_retour
        zr(j_cart_val-1+13) = parm_alpha
!
! ----- Affect in <CARTE>
!
        if (l_affe_all) then
            call nocart(carcri, 1, nb_cmp)
        else
            call jeveuo(list_elem_affe, 'L', j_elem_affe)
            call nocart(carcri, 3, nb_cmp, mode = 'NUM', nma = nb_elem_affe,&
                        limanu = zi(j_elem_affe))
            call jedetr(list_elem_affe)
        endif
    enddo
!
    call jedetr(carcri//'.NCMP')
!
end subroutine

