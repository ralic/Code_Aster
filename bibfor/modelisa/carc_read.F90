subroutine carc_read(info_carc_valk, info_carc_valr, model)
!
    implicit none
!
#include "asterf_types.h"
#include "asterfort/comp_meca_l.h"
#include "asterfort/comp_meca_mod.h"
#include "asterfort/dismoi.h"
#include "asterc/getexm.h"
#include "asterc/getfac.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterc/lcalgo.h"
#include "asterc/lccree.h"
#include "asterc/lctest.h"
#include "asterc/umat_get_function.h"
#include "asterc/mfront_get_pointers.h"
#include "asterfort/assert.h"
#include "asterfort/comp_meca_rkit.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
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
    character(len=16), intent(out) :: info_carc_valk(:)
    real(kind=8), intent(out) :: info_carc_valr(:)
    character(len=8), intent(in), optional :: model
!
! --------------------------------------------------------------------------------------------------
!
! <CARTE> CARCRI
!
! Read informations from command file
!
! --------------------------------------------------------------------------------------------------
!
! IO  info_carc_valk : carcri informations (character)
! IO  info_carc_valr : carcri informations (real)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: keywordfact
    integer :: iocc, iret, nbocc, ndim, cpointer_nbvarext, cpointer_namevarext, cpointer_fct_ldc
    character(len=16) :: algo_inte, type_matr_tang, method, post_iter
    real(kind=8) :: parm_theta, vale_pert_rela
    real(kind=8) :: resi_deborst_max, seuil, amplitude, taux_retour, parm_alpha, resi_radi_rela
    integer :: type_matr_t, iter_inte_pas, iter_deborst_max,ipostiter
    character(len=16) :: rela_comp, rela_comp_py, kit_comp(9)
    character(len=16) :: rela_thmc, rela_hydr, rela_ther, rela_meca, rela_meca_py
    aster_logical :: l_kit_thm, l_mfront, l_umat
    character(len=16) :: texte(3), subr_name, nom_mod_mfront
    character(len=128) :: libr_name
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    nbocc = 0
    keywordfact = 'COMPORTEMENT'
    call getfac(keywordfact, nbocc)
!
! - Read informations
!
    do iocc = 1, nbocc
!
! ----- Get RELATION
!
        call getvtx(keywordfact, 'RELATION', iocc = iocc, scal = rela_comp)
        l_kit_thm = ((rela_comp(1:5).eq.'KIT_H') .or. (rela_comp(1:6).eq.'KIT_TH'))
!
! ----- Coding comportment (Python)
!
        call lccree(1, rela_comp, rela_comp_py)
!
! ----- Get ALGO_INTE
!
        call getvtx(keywordfact, 'ALGO_INTE', iocc = iocc, scal = algo_inte, nbret = iret)
        if (iret .eq. 0) then
            call lcalgo(rela_comp_py, algo_inte)
        else
            if (l_kit_thm) then
                call comp_meca_rkit(keywordfact, iocc, rela_comp, kit_comp)
                rela_thmc = kit_comp(1)
                rela_ther = kit_comp(2)
                rela_hydr = kit_comp(3)
                rela_meca = kit_comp(4)
            else
                rela_meca = rela_comp
            endif
            call lccree(1, rela_meca, rela_meca_py)
            call lctest(rela_meca_py, 'ALGO_INTE', algo_inte, iret)
            if (iret .eq. 0) then
                texte(1) = algo_inte
                texte(2) = 'ALGO_INTE'
                texte(3) = rela_comp
                call utmess('F', 'COMPOR1_45', nk = 3, valk = texte)
            endif
        endif
!
! ----- Get ITER_INTE_PAS
!
        iter_inte_pas = 0
        call getvis(keywordfact, 'ITER_INTE_PAS', iocc = iocc, scal = iter_inte_pas)
!
! ----- Get ITER_CPLAN_MAXI/RESI_CPLAN_MAXI/RESI_CPLAN_RELA (Deborst method)
!
        resi_deborst_max = 1.d-6
        iter_deborst_max = 1
        call getvis(keywordfact, 'ITER_CPLAN_MAXI', iocc = iocc, scal = iter_deborst_max)
        call getvr8(keywordfact, 'RESI_CPLAN_MAXI', iocc = iocc, scal = resi_deborst_max,&
                    nbret = iret)
        if (iret .ne. 0) then
            resi_deborst_max = -resi_deborst_max
        else
            call getvr8(keywordfact, 'RESI_CPLAN_RELA', iocc = iocc, scal = resi_deborst_max)
        endif
!
! ----- Get TYPE_MATR_TANG/VALE_PERT_RELA/SEUIL/AMPLITUDE/TAUX_RETOUR
!
        vale_pert_rela = 0.d0
        seuil = -1.d0
        amplitude = -1.d0
        taux_retour = -1.d0
        type_matr_t = 0
        type_matr_tang = ' '
        call getvtx(keywordfact, 'TYPE_MATR_TANG', iocc = iocc, scal = type_matr_tang,&
                    nbret = iret)
        if (iret .eq. 0) then
            type_matr_t = 0
        else
            if (type_matr_tang .eq. 'PERTURBATION') then
                type_matr_t = 1
                call getvr8(keywordfact, 'VALE_PERT_RELA', iocc = iocc, scal = vale_pert_rela)
            else if (type_matr_tang .eq. 'VERIFICATION') then
                type_matr_t = 2
                call getvr8(keywordfact, 'VALE_PERT_RELA', iocc = iocc, scal = vale_pert_rela)
            else if (type_matr_tang .eq. 'TANGENTE_SECANTE') then
                call getvr8(keywordfact, 'SEUIL', iocc = iocc, scal = seuil)
                call getvr8(keywordfact, 'AMPLITUDE', iocc = iocc, scal = amplitude)
                call getvr8(keywordfact, 'TAUX_RETOUR', iocc = iocc, scal = taux_retour)
            else
                ASSERT(.false.)
            endif
            call lctest(rela_comp_py, 'TYPE_MATR_TANG', type_matr_tang, iret)
            if (iret .eq. 0) then
                texte(1) = type_matr_tang
                texte(2) = rela_comp
                call utmess('F', 'COMPOR1_46', nk = 2, valk = texte)
            endif
        endif
!
! ----- Get TYPE_MATR_TANG/VALE_PERT_RELA/SEUIL/AMPLITUDE/TAUX_RETOUR - <IMPLEX>
!
        if (getexm(' ','METHODE') .eq. 1) then
            call getvtx(' ', 'METHODE', iocc = 0, scal = method, nbret = iret)
            if (iret .ne. 0) then
                if (method .eq. 'IMPLEX') then
                    if ((type_matr_t.ne.0) .and. (rela_comp.ne.'SANS')) then
                        texte(1) = type_matr_tang
                        texte(2) = method
                        call utmess('F', 'COMPOR1_46', nk = 2, valk = texte)
                    else
                        type_matr_t = 9
                    endif
                    call lctest(rela_comp_py, 'TYPE_MATR_TANG', method, iret)
                    if ((iret.eq.0) .and. (rela_comp.ne.'SANS')) then
                        texte(1) = type_matr_tang
                        texte(2) = method
                        call utmess('F', 'COMPOR1_46', nk = 2, valk = texte)
                    endif
                endif
            endif
        endif
!
! ----- Get PARM_THETA/PARM_ALPHA
!
        parm_theta = 1.d0
        parm_alpha = 1.d0
        call getvr8(keywordfact, 'PARM_THETA', iocc = iocc, scal = parm_theta)
        call getvr8(keywordfact, 'PARM_ALPHA', iocc = iocc, scal = parm_alpha)
!
! ----- Get RESI_RADI_RELA
!
        if (type_matr_t .eq. 0 .and. type_matr_tang .ne. 'TANGENTE_SECANTE') then
            call getvr8(keywordfact, 'RESI_RADI_RELA', iocc = iocc, scal = resi_radi_rela,&
                        nbret = iret)
            if (iret .ne. 0) then
                seuil = resi_radi_rela
            else
                seuil = -10.d0
            endif
        endif
!
! ----- Get POST_ITER
!
        ipostiter=0.d0
        if (type_matr_t .eq. 0 .and. type_matr_tang .ne. 'TANGENTE_SECANTE') then
            call getvtx(keywordfact, 'POST_ITER', iocc = iocc, scal = post_iter, nbret = iret)
            if (iret .eq. 1) then
               if (post_iter .eq. 'CRIT_RUPT') then
                   ipostiter = 1.d0
               endif
            endif
        endif
!
! ----- Get function pointers for mfront
!
        cpointer_nbvarext = 0
        cpointer_namevarext = 0
        cpointer_fct_ldc = 0
        call comp_meca_l(rela_comp, 'MFRONT', l_mfront)
        call comp_meca_l(rela_comp, 'UMAT', l_umat)
        if ( l_mfront ) then
            call getvtx(keywordfact, 'LIBRAIRIE', iocc = iocc, scal = libr_name)
            call getvtx(keywordfact, 'NOM_ROUTINE', iocc = iocc, scal = subr_name)
            if ( .not. present(model) ) then
! ------------- CALC_POINT_MAT case
                ndim = 3
                nom_mod_mfront = '_Tridimensional'
            else
! ------------- STAT_NON_LINE case
                call dismoi('DIM_GEOM', model, 'MODELE', repi = ndim)
                call comp_meca_mod(keywordfact, iocc, model, ndim, nom_mod_mfront)
            endif
            call mfront_get_pointers(libr_name, subr_name, nom_mod_mfront,&
                                     cpointer_nbvarext, cpointer_namevarext,&
                                     cpointer_fct_ldc)
        elseif ( l_umat ) then
            call getvtx(keywordfact, 'LIBRAIRIE', iocc = iocc, scal = libr_name)
            call getvtx(keywordfact, 'NOM_ROUTINE', iocc = iocc, scal = subr_name)
            call umat_get_function(libr_name, subr_name, cpointer_fct_ldc)
        endif
!
! ----- Save options in list
!
        info_carc_valr(18*(iocc-1) + 1) = 0.d0
        info_carc_valr(18*(iocc-1) + 2) = type_matr_t
        info_carc_valr(18*(iocc-1) + 3) = 0.d0
        info_carc_valr(18*(iocc-1) + 4) = parm_theta
        info_carc_valr(18*(iocc-1) + 5) = iter_inte_pas
        info_carc_valr(18*(iocc-1) + 6) = 0.d0
        info_carc_valr(18*(iocc-1) + 7) = vale_pert_rela
        info_carc_valr(18*(iocc-1) + 8) = resi_deborst_max
        info_carc_valr(18*(iocc-1) + 9) = iter_deborst_max
        info_carc_valr(18*(iocc-1) + 10) = seuil
        info_carc_valr(18*(iocc-1) + 11) = amplitude
        info_carc_valr(18*(iocc-1) + 12) = taux_retour
        info_carc_valr(18*(iocc-1) + 13) = ipostiter
        info_carc_valr(18*(iocc-1) + 14) = dble(cpointer_nbvarext)
        info_carc_valr(18*(iocc-1) + 15) = dble(cpointer_namevarext)
        info_carc_valr(18*(iocc-1) + 16) = dble(cpointer_fct_ldc)
        info_carc_valr(18*(iocc-1) + 18) = parm_alpha
        info_carc_valk(2*(iocc-1) + 1) = rela_comp
        info_carc_valk(2*(iocc-1) + 2) = algo_inte
    end do
!
    call jedema()
end subroutine
