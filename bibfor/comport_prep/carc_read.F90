subroutine carc_read(ds_compor_para, model_)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/comp_meca_mod.h"
#include "asterfort/dismoi.h"
#include "asterc/getexm.h"
#include "asterfort/assert.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterc/lcalgo.h"
#include "asterc/lccree.h"
#include "asterc/lctest.h"
#include "asterc/lcdiscard.h"
#include "asterc/umat_get_function.h"
#include "asterc/mfront_get_pointers.h"
#include "asterc/mfront_get_number_of_internal_state_variables.h"
#include "asterc/mfront_get_internal_state_variables.h"
#include "asterc/mfront_set_outofbounds_policy.h"
#include "asterfort/comp_meca_l.h"
#include "asterfort/comp_meca_rkit.h"
#include "asterfort/comp_read_exte.h"
#include "asterfort/mfront_get_libname.h"
#include "asterfort/mfront_get_function.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/deprecated_algom.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    type(NL_DS_ComporParaPrep), intent(inout) :: ds_compor_para
    character(len=8), intent(in), optional :: model_
!
! --------------------------------------------------------------------------------------------------
!
! Preparation of comportment (mechanics)
!
! Read informations from command file
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_compor_para   : datastructure to prepare parameters for constitutive laws
! In  model            : name of model
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: keywordfact=' '
    integer :: i_comp=0, iret=0, nb_comp=0, model_dim=0
    integer :: cptr_nbvarext=0, cptr_namevarext=0, cptr_fct_ldc=0
    integer :: cptr_matprop=0, cptr_nbprop=0, nbval = 0
    character(len=16) :: algo_inte=' ', type_matr_tang=' ', method=' ', post_iter=' ', post_incr=' '
    real(kind=8) :: parm_theta=0.d0, vale_pert_rela=0.d0
    real(kind=8) :: resi_deborst_max=0.d0, seuil=0.d0, amplitude=0.d0, taux_retour=0.d0
    real(kind=8) :: parm_alpha=0.d0, resi_radi_rela=0.d0
    integer :: type_matr_t=0, iter_inte_pas=0, iter_deborst_max=0, nbtest=0
    integer :: ipostiter=0, ipostincr=0
    character(len=8) :: mesh = ' '
    character(len=16) :: rela_comp=' ', rela_comp_py=' '
    character(len=16) :: veri_b=' '
    character(len=16) :: kit_comp(9) = (/' ',' ',' ',' ',' ',' ',' ',' ',' '/)
    character(len=16):: rela_thmc=' ', rela_hydr=' ', rela_ther=' ', rela_meca=' ', rela_meca_py=' '
    aster_logical :: l_kit_thm=.false._1, l_mfront=.false._1
    aster_logical :: l_mfront_offi=.false._1, l_umat=.false._1, l_kit = .false._1
    character(len=16) :: texte(3)=(/ ' ',' ',' '/), model_mfront=' '
    character(len=80), pointer :: int_var(:) => null()
    character(len=255) :: libr_name=' ', subr_name=' '
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    keywordfact = 'COMPORTEMENT'
    nb_comp     = ds_compor_para%nb_comp
!
! - Read informations
!
    do i_comp = 1, nb_comp
!
! ----- Get RELATION
!
        call getvtx(keywordfact, 'RELATION', iocc = i_comp, scal = rela_comp)
!
! ----- Detection of specific cases
!
        call comp_meca_l(rela_comp, 'KIT'    , l_kit)
        call comp_meca_l(rela_comp, 'KIT_THM', l_kit_thm)
!
! ----- Coding comportment (Python)
!
        call lccree(1, rela_comp, rela_comp_py)
!
! ----- Get ALGO_INTE
!
        call getvtx(keywordfact, 'ALGO_INTE', iocc = i_comp, scal = algo_inte, nbret = iret)
        if (iret .eq. 0) then
            call lcalgo(rela_comp_py, algo_inte)
        else
            if (l_kit_thm) then
                call comp_meca_rkit(keywordfact, i_comp, rela_comp, kit_comp)
                rela_thmc = kit_comp(1)
                rela_ther = kit_comp(2)
                rela_hydr = kit_comp(3)
                rela_meca = kit_comp(4)
            else
                rela_meca = rela_comp
            endif
            call lccree(1, rela_meca, rela_meca_py)
            call lctest(rela_meca_py, 'ALGO_INTE', algo_inte, iret)
            call lcdiscard(rela_meca_py)
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
        call getvis(keywordfact, 'ITER_INTE_PAS', iocc = i_comp, scal=iter_inte_pas, nbret=iret)
        if (iret .eq. 0) then
            iter_inte_pas = 0
        endif
!
! ----- Get ITER_CPLAN_MAXI/RESI_CPLAN_MAXI/RESI_CPLAN_RELA (Deborst method)
!
        resi_deborst_max = 1.d-6
        iter_deborst_max = 1
        call getvis(keywordfact, 'ITER_CPLAN_MAXI', iocc = i_comp, scal = iter_deborst_max)
        call getvr8(keywordfact, 'RESI_CPLAN_MAXI', iocc = i_comp, scal = resi_deborst_max,&
                    nbret = iret)
        if (iret .ne. 0) then
            resi_deborst_max = -resi_deborst_max
        else
            call getvr8(keywordfact, 'RESI_CPLAN_RELA', iocc = i_comp, scal = resi_deborst_max)
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
        call getvtx(keywordfact, 'TYPE_MATR_TANG', iocc = i_comp, scal = type_matr_tang,&
                    nbret = iret)
        if (iret .eq. 0) then
            type_matr_t = 0
        else
            if (type_matr_tang .eq. 'PERTURBATION') then
                type_matr_t = 1
                call getvr8(keywordfact, 'VALE_PERT_RELA', iocc = i_comp, scal = vale_pert_rela)
            else if (type_matr_tang .eq. 'VERIFICATION') then
                type_matr_t = 2
                call getvr8(keywordfact, 'VALE_PERT_RELA', iocc = i_comp, scal = vale_pert_rela)
            else if (type_matr_tang .eq. 'TANGENTE_SECANTE') then
                call deprecated_algom('TANG_SECA')
                call getvr8(keywordfact, 'SEUIL', iocc = i_comp, scal = seuil)
                call getvr8(keywordfact, 'AMPLITUDE', iocc = i_comp, scal = amplitude)
                call getvr8(keywordfact, 'TAUX_RETOUR', iocc = i_comp, scal = taux_retour)
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
        call getvr8(keywordfact, 'PARM_THETA', iocc = i_comp, scal = parm_theta)
        call getvr8(keywordfact, 'PARM_ALPHA', iocc = i_comp, scal = parm_alpha)
!
! ----- Get RESI_RADI_RELA
!
        if (type_matr_t .eq. 0 .and. type_matr_tang .ne. 'TANGENTE_SECANTE') then
            call getvr8(keywordfact, 'RESI_RADI_RELA', iocc = i_comp, scal = resi_radi_rela,&
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
        ipostiter = 0
        if (getexm('COMPORTEMENT','POST_ITER') .eq. 1) then
            post_iter = ' '
            if (type_matr_t .eq. 0 .and. type_matr_tang .ne. 'TANGENTE_SECANTE') then
                call getvtx(keywordfact, 'POST_ITER', iocc = i_comp, scal = post_iter, nbret = iret)
                if (iret .eq. 1) then
                    if (post_iter .eq. 'CRIT_RUPT') then
                        ipostiter = 1
                    endif
                endif
            endif
        endif
!
! ----- Get POST_INCR
!
        ipostincr = 0
        if (getexm('COMPORTEMENT','POST_INCR') .eq. 1) then
            post_incr = ' '
            call getvtx(keywordfact, 'POST_INCR', iocc = i_comp, scal = post_incr, nbret = iret)
            if (iret .eq. 1) then
               if (post_incr .eq. 'REST_ECRO') then
                    ipostincr = 1
               endif
            endif
        endif
!
! ----- For KIT
!
        if (l_kit) then
            call comp_meca_rkit(keywordfact, i_comp, rela_comp, kit_comp)
        endif
!
! ----- Get parameters for external programs (MFRONT/UMAT)
!
        call comp_read_exte(rela_comp  , kit_comp ,&
                            l_umat     , l_mfront , l_mfront_offi,&
                            libr_name  , subr_name,&
                            keywordfact, i_comp   )
!
! ----- Get function pointers for external programs (MFRONT/UMAT)
!
        cptr_nbvarext   = 0
        cptr_namevarext = 0
        cptr_fct_ldc    = 0
        if ( l_mfront ) then
            if ( present(model_) ) then
                call dismoi('NOM_MAILLA', model_, 'MODELE', repk=mesh)
! ------------- STAT_NON_LINE case
                call comp_meca_mod(mesh       , model_      ,&
                                   keywordfact, i_comp        , rela_comp,&
                                   model_dim  , model_mfront)
            else
! ------------- CALC_POINT_MAT case
                model_dim    = 3
                model_mfront = '_Tridimensional'
            endif
!           The keywords in DEFI_MATERIAU are those for Tridimensional hypothesis
!FIXME      ASSERT(model_mfront == '_Tridimensional' .or. .not. l_mfront_offi)
            call mfront_get_number_of_internal_state_variables(libr_name, subr_name,&
                                                               model_mfront, nbtest)
            if( nbtest.ne.0 ) then
                call wkvect('&&CARC_READ.INT_VAR', 'V V K80', nbtest, vk80=int_var)
                call mfront_get_internal_state_variables(libr_name, subr_name,&
                                                         model_mfront, int_var,&
                                                         nbtest)
            endif
            call mfront_get_pointers(libr_name, subr_name, model_mfront,&
                                     cptr_nbvarext, cptr_namevarext,&
                                     cptr_fct_ldc,&
                                     cptr_matprop, cptr_nbprop)
            call getvtx(keywordfact, 'VERI_BORNE', iocc = i_comp,&
                        scal = veri_b, nbret = nbval )
            if ( nbval.eq.0 ) then
                call mfront_set_outofbounds_policy(libr_name, subr_name, model_mfront, 2)
            else
                if ( veri_b.eq.'ARRET' ) then
                    call mfront_set_outofbounds_policy(libr_name, subr_name, model_mfront, 2)
                elseif ( veri_b.eq.'MESSAGE' ) then
                    call mfront_set_outofbounds_policy(libr_name, subr_name, model_mfront, 1)
                else
                    call mfront_set_outofbounds_policy(libr_name, subr_name, model_mfront, 0)
                endif
            endif
        elseif ( l_umat ) then
            call umat_get_function(libr_name, subr_name, cptr_fct_ldc)
        endif
!
! ----- Ban if RELATION = MFRONT and ITER_INTE_PAS negative
!
        if (iter_inte_pas .lt. 0.d0) then
            if (l_mfront_offi .or. l_mfront) then
                call utmess('F', 'COMPOR1_95')
            end if
        end if
!
        call lcdiscard(rela_comp_py)
!
! ----- Save options in list
!
        ds_compor_para%v_para(i_comp)%type_matr_t           = type_matr_t
        ds_compor_para%v_para(i_comp)%parm_alpha            = parm_alpha
        ds_compor_para%v_para(i_comp)%parm_theta            = parm_theta
        ds_compor_para%v_para(i_comp)%iter_inte_pas         = iter_inte_pas
        ds_compor_para%v_para(i_comp)%vale_pert_rela        = vale_pert_rela
        ds_compor_para%v_para(i_comp)%resi_deborst_max      = resi_deborst_max
        ds_compor_para%v_para(i_comp)%iter_deborst_max      = iter_deborst_max
        ds_compor_para%v_para(i_comp)%seuil                 = seuil
        ds_compor_para%v_para(i_comp)%amplitude             = amplitude
        ds_compor_para%v_para(i_comp)%taux_retour           = taux_retour
        ds_compor_para%v_para(i_comp)%post_iter             = ipostiter
        ds_compor_para%v_para(i_comp)%post_incr             = ipostincr
        ds_compor_para%v_para(i_comp)%c_pointer%nbvarext    = cptr_nbvarext
        ds_compor_para%v_para(i_comp)%c_pointer%namevarext  = cptr_namevarext
        ds_compor_para%v_para(i_comp)%c_pointer%fct_ldc     = cptr_fct_ldc
        ds_compor_para%v_para(i_comp)%c_pointer%matprop     = cptr_matprop
        ds_compor_para%v_para(i_comp)%c_pointer%nbprop      = cptr_nbprop
        ds_compor_para%v_para(i_comp)%rela_comp             = rela_comp
        ds_compor_para%v_para(i_comp)%algo_inte             = algo_inte
    end do
!
    call jedema()
end subroutine
