subroutine nmstat(phase      , list_func_acti, sdstat, sdtime, ds_print,&
                  sdcont_defi)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8prem.h"
#include "asterfort/cfdisl.h"
#include "asterfort/impfot.h"
#include "asterfort/impmem.h"
#include "asterfort/isfonc.h"
#include "asterfort/nmrini.h"
#include "asterfort/nmrvai.h"
#include "asterfort/nmtimr.h"
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
    character(len=1), intent(in) :: phase
    character(len=24), intent(in) :: sdstat
    character(len=24), intent(in) :: sdtime
    character(len=24), intent(in) :: sdcont_defi
    integer, intent(in) :: list_func_acti(*)
    type(NL_DS_Print), intent(in) :: ds_print
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Print statistics
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_print         : datastructure for printing parameters
! In  phasis           : phasis
!                          'N' current Newton iteration
!                          'P' current step time
!                          'T' on all transient
! In  sdtime           : datastructure for timers management
! In  sdstat           : datastructure for statistics
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  list_func_acti   : list of active functionnalities
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: lctcd, lallv, lcont, lctcc
    aster_logical :: lctcg, lboucc, lboucf, lfrot
    aster_logical :: lmvib, lflam, lpost, lnewtg
    aster_logical :: lprint
    real(kind=8) :: tpsint, tpsasm, tpsfac, tps2mb, tpssol
    real(kind=8) :: tps, tpsmoy, tpspst, tpslst, tpsrst
    real(kind=8) :: effica
    integer :: nbrint, nbrfac, nbrsol, nbrrel
    integer :: nbiter, nbinst
    real(kind=8) :: tpscog
    real(kind=8) :: tpscda
    real(kind=8) :: tpsccp, tpsccm, tpsccv, tpsccc, tpsccf
    integer :: ctgeom
    integer :: nbliac, nbliaf
    integer :: ctccpr, ctcmat, ctcvec, ctiter, ctcfro
    integer :: vali(5)
    integer :: cycl_nb(4)
    character(len=24) :: tpscvt
!
! --------------------------------------------------------------------------------------------------
!
    tps    = 0.d0
    tpsint = 0.d0
    tpsasm = 0.d0
    tpsfac = 0.d0
    tps2mb = 0.d0
    tpssol = 0.d0
    tpscog = 0.d0
    tpscda = 0.d0
    tpsccp = 0.d0
    tpsccm = 0.d0
    tpsccv = 0.d0
    tpsccc = 0.d0
    tpsccf = 0.d0
    tpspst = 0.d0
    tpslst = 0.d0
    tpsrst = 0.d0
    nbiter = 0
    nbinst = 0
    nbrint = 0
    nbrfac = 0
    nbrsol = 0
    ctgeom = 0
    nbliac = 0
    nbliaf = 0
    ctccpr = 0
    ctcmat = 0
    ctcvec = 0
    ctiter = 0
    ctcfro = 0
    nbrrel = 0
    lmvib = isfonc(list_func_acti,'MODE_VIBR')
    lflam = isfonc(list_func_acti,'CRIT_STAB')
    lcont = isfonc(list_func_acti,'CONTACT' )
    lctcg = isfonc(list_func_acti,'BOUCLE_EXT_GEOM')
    lctcd = isfonc(list_func_acti,'CONT_DISCRET')
    lctcc = isfonc(list_func_acti,'CONT_CONTINU')
    lboucc = isfonc(list_func_acti,'BOUCLE_EXT_CONT')
    lboucf = isfonc(list_func_acti,'BOUCLE_EXT_FROT')
    lnewtg = isfonc(list_func_acti,'GEOM_NEWTON')
    lpost = lmvib.or.lflam.or.lcont
    if (lcont) then
        lallv = cfdisl(sdcont_defi,'ALL_VERIF')    
        lfrot = cfdisl(sdcont_defi,'FROTTEMENT')
    else
        lallv = .false.
        lfrot = .false.
    endif
!
! - Print for this step time ?
!
    lprint = ds_print%l_print
!
! - Total time during phasis
!
    call nmtimr(sdtime, 'TEMPS_PHASE', phase, tps)
!
! - Total "lost" time (step cutting)
!
    if (phase .eq. 'T') then
        call nmtimr(sdtime, 'PAS_LOST', phase, tpslst)
    endif
!
! - Time of operations during phasis
!
    call nmtimr(sdtime, 'INTEGRATION', phase, tpsint)
    call nmtimr(sdtime, 'ASSE_MATR', phase, tpsasm)
    call nmtimr(sdtime, 'FACTOR', phase, tpsfac)
    call nmtimr(sdtime, 'SECO_MEMB', phase, tps2mb)
    call nmtimr(sdtime, 'SOLVE', phase, tpssol)
!
! - Number of operations during phasis
!
    call nmrvai(sdstat, 'INTEGRATION', phase, nbrint)
    call nmrvai(sdstat, 'FACTOR', phase, nbrfac)
    call nmrvai(sdstat, 'SOLVE', phase, nbrsol)
    call nmrvai(sdstat, 'RECH_LINE_ITER', phase, nbrrel)
!
    if ((phase.eq.'P') .or. (phase.eq.'T')) then
        call nmrvai(sdstat, 'ITE', phase, nbiter)
    endif
!
    if (phase .eq. 'T') then
        call nmrvai(sdstat, 'PAS', phase, nbinst)
    endif
!
! - Contact - Geometric loop
!
    if (lctcg .or. lnewtg) then
        call nmtimr(sdtime, 'CONT_GEOM', phase, tpscog)
        call nmrvai(sdstat, 'CONT_GEOM', phase, ctgeom)
    endif
!
! - Contact
!
    if (lcont .and. (.not.lallv)) then
        call nmrvai(sdstat, 'CONT_NBLIAC', phase, nbliac)
        if (lfrot) then
            call nmrvai(sdstat, 'CONT_NBLIAF', phase, nbliaf)
        endif
        if (lctcd) then
            call nmrvai(sdstat, 'CTCD_ALGO_ITER', phase, ctiter)
            call nmtimr(sdtime, 'CTCD_ALGO', phase, tpscda)
        else if (lctcc) then
            call nmrvai(sdstat, 'CTCC_PREP', phase, ctccpr)
            call nmrvai(sdstat, 'CTCC_MATR', phase, ctcmat)
            call nmrvai(sdstat, 'CTCC_VECT', phase, ctcvec)
            call nmrvai(sdstat, 'CTCC_CONT', phase, ctiter)
            call nmrvai(sdstat, 'CTCC_FROT', phase, ctcfro)
            call nmtimr(sdtime, 'CTCC_PREP', phase, tpsccp)
            call nmtimr(sdtime, 'CTCC_MATR', phase, tpsccm)
            call nmtimr(sdtime, 'CTCC_VECT', phase, tpsccv)
            call nmtimr(sdtime, 'CTCC_CONT', phase, tpsccc)
            call nmtimr(sdtime, 'CTCC_FROT', phase, tpsccf)
            call nmrvai(sdstat, 'CTCC_CYCL_1', phase, cycl_nb(1))
            call nmrvai(sdstat, 'CTCC_CYCL_2', phase, cycl_nb(2))
            call nmrvai(sdstat, 'CTCC_CYCL_3', phase, cycl_nb(3))
            call nmrvai(sdstat, 'CTCC_CYCL_4', phase, cycl_nb(4))
        endif
    endif
!
! - Post-treatment (buckling)
!
    if (lpost) then
        call nmtimr(sdtime, 'POST_TRAITEMENT', phase, tpspst)
    endif
!
! - Remain time
!
    tpsrst = tps - tpsint - tpsasm - tpsfac - tps2mb - tpssol - tpscog -&
                   tpscda - tpsccp - tpsccm - tpsccv - tpsccc - tpsccf
    if (tpsrst .le. 0.d0) then
        tpsrst = 0.d0
    endif
!
! - Print at end of current step time
!
    if ((phase.eq.'P') .and. lprint) then
        tpsmoy = tps/nbiter
        call impfot(tps, tpscvt)
        call utmess('I', 'MECANONLINE7_1', sk=tpscvt)
        call impfot(tpsmoy, tpscvt)
        call utmess('I', 'MECANONLINE7_2', sk=tpscvt, si=nbiter)
        call impfot(tpsint, tpscvt)
        call utmess('I', 'MECANONLINE7_4', sk=tpscvt, si=nbrint)
        call impfot(tpsasm, tpscvt)
        call utmess('I', 'MECANONLINE7_13', sk=tpscvt)
        call impfot(tpsfac, tpscvt)
        call utmess('I', 'MECANONLINE7_3', sk=tpscvt, si=nbrfac)
        call impfot(tps2mb, tpscvt)
        call utmess('I', 'MECANONLINE7_12', sk=tpscvt)
        call impfot(tpssol, tpscvt)
        call utmess('I', 'MECANONLINE7_5', sk=tpscvt, si=nbrsol)
!
! ----- Contact - Geometric loop
!
        if ((lctcg.and.(.not.lallv)) .or. lnewtg) then
            call impfot(tpscog, tpscvt)
            call utmess('I', 'MECANONLINE7_11', sk=tpscvt, si=ctgeom)
        endif
!
! ----- Contact - Discrete
!
        if (lctcd .and. (.not.lallv)) then
            call impfot(tpscda, tpscvt)
            call utmess('I', 'MECANONLINE7_10', sk=tpscvt, si=ctiter)
        endif
!
! ----- Contact - Continue
!
        if (lctcc .and. (.not.lallv)) then
            call impfot(tpsccp, tpscvt)
            call utmess('I', 'MECANONLINE7_24', sk=tpscvt, si=ctccpr)
            call impfot(tpsccm, tpscvt)
            call utmess('I', 'MECANONLINE7_20', sk=tpscvt, si=ctcmat)
            call impfot(tpsccv, tpscvt)
            call utmess('I', 'MECANONLINE7_25', sk=tpscvt, si=ctcvec)
            if (lboucc) then
                call impfot(tpsccc, tpscvt)
                call utmess('I', 'MECANONLINE7_23', sk=tpscvt, si=ctiter)
            endif
            if (lboucf) then
                call impfot(tpsccf, tpscvt)
                call utmess('I', 'MECANONLINE7_22', sk=tpscvt, si=ctcfro)
            endif
        endif
!
! ----- Time for post-treatment
!
        if (lpost) then
            call impfot(tpspst, tpscvt)
            call utmess('I', 'MECANONLINE7_7', sk=tpscvt)
        endif
!
! ----- Remain time
!
        call impfot(tpsrst, tpscvt)
        call utmess('I', 'MECANONLINE7_6', sk=tpscvt)
        if (nbrrel .ne. 0) then
            call utmess('I', 'MECANONLINE7_8', si=nbrrel)
        endif
!
! ----- Statistics for contact
!
        if (lcont .and. (.not.lallv)) then
            call utmess('I', 'MECANONLINE7_30')
            call utmess('I', 'MECANONLINE7_31', si=nbliac)
            if (lfrot) then
                call utmess('I', 'MECANONLINE7_32', si=nbliaf)
            endif
            if (lctcc) then
                if (cycl_nb(1) .ne. 0) call utmess('I', 'MECANONLINE7_33', si=cycl_nb(1))
                if (cycl_nb(2) .ne. 0) call utmess('I', 'MECANONLINE7_34', si=cycl_nb(2))
                if (cycl_nb(3) .ne. 0) call utmess('I', 'MECANONLINE7_35', si=cycl_nb(3))
                if (cycl_nb(4) .ne. 0) call utmess('I', 'MECANONLINE7_36', si=cycl_nb(4))
            endif
        endif
!
! ----- Memory 
!
        call impmem()
    endif
!
! - Print at end of computation
!
    if (phase .eq. 'T') then
        vali(1) = nbinst
        vali(2) = nbiter
        vali(3) = nbrint
        vali(4) = nbrfac
        vali(5) = nbrsol
        call utmess('I', 'MECANONLINE8_1', ni=5, vali=vali)
        if (nbrrel .ne. 0) then
            call utmess('I', 'MECANONLINE8_2', si=nbrrel)
        endif
!
! ----- Statistics for contact
!
        if (lcont .and. (.not.lallv)) then
            if (lctcd .or. lboucc .or. lboucf .or. lctcg .or. lnewtg) then
                call utmess('I', 'MECANONLINE8_30')
            endif
        endif
!
! ----- Contact - Geometric loop
!
        if ((lctcg.and.(.not.lallv)) .or. lnewtg) then
            call utmess('I', 'MECANONLINE8_11', si=ctgeom)
        endif
!
! ----- Contact - Discrete
!
        if (lctcd .and. (.not.lallv)) then
            call utmess('I', 'MECANONLINE8_10', si=ctiter)
        endif
!
! ----- Contact - Continue
!
        if (lctcc .and. (.not.lallv)) then
            if (lboucf) then
                call utmess('I', 'MECANONLINE8_22', si=ctcfro)
            endif
            if (lboucc) then
                call utmess('I', 'MECANONLINE8_23', si=ctiter)
            endif
        endif
!
! ----- Remain time
!
        call impfot(tps, tpscvt)
        call utmess('I', 'MECANONLINE8_50', sk=tpscvt)
        if (tpslst .ge. r8prem()) then
            call impfot(tpslst, tpscvt)
            effica = 100.d0*(tps-tpslst)/tps
            call utmess('I', 'MECANONLINE8_70', sk=tpscvt, sr=effica)
        endif
        call impfot(tpsint, tpscvt)
        call utmess('I', 'MECANONLINE8_54', sk=tpscvt)
        call impfot(tpsasm, tpscvt)
        call utmess('I', 'MECANONLINE8_51', sk=tpscvt)
        call impfot(tpsfac, tpscvt)
        call utmess('I', 'MECANONLINE8_53', sk=tpscvt)
        call impfot(tps2mb, tpscvt)
        call utmess('I', 'MECANONLINE8_52', sk=tpscvt)
        call impfot(tpssol, tpscvt)
        call utmess('I', 'MECANONLINE8_55', sk=tpscvt)
!
! ----- Contact - Geometric loop
!
        if ((lctcg.and.(.not.lallv)) .or. lnewtg) then
            call impfot(tpscog, tpscvt)
            call utmess('I', 'MECANONLINE8_61', sk=tpscvt)
        endif
!
! ----- Contact - Discrete
!
        if (lctcd .and. (.not.lallv)) then
            call impfot(tpscda, tpscvt)
            call utmess('I', 'MECANONLINE8_56', sk=tpscvt)
        endif
!
! ----- Contact - Continu
!
        if (lctcc .and. (.not.lallv)) then
            call impfot(tpsccp, tpscvt)
            call utmess('I', 'MECANONLINE8_58', sk=tpscvt)
            call impfot(tpsccm, tpscvt)
            call utmess('I', 'MECANONLINE8_57', sk=tpscvt)
            call impfot(tpsccv, tpscvt)
            call utmess('I', 'MECANONLINE8_64', sk=tpscvt)
            if (lboucc) then
                call impfot(tpsccc, tpscvt)
                call utmess('I', 'MECANONLINE8_60', sk=tpscvt)
            endif
            if (lboucf) then
                call impfot(tpsccf, tpscvt)
                call utmess('I', 'MECANONLINE8_59', sk=tpscvt)
            endif
        endif
!
! ----- Post-treatment
!
        if (lpost) then
            call impfot(tpspst, tpscvt)
            call utmess('I', 'MECANONLINE8_62', sk=tpscvt)
        endif
!
! ----- Remain time
!
        call impfot(tpsrst, tpscvt)
        call utmess('I', 'MECANONLINE8_63', sk=tpscvt)
!
    endif
!
! - New statistics (erase old ones)
!
    call nmrini(sdtime, sdstat, phase)
!
end subroutine
