subroutine dpvpre(mod, nvi, option, crit, instam,&
                  instap, nbmat, materf, sigm, deps,&
                  vim, vip, sig, nbre, dsidep,&
                  iret)
! =====================================================================
    implicit      none
#include "asterfort/assert.h"
#include "asterfort/dpvpcr.h"
#include "asterfort/dpvpdb.h"
#include "asterfort/dpvpot.h"
#include "asterfort/dpvpsi.h"
#include "asterfort/dpvpva.h"
#include "asterfort/lcdevi.h"
#include "asterfort/lceqma.h"
#include "asterfort/lcinma.h"
#include "asterfort/lcinve.h"
#include "asterfort/lcopil.h"
#include "asterfort/lcopli.h"
#include "asterfort/lcprmv.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lcsove.h"
#include "asterfort/trace.h"
    integer :: iret, nvi, nbmat
    real(kind=8) :: deps(6), vim(nvi), vip(nvi), sig(6)
    real(kind=8) :: sigm(6), materf(nbmat, 2), dsidep(6, 6), crit(3)
    real(kind=8) :: instam, instap
    character(len=8) :: mod
    character(len=16) :: option
! =====================================================================
! --- LOI DE COMPORTEMENT DRUCKER PRAGER VISCOPLASTIQUE ---------------
! --- VISC_DRUC_PRAG --------------------------------------------------
! ----RESOLUTION -----------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! =====================================================================
    logical :: resi
    integer :: ndt, ndi, ii, pos, nbre
    real(kind=8) :: deux, trois
    real(kind=8) :: ppic, pult
    real(kind=8) :: dp
    real(kind=8) :: dt, plas
    real(kind=8) :: fcrit
    real(kind=8) :: siim, siie, seqm, seqe, i1m, i1e
    real(kind=8) :: fonecm(3), fonecp(3)
    real(kind=8) :: hookf(6, 6), dkooh(6, 6)
    real(kind=8) :: sige(6), se(6), sm(6)
    real(kind=8) :: inte(6)
! =====================================================================
    parameter ( deux    = 2.0d0 )
    parameter ( trois   = 3.0d0 )
    common /tdim/   ndt  , ndi
! =====================================================================
! --- RECUPERATION DES MATERIAUX --------------------------------------
! =====================================================================
    ppic = materf(4,2)
    pult = materf(5,2)
! =====================================================================
! --- INITIALISATION --------------------------------------------------
! =====================================================================
    call lcinma(0.0d0, hookf)
    call lcinma(0.0d0, dkooh)
    call lcinve(0.0d0, sig)
    call lcinve(0.0d0, sige)
!
    iret = 0
    dt = instap - instam
    plas = 0.d0
    dp = 0.d0
!
! =====================================================================
! =====================================================================
    resi = option(1:9).eq.'FULL_MECA' .or. option .eq.'RAPH_MECA'
    call assert((option(1:9).eq.'RIGI_MECA') .or. (option(1:9).eq.'FULL_MECA') .or.&
                (option .eq.'RAPH_MECA'))
! =====================================================================
! --- OPERATEUR ELASTIQUE LINEAIRE ISOTROPE ---------------------------
! =====================================================================
    call lcopli('ISOTROPE', mod, materf(1, 1), hookf)
    call lcopil('ISOTROPE', mod, materf(1, 1), dkooh)
!
    call lcdevi(sigm, sm)
    call lcprsc(sm, sm, siim)
    seqm = sqrt (trois*siim/deux)
    i1m = trace(ndi,sigm)
!
! =====================================================================
! --- PREDICTION ELASTIQUE : SIGF = HOOKF EPSP -----------------------
! =====================================================================
!
!
    call lcprmv(hookf, deps, inte)
    call lcsove(sigm, inte, sige)
!
    call lcdevi(sige, se)
    call lcprsc(se, se, siie)
    seqe = sqrt (trois*siie/deux)
    i1e = trace(ndi,sige)
! =====================================================================
! --- CALCUL DU CRITERE -----------------------------------------------
! =====================================================================
    if (resi) then
! =====================================================================
! --- SIGNE DU CRITERE ------------------------------------------------
! =====================================================================
        call dpvpva(vim(1), nbmat, materf, fonecm)
!
        fcrit = dpvpcr(fonecm, seqe, i1e)
!
        if (fcrit .gt. 0.0d0) then
            plas = 1.0d0
! =====================================================================
! ---------------------RESOLUTION EQ NON LINEAIRE EN DP----------------
! =====================================================================
            call dpvpdb(nbmat, materf, crit, dt, vim,&
                        vip, nvi, seqe, i1e, seqm,&
                        i1m, dp, nbre, iret)
        else
            plas = 0.0d0
            dp = 0.0d0
            nbre = 0
        endif
! =====================================================================
! --- MISE A JOUR DES CONTRAINTES TENANT COMPTE DE DP SI VISCOPLASTICIT
! =====================================================================
        if (plas .eq. 0.0d0) then
            do 10 ii = 1, ndt
                sig(ii) = sige(ii)
10          continue
!
            vip(1) = vim(1)
            vip(3) = vim(3)
            vip(4) = vim(4)
        else
            vip(1) = vim(1) + dp
!
            call dpvpva(vip(1), nbmat, materf, fonecp)
            call dpvpsi(nbmat, materf, se, seqe, i1e,&
                        fonecp, dp, sig)
        endif
! =====================================================================
! --- STOCKAGE DES VARIABLES INTERNES ---------------------------------
! =====================================================================
        vip(2) = plas
!
        vip(nvi) = nbre
!
        if (vip(1) .lt. ppic) then
            pos = 1
        else if (vip(1) .lt. pult) then
            pos = 2
        else
            pos = 3
        endif
!
        vip(3) = pos
    endif
! =====================================================================
! --- TERMES DE L OPERATEUR TANGENT -----------------------------------
! =====================================================================
    if (option(10:14) .eq. '_ELAS') then
        call lceqma(hookf, dsidep)
    endif
!
    if (option(1:14) .eq. 'RIGI_MECA_TANG') then
        call lceqma(hookf, dsidep)
    endif
    if (option(1:9) .eq. 'FULL_MECA') then
        if (vip(2) .eq. 1.d0) then
            call dpvpot(mod, vim(1), vip(1), nbmat, materf,&
                        sige, dt, dp, plas, dsidep)
        else if (vip(2).eq.0.d0) then
            call lceqma(hookf, dsidep)
        endif
    endif
! =====================================================================
end subroutine
