subroutine lcdpli(mod, nvi, option, materf, sigm,&
                  deps, vim, vip, sig, dsidep,&
                  iret)
! =====================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    implicit      none
    include 'asterfort/assert.h'
    include 'asterfort/dplitg.h'
    include 'asterfort/dpmata.h'
    include 'asterfort/lcdevi.h'
    include 'asterfort/lceqma.h'
    include 'asterfort/lcopil.h'
    include 'asterfort/lcopli.h'
    include 'asterfort/lcprmv.h'
    include 'asterfort/lcsove.h'
    include 'asterfort/majsig.h'
    include 'asterfort/resdp1.h'
    include 'asterfort/trace.h'
    include 'blas/ddot.h'
    integer :: iret, nvi
    real(kind=8) :: deps(6), vim(nvi), vip(nvi), sig(6)
    real(kind=8) :: sigm(6), materf(5, 2), dsidep(6, 6)
    character(len=8) :: mod
    character(len=16) :: option
! =====================================================================
! --- LOI DE COMPORTEMENT DRUCKER PRAGER DE TYPE LINEAIRE -------------
! =====================================================================
! IN  OPTION  OPTION DE CALCUL (RAPH_MECA, RIGI_MECA_TANG OU FULL_MECA)
! IN  EPSM    CHAMP DE DEFORMATION EN T-
! IN  DEPS    INCREMENT DU CHAMP DE DEFORMATION
! IN  VIM     VARIABLES INTERNES EN T-
!               1   : ENDOMMAGEMENT (D)
!               2-7 : DEFORMATION ELASTIQUE (EPE AVEC RAC2 HD)
!               8   : INDICATEUR DISSIPATIF (1) OU ELASTIQUE (0)
! VAR VIP     VARIABLES INTERNES EN T+
!              IN  ESTIMATION (ITERATION PRECEDENTE)
!              OUT CALCULEES
! OUT SIGP    CONTRAINTES EN T+
! OUT DSIDEP  MATRICE TANGENTE
! OUT IRET    CODE RETOUR (0 = OK)
! =====================================================================
    logical :: rigi, resi
    integer :: ndt, ndi, ii
    real(kind=8) :: trois, deux, dp, dpdeno, alpha, pmoins, pplus
    real(kind=8) :: hookf(6, 6), dkooh(6, 6), plas
    real(kind=8) :: epsp(6), epsm2(6), sige(6), se(6), siie, seq, i1e
! =====================================================================
    parameter ( deux  = 2.0d0 )
    parameter ( trois = 3.0d0 )
    common /tdim/   ndt  , ndi
! =====================================================================
! --- INITIALISATION --------------------------------------------------
! =====================================================================
    pmoins = vim(1)
    iret = 0
    resi = option(1:9).eq.'FULL_MECA' .or. option .eq.'RAPH_MECA'
    rigi = option(1:9).eq.'FULL_MECA' .or. option(1:9).eq.'RIGI_MECA'
    call assert((option(1:9).eq.'RIGI_MECA') .or. (option(1:9).eq.'FULL_MECA') .or.&
                (option .eq.'RAPH_MECA'))
! =====================================================================
! --- AFFECTATION DES VARIABLES ---------------------------------------
! =====================================================================
    alpha = materf(3,2)
! =====================================================================
! --- OPERATEUR ELASTIQUE LINEAIRE ISOTROPE ---------------------------
! =====================================================================
    call lcopli('ISOTROPE', mod, materf(1, 1), hookf)
    call lcopil('ISOTROPE', mod, materf(1, 1), dkooh)
    call lcprmv(dkooh, sigm, epsm2)
    call lcsove(epsm2, deps, epsp)
! =====================================================================
! --- INTEGRATION ELASTIQUE : SIGF = HOOKF EPSP -----------------------
! =====================================================================
    call lcprmv(hookf, epsp, sige)
    call lcdevi(sige, se)
    siie=ddot(ndt,se,1,se,1)
    seq = sqrt (trois*siie/deux)
    i1e = trace (ndi,sige)
!
! =====================================================================
! --- CALCUL DES CONTRAINTES ------------------------------------------
! =====================================================================
    if (resi) then
! =====================================================================
! --- RESOLUTION DU SYSTEME -------------------------------------------
! =====================================================================
        call resdp1(materf, seq, i1e, pmoins, dp,&
                    plas)
        if (plas .eq. 0.0d0) then
            do 10 ii = 1, ndt
                sig(ii) = sige(ii)
10          continue
        else
            call majsig(materf, se, seq, i1e, alpha,&
                        dp, plas, sig)
        endif
!
! =====================================================================
! --- STOCKAGE DES VARIABLES INTERNES ---------------------------------
! =====================================================================
        vip(1) = vim(1) + dp
        vip(2) = vim(2) + trois*alpha*dp
        vip(nvi) = plas
!
! =====================================================================
! --- PREPARATION AU CALCUL DE LA MATRICE TANGENTE --------------------
! =====================================================================
        dpdeno = dplitg( materf,vip(1),plas )
        pplus = vip(1)
    else
        plas = vim(nvi)
        dp = 0.0d0
        pplus = 0.0d0
        dpdeno = dplitg( materf,pmoins,plas)
    endif
! =====================================================================
! --- CALCUL DE LA MATRICE TANGENTE -----------------------------------
! =====================================================================
    if (rigi) then
        if (option(10:14) .eq. '_ELAS') then
            call lceqma(hookf, dsidep)
        else
            call dpmata(mod, materf, alpha, dp, dpdeno,&
                        pplus, se, seq, plas, dsidep)
        endif
    endif
! =====================================================================
end subroutine
