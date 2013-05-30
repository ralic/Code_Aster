subroutine lcdrpr(typmod, option, imate, compor, sigm,&
                  td, tf, tr, depsm, vim,&
                  vip, sig, dsidep, iret)
! =====================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
    include 'asterfort/dpmate.h'
    include 'asterfort/dpvpdi.h'
    include 'asterfort/lcdpli.h'
    include 'asterfort/lcdppa.h'
    integer :: imate, iret
    real(kind=8) :: depsm(6), vim(*), vip(*), sig(6), dsidep(6, 6)
    real(kind=8) :: sigm(6), td, tf, tr
    character(len=8) :: typmod(*)
    character(len=16) :: option, compor(*)
! ======================================================================
! --- LOI DE COMPORTEMENT DE TYPE DRUCKER PRAGER -----------------------
! --- ELASTICITE ISOTROPE ----------------------------------------------
! --- PLASTICITE DE VON MISES + TERME DE TRACE -------------------------
! --- ECROUISSAGE ISOTROPE LINEAIRE ------------------------------------
! ======================================================================
! IN  OPTION  OPTION DE CALCUL (RAPH_MECA, RIGI_MECA_TANG OU FULL_MECA)
! IN  IMATE   NATURE DU MATERIAU
! IN  EPSM    CHAMP DE DEFORMATION EN T-
! IN  DEPS    INCREMENT DU CHAMP DE DEFORMATION
! IN  VIM     VARIABLES INTERNES EN T-
!               1   : ENDOMMAGEMENT (D)
!               2   : INDICATEUR DISSIPATIF (1) OU ELASTIQUE (0)
! VAR VIP     VARIABLES INTERNES EN T+
!              IN  ESTIMATION (ITERATION PRECEDENTE)
!              OUT CALCULEES
! OUT SIGP    CONTRAINTES EN T+
! OUT DSIDEP  MATRICE TANGENTE
! OUT IRET    CODE RETOUR (0 = OK)
! ======================================================================
    integer :: nbmat, typedp, ndt, ndi, nvi
    parameter    (nbmat  = 5 )
    real(kind=8) :: materf(nbmat, 2), deps(6)
    character(len=8) :: mod
! ======================================================================
    common /tdim/   ndt, ndi
! ======================================================================
! --- RECUPERATION DU TYPE DE LOI DE COMPORTEMENT DP -------------------
! ======================================================================
    mod = typmod(1)
    call dpmate(mod, imate, materf, ndt, ndi,&
                nvi, typedp)
! ======================================================================
! --- RETRAIT DE LA DEFORMATION DUE A LA DILATATION THERMIQUE ----------
! ======================================================================
    call dpvpdi(nbmat, materf, td, tf, tr,&
                depsm, deps)
! ======================================================================
    if (typedp .eq. 1) then
! ======================================================================
! --- CAS LINEAIRE -----------------------------------------------------
! ======================================================================
        call lcdpli(mod, nvi, option, materf, sigm,&
                    deps, vim, vip, sig, dsidep,&
                    iret)
    else if (typedp.eq.2) then
! ======================================================================
! --- CAS PARABOLIQUE --------------------------------------------------
! ======================================================================
        call lcdppa(mod, nvi, option, materf, compor,&
                    sigm, deps, vim, vip, sig,&
                    dsidep, iret)
    endif
! ======================================================================
end subroutine
