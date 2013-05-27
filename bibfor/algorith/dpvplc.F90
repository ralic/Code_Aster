subroutine dpvplc(typmod, option, imate, crit, instam,&
                  instap, td, tf, tr, depsm,&
                  sigm, vim, sig, vip, dsidep,&
                  iret)
! =====================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit      none
    include 'asterfort/dpvpdi.h'
    include 'asterfort/dpvpma.h'
    include 'asterfort/dpvpre.h'
    integer :: imate, iret
    real(kind=8) :: depsm(6), vim(*), vip(*), sig(6), dsidep(6, 6)
    real(kind=8) :: sigm(6), td, tf, tr
    real(kind=8) :: instam, instap, crit(3)
    character(len=8) :: typmod(*)
    character(len=16) :: option
! =====================================================================
! --- LOI DE COMPORTEMENT DE TYPE DRUCKER PRAGER VISCOPLASTIQUE -
! --- VISC_DRUC_PRAG --------------------------------------------------
! --- ECROUISSAGE LINEAIRE --------------------------------------------
! ----VISCOSITE DE TYPE PERZYNA ---------------------------------------
! =====================================================================
! IN  DEPSM    INCREMENT DU CHAMP DE DEFORMATION
! IN  SIGM     CONTRAINTES EN T-
! IN  VIM     VARIABLES INTERNES EN T-
!               1   : P
!               2   : INDICATEUR DE PLASTICITE
!               3   : POSITION DE PAR RAPPORT A PPIC et à PULT
!               4   : NOMBRE D ITERATIONS POUR LA CONVERGENCE LOCALE
! OUT SIG    CONTRAINTES EN T+
! VAR VIP     VARIABLES INTERNES EN T+
! OUT DSIDEP  MATRICE TANGENTE
! OUT IRET    CODE RETOUR (0 = OK)
! =====================================================================
    integer :: nbmat, ndt, ndi, nvi, indal, nbre
    parameter    (nbmat  = 50 )
    real(kind=8) :: materd(nbmat, 2), materf(nbmat, 2), deps(6)
    character(len=3) :: matcst
! =====================================================================
    common /tdim/   ndt, ndi
! =====================================================================
    matcst = 'OUI'
! =====================================================================
! --- RECUPERATION DU TYPE DE LOI DE COMPORTEMENT DP ------------------
! =====================================================================
    call dpvpma(typmod, imate, nbmat, td, materd,&
                materf, matcst, ndt, ndi, nvi,&
                indal)
! =====================================================================
! --- RETRAIT DE LA DEFORMATION DUE A LA DILATATION THERMIQUE ---------
! =====================================================================
    call dpvpdi(nbmat, materf, td, tf, tr,&
                depsm, deps)
!
! =====================================================================
! --- RESOLTUTION DE LA LOI DRUCKER PRAGER VISCOPLASTIQUE -------------
! =====================================================================
    call dpvpre(typmod, nvi, option, crit, instam,&
                instap, nbmat, materf, sigm, deps,&
                vim, vip, sig, nbre, dsidep,&
                iret)
! =====================================================================
end subroutine
