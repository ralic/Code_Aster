subroutine brag00(fami, kpg, ksp, ndim, typmod,&
                  imate, compor, instam, instap, tm,&
                  tp, tref, epsm, deps, sigm,&
                  vim, option, sigp, vip, dsidep)
!
!    ROUTINE ANCIENNEMENT NOMMEE BETRAG
!
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
!
!
! ATTENTION : LE CHAMP D'AVANCEMENT DE LA RAG EST CALCUL?DANS LA ROUTINE
!       A.SELLIER LMDC FEVRIER 2004/E.GRIMAL LMDC-EDF AVRIL 2004
!
!       INSTAM TEMPS AU DEBUT DU PAS
!       INSTAP TEMPS FIN DU PAS
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  IMATE   : ADRESSE DU MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT : RELCOM ET DEFORM
! IN  INSTAM  : INSTANT DU CALCUL PRECEDENT
! IN  INSTAP  : INSTANT DU CALCUL
! IN  TM      : TEMPERATURE A L'INSTANT PRECEDENT
! IN  TP      : TEMPERATURE A L'INSTANT DU CALCUL
! IN  SECHM   : DEGRE DE SATURATION A L'INSTANT PRECEDENT
! IN  SECHP   : DEGRE DE SATURATION A L'INSTANT DU CALCUL
! IN  EPSM    : DEFORMATION PAS AVANT (ATTENTION 4 A 6 SQRT(2)*EPS)
! IN  TREF    : TEMPERATURE DE REFERENCE
! IN  DEPS    : INCREMENT DE DEFORMATION
!               SI C_PLAN DEPST(3) EST EN FAIT INCONNU (ICI:0)
!                 =>  ATTENTION LA PLACE DE DEPST(3) EST ALORS UTILISEE.
! IN  SIGM    : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
! IN  VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
! IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
! OUT SIGP    : CONTRAINTES A L'INSTANT ACTUEL
! OUT VIP     : VARIABLES INTERNES A L'INSTANT ACTUEL
! OUT DSIDEP  : MATRICE CARREE (INUTILISE POUR RAPH_MECA)
!
!               ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
!               L'ORDRE :  XX,YY,ZZ,SQRT(2)*XY,SQRT(2)*XZ,SQRT(2)*YZ
!
!       COMPORTEMENT VISCO-ELASTIQUE ENDOMMGEABLE ANISOTROPE
!       TRIDIMENSIONNEL DU BETON (MODELE SIMPLIFIE TOTALEMENT EXPLICITE)
!
    implicit none
    include 'asterfort/brag01.h'
    include 'asterfort/brdsde.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/rcvarc.h'
    integer :: ndim, imate
    character(len=16) :: compor(3), option
    character(len=8) :: typmod(*)
    real(kind=8) :: vim(65), vip(65)
    real(kind=8) :: instam, instap
!      REAL*8          HYDRM,HYDRP,SECHM,SECHP
    real(kind=8) :: tm, tp, tref
    real(kind=8) :: sigm(6), deps(6), epsm(6)
    real(kind=8) :: sigp(6), dsidep(6, 6)
!
    character(len=8) :: nomres(2)
    real(kind=8) :: valres(2)
    integer :: icodre(2)
    real(kind=8) :: rbid
!      REAL*8          SECH0
!      REAL*8          T0
!      REAL*8          DEPS0(6),EPS0(6)
    real(kind=8) :: devpt(6)
!      REAL*8          INSTA0
    real(kind=8) :: e0, nu0
!      REAL*8          MU0,K0
    logical :: fluage
! MODIFI MARS 2006 "NOUVELLE" VARIABLE DE COMMANDE HYDR ET SECH
    character(len=*) :: fami
    integer :: iret, kpg, ksp
    real(kind=8) :: sechm, sechp, sref
!
!       RECUPERATION DE L HYDRATATION ET DU SECHAGE
    call rcvarc(' ', 'SECH', '+', fami, kpg,&
                ksp, sechp, iret)
    if (iret .ne. 0) sechp=0.d0
    call rcvarc(' ', 'SECH', '-', fami, kpg,&
                ksp, sechm, iret)
    if (iret .ne. 0) sechm=0.d0
    call rcvarc(' ', 'SECH', 'REF', fami, kpg,&
                ksp, sref, iret)
    if (iret .ne. 0) sref=0.d0
!  ------- CARACTERISTIQUES ELASTIQUES (A CHARGER
!          CAUSE CHARGEMENT DSIDEP AU 1 PAS)
    nomres(1)='E'
    nomres(2)='NU'
    call rcvalb(fami, kpg, ksp, '-', imate,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                2, nomres, valres, icodre, 2)
!
    e0 = valres(1)
    nu0 = valres(2)
!     MU0   = E0/2.0D0/(1.D0+NU0)
!     K0    = E0/3.0D0/(1.D0-2.D0*NU0)
!
!     TEST SUR PRISE EN COMPTE DU FLUAGE
    nomres(1) = 'ACTIV_FL'
!
    call rcvalb(fami, kpg, ksp, '-', imate,&
                ' ', 'BETON_RAG', 0, ' ', rbid,&
                1, nomres, valres, icodre, 2)
!
    if (valres(1) .eq. 1.0d0) then
        fluage=.true.
    else
        fluage=.false.
    endif
!
!--  CALCUL DE DSIDEP POUR LA MATRICE TANGENTE
!   -------------------------------------------
!
    if ((option(1:14) .eq. 'RIGI_MECA_TANG') .or. (option(1:9) .eq. 'FULL_MECA' )) then
!
        call brdsde(e0, nu0, dsidep, vim, sigm)
!
    endif
!
    if ((option(1:9) .eq. 'RAPH_MECA') .or. (option(1:9) .eq. 'FULL_MECA')) then
!
! CALCUL A DT/2 (RUNGE-KUTTA)
!
!     INSTA0=(INSTAM+INSTAP)/2.
!     T0=(TM+TP)/2.
!     SECH0=(SECHM+SECHP)/2.
!
!     DO 20 J=1,6
!      DEPS0(J)=DEPS(J)/2.
!      EPS0(J)=EPSM(J)+DEPS0(J)
!20   CONTINUE
!
        if (fluage) goto 10
!
!
        call brag01(fami, kpg, ksp, ndim, typmod,&
                    imate, compor, instam, instap, sechm,&
                    sechp, tm, tp, tref, epsm,&
                    deps, sigm, vim, option, sigp,&
                    vip, dsidep, devpt, 3)
!
10      continue
        call brag01(fami, kpg, ksp, ndim, typmod,&
                    imate, compor, instam, instap, sechm,&
                    sechp, tm, tp, tref, epsm,&
                    deps, sigm, vim, option, sigp,&
                    vip, dsidep, devpt, 1)
!
    endif
end subroutine
