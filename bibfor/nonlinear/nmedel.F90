subroutine nmedel(ndim, typmod, imate, deps, sigm,&
                  option, sigp, dsidep)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
!
    include 'asterfort/r8inir.h'
    include 'asterfort/rcvalb.h'
    integer :: ndim, imate
    character(len=8) :: typmod(*)
    character(len=16) :: option
    real(kind=8) :: deps(6)
    real(kind=8) :: sigm(6), sigp(6), dsidep(6, 6)
!
! ----------------------------------------------------------------------
!     LOI ELASTIQUE POUR L'ELEMENT A DISCONTINUITE
! ----------------------------------------------------------------------
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  IMATE   : ADRESSE DU MATERIAU CODE
! IN  DEPS    : INCREMENT DE DEFORMATION
!               SI C_PLAN DEPS(3) EST EN FAIT INCONNU (ICI:0)
!                 =>  ATTENTION LA PLACE DE DEPS(3) EST ALORS UTILISEE.
! IN  SIGM    : CONTRAINTES A L'INSTANT DU CALCUL PRECEDENT
! IN  OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
! OUT SIGP    : CONTRAINTES A L'INSTANT ACTUEL
! OUT DSIDEP  : MATRICE CARREE (INUTILISE POUR RAPH_MECA)
!
!               ATTENTION LES TENSEURS ET MATRICES SONT RANGES DANS
!               L'ORDRE :  XX,YY,ZZ,SQRT(2)*XY,SQRT(2)*XZ,SQRT(2)*YZ
!-----------------------------------------------------------------------
!
    logical :: cplan
    real(kind=8) :: deuxmu
    real(kind=8) :: valres(3)
    real(kind=8) :: depsmo, e, nu, troisk
    real(kind=8) :: kron(6), depsdv(6)
    integer :: ndimsi
    integer :: k, j, kpg, spt
    integer :: icodre(3)
    character(len=8) :: nomres(3), fami, poum
    data        kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
!
!     INITIALISATIONS
!     ---------------
!
    cplan = typmod(1) .eq. 'C_PLAN'
    ndimsi = 2*ndim
!
!     RECUPERATION DES CARACTERISTIQUES
!     ---------------------------------
    nomres(1)='E'
    nomres(2)='NU'
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    call rcvalb(fami, kpg, spt, poum, imate,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                2, nomres(1), valres(1), icodre(1), 2)
    e = valres(1)
    nu = valres(2)
!
    deuxmu = e/(1.d0+nu)
    troisk = e/(1.d0-2.d0*nu)
!
!
    if (cplan) deps(3)=-nu/(1.d0-nu)*(deps(1)+deps(2)) +(1.d0+nu)/(1.d0-nu)
    depsmo = (deps(1)+deps(2)+deps(3))/3.d0
    do 115 k = 1, ndimsi
        depsdv(k) = deps(k) - depsmo * kron(k)
115  end do
!
!
    do 145 k = 1, ndimsi
        sigp(k) = sigm(k)+deuxmu*depsdv(k)+troisk*depsmo*kron(k)
145  end do
!
!
!      CALCUL DE DSIDEP(6,6) :
!     ------------------------
!
    if (option(1:14) .eq. 'RIGI_MECA_TANG' .or. option(1:9) .eq. 'FULL_MECA') then
        call r8inir(36, 0.d0, dsidep, 1)
!
        do 130 k = 1, 3
            do 131 j = 1, 3
                dsidep(k,j) = troisk/3.d0-deuxmu/(3.d0)
131          continue
130      continue
        do 120 k = 1, ndimsi
            dsidep(k,k) = dsidep(k,k) + deuxmu
120      continue
!
    endif
!
end subroutine
