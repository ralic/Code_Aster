subroutine lceifa(fami, kpg, ksp, mat, option,&
                  mu, su, deltap, ddedt, vim,&
                  vip, r)
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
! person_in_charge: jerome.laverne at edf.fr
    implicit none
#include "asterf_types.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
    character(len=16) :: option
    integer :: mat, kpg, ksp
    real(kind=8) :: mu(3), su(3), deltap(6), ddedt(6, 6), vim(*), vip(*), r
    character(len=*) :: fami
!
!-----------------------------------------------------------------------
!            LOI DE COMPORTEMENT COHESIVE DE FATIGUE CZM_FAT_MIX
!            POUR LES ELEMENTS D'INTERFACE 2D ET 3D.
!
! IN : FAMI,KPG,KSP,MAT,OPTION
!      MU  : LAGRANGE
!      SU  : SAUT DE U
!      VIM : VARIABLES INTERNES
!
! OUT : DELTAP : DELTA, SOLUTION DE LA MINIMISATION
!       DDEDT : D(DELTA)/DT
!       VIP   : VARIABLES INTERNES MISES A JOUR
!       R     : PENALISATION DU LAGRANGE
!-----------------------------------------------------------------------
!
    aster_logical :: resi, rigi, elas
    integer :: regime
    real(kind=8) :: sc, gc, dc, dc0, c, h, ka, sk, rk, val(4), kap, gap, incr
    real(kind=8) :: dn, tn, t(3), ddndtn, deltam
    integer :: cod(4)
    character(len=16) :: nom(4)
    character(len=1) :: poum
!
    data nom /'GC','SIGM_C','PENA_LAGR','RIGI_GLIS'/
!-----------------------------------------------------------------------
!
!
! OPTION CALCUL DU RESIDU OU CALCUL DE LA MATRICE TANGENTE
!
    resi = option(1:4).eq.'FULL' .or. option(1:4).eq.'RAPH'
    rigi = option(1:4).eq.'FULL' .or. option(1:4).eq.'RIGI'
    elas = option(11:14).eq.'ELAS'
!
! RECUPERATION DES PARAMETRES PHYSIQUES
!
    if (option .eq. 'RIGI_MECA_TANG') then
        poum = '-'
    else
        poum = '+'
    endif
!
    call rcvalb(fami, kpg, ksp, poum, mat,&
                ' ', 'RUPT_FRAG', 0, ' ', [0.d0],&
                4, nom, val, cod, 2)
!
    gc = val(1)
    sc = val(2)
    dc0 = 2*gc/sc
    h = sc/dc0
    r = h * val(3)
    c = h * val(4)
!
! -- INITIALISATION
!
!  CUMUL D'OUVERTURE
    ka = vim(1)
    deltam = vim(7)
!
    sk = max( 0.d0 , sc*(1.d0 - ka/dc0) )
    rk = max( 0.d0 , sc*(1.d0 - (ka-deltam)/dc0) )
    dc = dc0 - ka + deltam
!
!    FORCES COHESIVES AUGMENTEES
    t(1) = mu(1) + r*su(1)
    t(2) = mu(2) + r*su(2)
    t(3) = mu(3) + r*su(3)
    tn = t(1)
!
! -- CALCUL DE DELTA
!
!    SI RIGI_MECA_*
    if (.not. resi) then
        regime = nint(vim(2))
        goto 5000
    endif
!
!    CONTACT
    if (tn .lt. 0.d0) then
        regime = -1
        dn = 0
!
!    SURFACE LIBRE (SOUS CONTRAINTE)
    else if (tn .lt. r*deltam) then
        regime = 3
        dn = tn/r
!
!    ADHERENCE (INITIALE OU COURANTE)
    else if (tn .le. r*deltam + sk) then
        regime = 0
        dn = deltam
!
!    ENDOMMAGEMENT
    else if (tn .lt. r*dc) then
        regime = 1
        dn = (tn-rk)/(r-h)
!
!    SURFACE LIBRE FINALE (RUPTURE)
    else
        regime = 2
        dn = tn/r
    endif
!
    call r8inir(6, 0.d0, deltap, 1)
!    COMPOSANTE DE L'OUVERTURE :
    deltap(1) = dn
!    COMPOSANTES DE CISAILLEMENT : ELASTIQUE
    deltap(2) = t(2)/(c+r)
    deltap(3) = t(3)/(c+r)
!
!
! -- ACTUALISATION DES VARIABLES INTERNES
!   V1 :  PLUS GRANDE NORME DU SAUT (SATURE A DC)
!   V2 :  REGIME DE LA LOI
!        -1 : CONTACT
!         0 : ADHERENCE INITIALE OU COURANTE
!         1 : DISSIPATION
!         2 : SURFACE LIBRE FINALE (RUPTURE)
!         3 : SURFACE LIBRE (SOUS CONTRAINTE)
!   V3 :  INDICATEUR D'ENDOMMAGEMENT
!         0 : SAIN
!         1 : ENDOMMAGE
!         2 : CASSE
!   V4 :  SEUIL, POURCENTAGE D'ENERGIE DISSIPEE
!   V5 :  VALEUR DE L'ENERGIE DISSIPEE (V4*GC)
!   V6 :  NON UTILISEE POUR CETTE LOI
!   V7 A V9 : VALEURS DE DELTA
!
    incr = max(0.d0, deltap(1) - deltam)
    kap = min( ka + incr , dc0 )
!
    gap = kap/dc0 * (2 - kap/dc0)
    gap = max(0.d0,gap)
    gap = min(1.d0,gap)
!
    vip(1) = kap
    vip(2) = regime
!
    if (kap .eq. 0.d0) then
        vip(3) = 0.d0
    else if (kap.eq.dc0) then
        vip(3) = 2.d0
    else
        vip(3) = 1.d0
    endif
!
    vip(4) = gap
    vip(5) = gc*vip(4)
    vip(6) = 0.d0
    vip(7) = deltap(1)
    vip(8) = deltap(2)
    vip(9) = deltap(3)
!
!
! -- MATRICE TANGENTE
!
5000 continue
    if (.not. rigi) goto 9999
!
!    AJUSTEMENT POUR PRENDRE EN COMPTE *_MECA_ELAS
    if (elas) then
        if (regime .eq. 1) regime = 0
    endif
!
    call r8inir(36, 0.d0, ddedt, 1)
!
    ddedt(2,2) = 1/(c+r)
    ddedt(3,3) = 1/(c+r)
!
    if (regime .eq. 0) then
        ddndtn = 0
    else if (regime .eq. 1) then
        ddndtn = 1/(r-h)
    else if (regime .eq. 2) then
        ddndtn = 1/r
    else if (regime .eq. 3) then
        ddndtn = 1/r
    else if (regime .eq. -1) then
        ddndtn = 0
    endif
    ddedt(1,1) = ddndtn
!
9999 continue
!
end subroutine
