subroutine lceitc(fami, kpg, ksp, mat, option,&
                  mu, su, de, ddedt, vim,&
                  vip, r, pfluide)
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
! person_in_charge: jerome.laverne at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
    character(len=16) :: option
    integer :: mat, kpg, ksp
    real(kind=8) :: mu(3), su(3), de(6), ddedt(6, 6), vim(*), vip(*), r
    character(len=*) :: fami
    real(kind=8), optional, intent(in) :: pfluide
!
!-----------------------------------------------------------------------
!            LOI DE COMPORTEMENT COHESIVE CZM_LIN_MIX
!            POUR LES ELEMENTS D'INTERFACE 2D ET 3D.
!
! IN : FAMI,KPG,KSP,MAT,OPTION
!      MU  : LAGRANGE
!      SU  : SAUT DE U
!      VIM : VARIABLES INTERNES
!      PFLUIDE : POUR LES MODELES HM-XFEM
!
! OUT : DE    : DELTA, SOLUTION DE LA MINIMISATION
!       DDEDT : D(DELTA)/DT
!       VIP   : VARIABLES INTERNES MISES A JOUR
!       R     : PENALISATION DU LAGRANGE
!-----------------------------------------------------------------------
!
    aster_logical :: resi, rigi
    integer :: regime, i, j
    real(kind=8) :: sc, gc, dc, h, ka, val(4), tmp, ga, kap, gap
    real(kind=8) :: dn, tno, t(3), tpo(3), lbd
    integer :: cod(4)
    character(len=16) :: nom(4)
    character(len=1) :: poum
    real(kind=8) :: eps
    parameter    (eps=1.d-8)
!
    data nom /'GC','SIGM_C','PENA_LAGR','RIGI_GLIS'/
!-----------------------------------------------------------------------
!
!
! OPTION CALCUL DU RESIDU OU CALCUL DE LA MATRICE TANGENTE
!
    resi = option(1:4).eq.'FULL' .or. option(1:4).eq.'RAPH'
    rigi = option(1:4).eq.'FULL' .or. option(1:4).eq.'RIGI'
!
    if (option .eq. 'RIGI_MECA_TANG') then
        poum = '-'
    else
        poum = '+'
    endif
!
! RECUPERATION DES PARAMETRES PHYSIQUES
    call rcvalb(fami, kpg, ksp, poum, mat,&
                ' ', 'RUPT_FRAG', 0, ' ', [0.d0],&
                4, nom, val, cod, 2)
!
    gc = val(1)
    sc = val(2)
    dc = 2.d0*gc/sc
    h = sc/dc
    r = h * val(3)
!
! -- INITIALISATION
!
!    LECTURE DES VARIABLES INTERNES
    ga = vim(4)
!
!    CALCUL DE KAPPA : KA = DC*(1-SQRT(1-GA))
    tmp = sqrt(max(0.d0,1.d0-ga))
    tmp = dc*(1.d0-tmp)
    tmp = max(0.d0,tmp)
    tmp = min(dc,tmp)
    ka = tmp
!
!    FORCE COHESIVE AUGMENTEE : LAMBDA + R.[U] + PF
!    ON RAJOUTE PF DANS LE CALCUL DE LA CONTRAINTE (EFFECTIVE) POUR 
!    LE MODELE HM-XFEM
!
    if (present(pfluide)) then
       t(1) = mu(1) + r*su(1) + pfluide
       t(2) = mu(2) + r*su(2)
       t(3) = mu(3) + r*su(3)
    else
       t(1) = mu(1) + r*su(1)
       t(2) = mu(2) + r*su(2)
       t(3) = mu(3) + r*su(3)
    endif
!
!    PARTIE NORMALE POSITIVE
!
    if (t(1).ge.0.d0) then
       tpo(1) = t(1)
    else
       tpo(1) = 0.d0
    endif
    tpo(2) = t(2)
    tpo(3) = t(3)
    tno = sqrt(tpo(1)**2+tpo(2)**2+tpo(3)**2)
!
! -- CALCUL DE DELTA
!
!    SI RIGI_MECA_*
    if (.not. resi) then
        regime = nint(vim(2))
        goto 500
    endif
!
!    SI ZONE COHESIVE INTACTE A L'INSTANT PRECEDANT
    if (ka.eq.0.d0) then
!       ADHESION INITIALE
       if (tno.le.sc) then
          regime=-1
          lbd=0.d0
!       SURFACE LIBRE
       else if (tno.ge.r*dc) then
          regime=2
          lbd=1/r
!       ENDOMMAGEMENT
       else
          regime=1
          lbd=(tno-sc)/((r-h)*tno)
       endif
!    SI ZONE COHESIVE ANEANTIE A L'INSTANT PRECEDANT
    else if (ka.ge.dc) then
       regime=2
       lbd=1/r
!    SI ZONE COHESIVE ENDOMMAGEE A L'INSTANT PRECEDANT
    else
!       SURFACE LIBRE
       if (tno.ge.r*dc) then
          regime=2
          lbd=1/r
!       ENDOMMAGEMENT
       else if (tno.ge.(ka*(r-h)+sc-eps)) then
          regime=1
          lbd=(tno-sc)/((r-h)*tno)
!       RETOUR ELASTIQUE
       else
          regime=3
          lbd=ka/(ka*(r-h)+sc)
       endif
    endif
!
    call r8inir(6, 0.d0, de, 1)
    de(1) = lbd*tpo(1)
    de(2) = lbd*tpo(2)
    de(3) = lbd*tpo(3)
    dn = sqrt(de(1)**2+de(2)**2+de(3)**2)
!
!
! -- ACTUALISATION DES VARIABLES INTERNES
!   V1 :  PLUS GRANDE NORME DU SAUT (SEUIL EN SAUT)
!   V2 :  REGIME DE LA LOI
!        -1 : CONTACT
!         0 : ADHERENCE INITIALE OU COURANTE
!         1 : DISSIPATION
!         2 : SURFACE LIBRE FINALE (RUPTURE)
!         3 : RETOUR ELASTIQUE
!   V3 :  INDICATEUR D'ENDOMMAGEMENT
!         0 : SAIN
!         1 : ENDOMMAGE
!         2 : CASSE
!   V4 :  POURCENTAGE D'ENERGIE DISSIPEE
!   V5 :  VALEUR DE L'ENERGIE DISSIPEE (V4*GC)
!   V6 :  ENERGIE RESIDUELLE COURANTE
!        (NULLE POUR CE TYPE D'IRREVERSIBILITE)
!   V7 A V9 : VALEURS DE DELTA
!
    kap = min( max(ka,dn) , dc )
    gap = kap/dc * (2.d0 - kap/dc)
    gap = max(0.d0,gap)
    gap = min(1.d0,gap)
!
    vip(1) = kap
    vip(2) = regime
!
    if (kap .eq. 0.d0) then
        vip(3) = 0.d0
    else if (kap.eq.dc) then
        vip(3) = 2.d0
    else
        vip(3) = 1.d0
    endif
!
    vip(4) = gap
    vip(5) = gc*vip(4)
    vip(6) = 0.d0
    vip(7) = de(1)
    vip(8) = de(2)
    vip(9) = de(3)
!
! -- MATRICE TANGENTE
!
500 continue
    if (.not. rigi) goto 999
!
    call r8inir(36, 0.d0, ddedt, 1)
!
    if (regime .eq. 2) then
       ddedt(1,1) = 1/r
       ddedt(2,2) = 1/r
       ddedt(3,3) = 1/r
       if (t(1).lt.0.d0) ddedt(1,1) = 0.d0
    else if (regime .eq. 1) then
       ddedt(1,1) = 1/(r-h)-sc/(tno*(r-h))
       ddedt(2,2) = 1/(r-h)-sc/(tno*(r-h))
       ddedt(3,3) = 1/(r-h)-sc/(tno*(r-h))
       if (t(1).lt.0.d0) ddedt(1,1) = 0.d0
       do i = 1, 3
          do j = 1, 3
             ddedt(i,j) = ddedt(i,j)+sc/((r-h)*tno**3)*tpo(i)*tpo(j)
          end do
       end do
    else if (regime .eq. 3) then
       ddedt(1,1) = ka/(ka*(r-h)+sc)
       ddedt(2,2) = ka/(ka*(r-h)+sc)
       ddedt(3,3) = ka/(ka*(r-h)+sc)
       if (t(1).lt.0.d0) ddedt(1,1) = 0.d0
    endif
!
999 continue
!
end subroutine
