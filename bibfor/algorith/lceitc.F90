subroutine lceitc(fami, kpg, ksp, mat, option,&
                  mu, su, de, ddedt, vim,&
                  vip, r)
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
! person_in_charge: jerome.laverne at edf.fr
!
    implicit none
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
    integer :: mat, kpg, ksp
    real(kind=8) :: mu(3), su(3), de(6), ddedt(6, 6)
    real(kind=8) :: vim(*), vip(*), r
    character(len=16) :: option
    character(len=*) :: fami
!
!-----------------------------------------------------------------------
!            LOI DE COMPORTEMENT COHESIVE CZM_TAC_MIX
!            POUR LES ELEMENTS D'INTERFACE 2D ET 3D.
!
! IN : FAMI,KPG,KSP,MAT,OPTION
!      MU  : LAGRANGE
!      SU  : SAUT DE U
!      VIM : VARIABLES INTERNES
!
! OUT : DE    : DELTA, SOLUTION DE LA MINIMISATION
!       DDEDT : D(DELTA)/DT
!       VIP   : VARIABLES INTERNES MISES A JOUR
!       R     : PENALISATION DU LAGRANGE
!-----------------------------------------------------------------------
!
    logical :: resi, rigi, elas
    integer :: regime, regm, i, j, cod(4), cinema
    real(kind=8) :: sc, gc, dc, h, ka, kap, gap, sk, val(4)
    real(kind=8) :: t(3), pr(3, 3), tpo(3), tno, lbd, d
    character(len=12) :: nom(4)
    character(len=1) :: poum
!
    data nom /'GC','SIGM_C','PENA_LAG','CINEMATI'/
!-----------------------------------------------------------------------
!
! ---------------------------
! -- PRINCIPALES NOTATIONS --
! ---------------------------
!
! -  CARACTERISTIQUES DE LA ZONE COHESIVE
!    GC     : ENERGIE COHESIVE
!    SC     : CONTRAINTE CRITIQUE
!    DC     : OUVERTURE CRITIQUE
!    H      : DECROISSANCE DE LA CONTRAINTE COHESIVE
!    R      : PARAMETRE DE PENALISATION
!
! -  DONNEES D'ENTREE
!    MU     : LAGRANGE
!    SU     : SAUT DE U
!    VIM    : VARIABLES INTERNES
!             |1   : PLUS GRANDE NORME DU SAUT (KA)
!             |2   : REGIME DE LA LOI (REGM)
!             |      |0 : ADHERENCE INITIALE OU COURANTE
!             |      |1 : DISSIPATION
!             |      |2 : SURFACE LIBRE FINALE (RUPTURE)
!             |      |3 : SURFACE LIBRE (SOUS CONTRAINTE)
!             |3   : INDICATEUR D'ENDOMMAGEMENT
!             |      |0 : SAIN
!             |      |1 : ENDOMMAGE
!             |      |2 : CASSE
!             |4   : POURCENTAGE D'ENERGIE DISSIPEE (GA)
!             |5   : VALEUR DE L'ENERGIE DISSIPEE (GA*GC)
!             |6   : ENERGIE RESIDUELLE COURANTE (RIEN)
!             |7-9 : VALEURS DE DELTA
!
! -  DONNEES DE SORTIE
!    DE     : DELTA
!    DDEDT  : DERIVEE DE DELTA
!    VIP    : VARIABLES INTERNES MISES A JOUR
!
! -  GRANDEURS LOCALES
!    GA     : POURCENTAGE D'ENERGIE DISSIPEE
!    REGM   : REGIME DE FONCTIONNEMENT DE LA LOI A L'INSTANT PRECEDENT
!    REGIME : NOUVEAU REGIME DE FONCTIONNEMENT
!    KA     : OUVERTURE MAXIMALE COURANTE
!    SK     : CONTRAINTE CRITIQUE COURANTE
!    T      : FORCE COHESIVE LAMBDA + R.[U]
!    PR     : MATRICE DE PROJECTION SUIVANT LA CINEMATIQUE
!    TPO    : FORCE COHESIVE PROJETEE
!    TNO    : NORME DE LA FORCE COHESIVE PROJETEE
!
! --------------------
! -- INITIALISATION --
! --------------------
!
!    OPTION CALCUL DU RESIDU OU CALCUL DE LA MATRICE TANGENTE
    resi = option(1:4).eq.'FULL' .or. option(1:4).eq.'RAPH'
    rigi = option(1:4).eq.'FULL' .or. option(1:4).eq.'RIGI'
    elas = option(11:14).eq.'ELAS'
!
!    RECUPERATION DES PARAMETRES PHYSIQUES
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
!    PARAMETRE DU COMPORTEMENT DE LA LOI DE TALON-CURNIER
    gc = val(1)
    sc = val(2)
    dc = 2.d0*gc/sc
    h = sc/dc
    r = h * val(3)
!
!    ENTIER DECRIVANT LA CINEMATIQUE DU COMPORTEMENT DE L INTERFACE
!     (CODE DANS LA ROUTINE RCSTOC)
    cinema = int(val(4))
!
!    LECTURE DES VARIABLES INTERNES
!      GA   = VIM(4)
    regm = nint(vim(2))
    ka = vim(1)
    sk = max(0.d0,sc - h*ka)
!
! -----------------------------
! -- CALCUL DU SECOND MEMBRE --
! -----------------------------
!
!    FORCE COHESIVE AUGMENTEE : LAMBDA + R.[U]
    t(1) = mu(1) + r*su(1)
    t(2) = mu(2) + r*su(2)
    t(3) = mu(3) + r*su(3)
!
!    PROJECTEUR POUR UNE COMPOSANTE NORMALE POSITIVE
    call r8inir(9, 0.d0, pr, 1)
!
    if ((cinema.eq.0) .and. (t(1).ge.0.d0)) pr(1,1) = 1.d0
    pr(2,2) = 1.d0
    if ((cinema.eq.0) .or. (cinema.eq.2)) pr(3,3) = 1.d0
!
!    PROJECTION DE LA COMPOSANTE NORMALE POSITIVE
    tpo(1) = t(1)*pr(1,1)
    tpo(2) = t(2)*pr(2,2)
    tpo(3) = t(3)*pr(3,3)
!
!    NORME DU SECOND MEMBRE PROJETE
    tno = sqrt(tpo(1)**2 + tpo(2)**2 + tpo(3)**2)
!
! --------------------------------------------
! -- RESOLUTION DU PROBLEME 1D SUR LA NORME --
! --------------------------------------------
!
!    DETERMINATION DU REGIME DE COMPORTEMENT
    if (resi) then
!
!      SURFACE LIBRE (SOUS CONTRAINTE)
        if (tno .lt. r*ka) then
            regime = 3
!
!      ADHERENCE (INITIALE OU COURANTE)
        else if (tno .le. r*ka + sk) then
            regime = 0
!
!      ENDOMMAGEMENT
        else if (tno .lt. r*dc) then
            regime = 1
!
!      SURFACE LIBRE FINALE (RUPTURE)
        else
            regime = 2
        endif
!
!    SINON, ON N'ACTUALISE PAS LE REGIME DE FONCTIONNEMENT DE LA LOI
    else
        regime = regm
    endif
!
!
!    CALCUL DE L'ECOULEMENT 1D SELON LE REGIME DE COMPORTEMENT
    if (regime .eq. 3) then
        lbd = 1.d0/r
    else if (regime.eq.0) then
        if (tno .gt. 0.d0) then
            lbd = ka/tno
        else
            lbd = 0.d0
        endif
    else if (regime.eq.1) then
        lbd = (dc + (tno-r*dc)/(r-h)) / tno
    else
        lbd = 1.d0/r
    endif
!
! ------------------------------------
! -- CONSTRUCTION DE LA SOLUTION 3D --
! ------------------------------------
!
!    CALCUL DU SAUT DE DEPLACEMENT 3D
    if (resi) then
        call r8inir(6, 0.d0, de, 1)
        de(1) = lbd*tpo(1)
        de(2) = lbd*tpo(2)
        de(3) = lbd*tpo(3)
    endif
!
!    MISE A JOUR DES VARIABLES INTERNES
    if (resi) then
!
        kap = min( max(ka,lbd*tno) , dc )
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
    endif
!
! ----------------------
! -- MATRICE TANGENTE --
! ----------------------
!
    if (rigi) then
!
!      AJUSTEMENT POUR PRENDRE EN COMPTE *_MECA_ELAS
        if (elas) then
            if (regime .eq. 1) regime = 0
        endif
!
!      CALCUL DU COEFFICIENT 1D DE LA MATRICE TANGENTE
        if (regime .eq. 3) then
            d = 0.d0
        else if (regime.eq.0) then
            if (tno .gt. 0.d0) then
                d = -lbd/tno**2
            else
                d = 0.d0
            endif
        else if (regime.eq.1) then
            d = (1.d0/(r-h) - lbd)/tno**2
        else
            d = 0.d0
        endif
!
!      MATRICE TANGENTE 3D
        call r8inir(36, 0.d0, ddedt, 1)
        do 100 i = 1, 3
            do 110 j = 1, 3
                ddedt(i,j) = d * tpo(i)*tpo(j) + lbd*pr(i,j)
110          continue
100      continue
!
    endif
!
end subroutine
