subroutine lceiex(fami, kpg, ksp, mat, option,&
                  mu, su, de, ddedt, vim,&
                  vip, r, codret)
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
! person_in_charge: kyrylo.kazymyrenko at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"


    character(len=16) :: option
    integer :: mat, kpg, ksp, i, codret
    real(kind=8) :: mu(3), su(3), de(6), ddedt(6, 6), vim(*), vip(*), bmin, bmax, res, deriv
    character(len=*) :: fami
!
!-----------------------------------------------------------------------
!            LOI DE COMPORTEMENT COHESIVE CZM_EXP_MIX
!            POUR LES ELEMENTS D'INTERFACE 2D ET 3D.
!


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
!
!-----------------------------------------------------------------------
!
    aster_logical :: resi, rigi, elas
    integer :: regime
    real(kind=8) :: sc, gc, dc, dc1, c, h, ka, sk, val(4), tmp, ga, kap, gap, r
    real(kind=8) :: dn, tn, t(3), ddndtn
    integer :: cod(4)
    character(len=16) :: nom(4)
    character(len=1) :: poum
    data nom /'GC','SIGM_C','PENA_LAGR','RIGI_GLIS'/

!
! OPTION CALCUL DU RESIDU OU CALCUL DE LA MATRICE TANGENTE
!

    resi = option(1:4).eq.'FULL' .or. option(1:4).eq.'RAPH'
    rigi = option(1:4).eq.'FULL' .or. option(1:4).eq.'RIGI'
    elas = option(11:14).eq.'ELAS'
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
!
!
    dc = 3.2*gc/sc
    h  = sc/dc
    r = h * val(3)
    c = h * val(4)
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
    sk = max(0.d0,sc*exp(-sc*ka/gc))
!
!
!    FORCES COHESIVES AUGMENTEES
    t(1) = mu(1) + r*su(1)
    t(2) = mu(2) + r*su(2)
    t(3) = mu(3) + r*su(3)
    tn = t(1)
!
!
!
! -- CALCUL DE DELTA
!
!    SI RIGI_MECA_*
    if (.not. resi) then
        regime = nint(vim(2))
        dn=vim(7)
        goto 5000
    endif
!
!    CONTACT
    if (tn .lt. 0.d0) then
        regime = -1
        dn = 0.d0
!
!    SURFACE LIBRE (SOUS CONTRAINTE)
    else if (tn .lt. r*ka) then
        regime = 3
        dn = tn/r
!
!    ADHERENCE (INITIALE OU COURANTE)
    else if (tn .le. r*ka + sk) then
        regime = 0
        dn = ka
!
!    ENDOMMAGEMENT
    else if (tn .lt. r*dc) then
        regime = 1
!

!    UTILISATION D UN ALGORITHME DE NEWTON
! ! !
! !
!     1 - DETERMINATION DE BORNES BMIN ET BMAX POUR NEWTON, AINSI
!         QUE D UN POINT D INITIALISATION JUDICIEUX  (solution de la bi-lineaire)
!
        dc1=dc*0.22
        dn = (tn-sc)/(r-(sc*3)/(3.2*dc))
        if (dn >= dc1) then
            dn=(tn-(3/7)*sc)/(r-(sc*3)/(7*3.2*(dc)))
            if (dn >= dc) then
                dn=0.999*dc
            endif
        endif
!
        bmin = dn/4
        bmax = max(dn*3,dc)
!
!
!     3 - BOUCLE DE CONVERGENCE DE L ALGORITHME DE NEWTON
        i = 0
200     continue
!         TEST DU CRITERE
        res = sc*exp(-sc*dn/gc) + r*dn-tn
        if (abs(res) .lt. 1.d-8 .or. i > 1000) goto 210
        i = i + 1
!
!         NOUVEL ESTIMATEUR
        deriv = -(sc**2/gc)*exp(-sc*dn/gc) + r
        dn = dn - res/deriv
!
!         PROJECTION SUR LES BORNES DE L'INTERVALLE
        if (dn .lt. bmin) dn = bmin
        if (dn .gt. bmax) dn = bmax
        goto 200
!
210     continue
!
        if (abs(res) .lt. 1.d-6 .and. i > 20) then
            call utmess("I","RUPTURE2_6")
        endif
!
        if (abs(res) .lt. 1.d-6 .and. i> 30) then
!          DIAGNOSTIC DE NON-CONVERGENCE
            if (i .ge. 30) then
                codret = 1
                goto 9999
            endif
        endif
!    SURFACE LIBRE FINALE (RUPTURE)
    else
        regime = 2
        dn = tn/r
    endif
    call r8inir(6, 0.d0, de, 1)
!    COMPOSANTE DE L'OUVERTURE :
    de(1) = dn
!    COMPOSANTES DE CISAILLEMENT : ELASTIQUE
    de(2) = t(2)/(c+r)
    de(3) = t(3)/(c+r)
!

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
!
! -- MATRICE TANGENTE

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
    ddedt(2,2) = 1.d0/(c+r)
    ddedt(3,3) = 1.d0/(c+r)
!
    if (regime .eq. 0) then
        ddndtn = 0.d0
    else if (regime .eq. 1) then
        ddndtn = 1/(r-(sc**2/gc)*exp(-sc*dn/gc))
    else if (regime .eq. 2) then
        ddndtn = 1.d0/r
    else if (regime .eq. 3) then
        ddndtn = 1.d0/r
    else if (regime .eq. -1) then
        ddndtn = 0.d0
    endif
    ddedt(1,1) = ddndtn

9999 continue
!
end subroutine
