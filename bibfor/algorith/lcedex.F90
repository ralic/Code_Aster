subroutine lcedex(option, imate, npg, lgpg, s,&
                  q, vim, vip, alphap, dalfs)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    include 'asterfort/nmedal.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/u2mess.h'
    integer :: imate, npg, lgpg
    real(kind=8) :: s(2), q(2, 2), dalfs(2, 2), alphap(2)
    real(kind=8) :: vim(lgpg, npg), vip(lgpg, npg)
    character(len=16) :: option
!
!-----------------------------------------------------------------------
!     COMPORTEMENT DE L'ELEMENT A DISCONTINUITE POUR LA LOI
!    'ZONE COHESIVE' EXPONENTIELLE : CZM_EXP
!
! IN
!     NPG     : NOMBRE DE POINTS DE GAUSS
!     LGPG    : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
!     IMATE   : ADRESSE DU MATERIAU CODE
!     COMPOR  : COMPORTEMENT :  (1) = TYPE DE RELATION COMPORTEMENT
!                               (2) = NB VARIABLES INTERNES / PG
!                               (3) = HYPOTHESE SUR LES DEFORMATIONS
!     OPTION  : OPTION DEMANDEE : RIGI_MECA_TANG , FULL_MECA , RAPH_MECA
!     VIM     : VARIABLES INTERNES A L'INSTANT DU CALCUL PRECEDENT
!     S,Q     : QUANTITES CINEMATIQUES NECESSAIRES POUR CALCUL DU SAUT
!
! OUT
!     ALPHAP  : SAUT A L'INSTANT PLUS
!     VIP     : VARIABLES INTERNES A L'INSTANT ACTUEL
!     DALFS   : DEVIVEE DU SAUT PAR RAPPORT A S
!
!-----------------------------------------------------------------------
!
    logical :: resi, rigi, elas
    integer :: i, j, kpg, kpg2, spt
    real(kind=8) :: coef1, coef2, coef3
    real(kind=8) :: sigmc, gc, lc, seuil, norma
    real(kind=8) :: dsialf(2, 2), h(2, 2), det
    real(kind=8) :: valres(2)
    integer :: icodre(2)
    character(len=8) :: nomres(2), fami, poum
!
! - INITIALISATIONS :
! -------------------
!
    resi = option.eq.'RAPH_MECA' .or. option.eq.'FULL_MECA'
    rigi = option.eq.'FULL_MECA' .or. option.eq.'RIGI_MECA_TANG'
!
    coef1=0.d0
    coef2=0.d0
    coef3=0.d0
!
! RECUPERATION DES PARAMETRES PHYSIQUES :
! ---------------------------------------
    nomres(1) = 'GC'
    nomres(2) = 'SIGM_C'
    fami='FPG1'
    kpg2=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg2, spt, poum, imate,&
                ' ', 'RUPT_FRAG', 0, ' ', 0.d0,&
                2, nomres, valres, icodre, 2)
!
    gc = valres(1)
    sigmc = valres(2)
    lc = gc/sigmc
    seuil = vim(3,1)
!
! CALCUL DU SAUT DANS L'ELEMENT : 'ALPHAP'
! -----------------------------------------
!
    if (resi) then
        call nmedal(alphap, sigmc, gc, s, q,&
                    seuil)
    endif
!
    if (option .eq. 'RIGI_MECA_TANG') then
        alphap(1)=vim(1,1)
        alphap(2)=vim(2,1)
    endif
!
!
!  MISE A JOUR DES VARIABLES INTERNES
! -----------------------------------
! VI1 = alpha(1)
! VI2 = alpha(2)
! VI3 = seuil
! VI4 = regime (adoucissant : 1 ou decharge : 0)
! VI5 = pourcentage d'énergie dissipee
! VI6 = contrainte normale
! VI7 = contrainte tangentielle
!
    if (resi) then
!
        norma = sqrt( alphap(1)**2 + alphap(2)**2 )
!
        if (norma .le. seuil) then
!
            elas=.true.
            do 67 kpg = 1, npg
                vip(1,kpg) = alphap(1)
                vip(2,kpg) = alphap(2)
                vip(3,kpg) = vim(3,kpg)
                vip(4,kpg) = 0.d0
                vip(5,kpg) = vim(5,kpg)
                vip(6,kpg) = s(1) + q(1,1)*alphap(1) + q(1,2)*alphap( 2)
                vip(7,kpg) = s(2) + q(2,1)*alphap(1) + q(2,2)*alphap( 2)
67          continue
!
        else
!
            elas=.false.
            do 68 kpg = 1, npg
                vip(1,kpg) = alphap(1)
                vip(2,kpg) = alphap(2)
                vip(3,kpg) = norma
                vip(4,kpg) = 1.d0
                vip(5,kpg) = 1.d0 - exp(-norma/lc)
                vip(6,kpg) = s(1) + q(1,1)*alphap(1) + q(1,2)*alphap( 2)
                vip(7,kpg) = s(2) + q(2,1)*alphap(1) + q(2,2)*alphap( 2)
68          continue
!
        endif
!
    endif
!
!
! CALCUL DE DALFS TERME LIE AU COMPORTEMENT, NECESSAIRE POUR LA
! MATRICE TANGENTE :
!----------------------------------------------------------------
!
    if (rigi) then
!
!       CALCUL DE LA DERIVEE DU VECTEUR CONTRAINTE
!       PAR RAPPORT AU SAUT : 'DSIALF'
        if (option .eq. 'RIGI_MECA_TANG') elas=(nint(vim(4,1)).eq.0)
!
        if (elas) then
!
            if (seuil .eq. 0.d0) then
                call r8inir(4, 0.d0, dsialf, 1)
            else
                coef1 = sigmc*exp(-sigmc*seuil/gc)/seuil
                call r8inir(4, 0.d0, dsialf, 1)
                dsialf(1,1) = coef1
                dsialf(2,2) = coef1
            endif
        else
!
            norma = sqrt( alphap(1)**2 + alphap(2)**2 )
            coef2 = sigmc*exp(-sigmc*norma/gc)/norma
            coef3 = - coef2*( (1/norma) + sigmc/gc )/norma
!
            call r8inir(4, 0.d0, dsialf, 1)
            dsialf(1,1) = coef2 + coef3*alphap(1)*alphap(1)
            dsialf(1,2) = coef3*alphap(1)*alphap(2)
            dsialf(2,1) = coef3*alphap(2)*alphap(1)
            dsialf(2,2) = coef2 + coef3*alphap(2)*alphap(2)
!
        endif
!
!       CALCUL DES DALFS : DERIVEE DE ALPHA PAR RAPPORT A S :
!       DALFS  = (DSIALF  - Q  )^-1
!
        if ((elas) .and. (seuil .eq. 0.d0)) then
!
            call r8inir(4, 0.d0, dalfs, 1)
!
        else
!
            call r8inir(4, 0.d0, h, 1)
            call r8inir(4, 0.d0, dalfs, 1)
!
            do 71 i = 1, 2
                do 72 j = 1, 2
                    h(i,j) = dsialf(i,j) - q(i,j)
72              continue
71          continue
!
            det = h(1,1)*h(2,2) - h(1,2)**2
!
            if (abs(det) .le. 1.d-16) call u2mess('F', 'ALGORITH4_48')
!
            dalfs(1,1) = h(2,2)/det
            dalfs(2,2) = h(1,1)/det
            dalfs(1,2) = -h(1,2)/det
            dalfs(2,1) = -h(1,2)/det
!
        endif
!
    endif
!
end subroutine
