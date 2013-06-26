subroutine sh6eps(xetemp, xidepp, deploc, dusdx)
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
!               ELEMENT SHB6
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/assebg.h'
    include 'asterfort/depsh6.h'
    include 'asterfort/mulmat.h'
    include 'asterfort/rlosh6.h'
    include 'asterfort/s6calb.h'
    include 'asterfort/sh6ksi.h'
    integer :: lag
    real(kind=8) :: xe(18), xidepp(*)
    real(kind=8) :: xxg5(5), xcoq(3, 3), bksip(3, 6, 5), b(3, 6)
    real(kind=8) :: xcent(3), ppp(3, 3)
    real(kind=8) :: xl(3, 3), xxx(3), yyy(3), blocal(3, 6)
    real(kind=8) :: deps(6), ue(3, 6)
    real(kind=8) :: deploc(*), dusdx(*)
    real(kind=8) :: xetemp(*), dusx(9), pppt(3, 3)
    real(kind=8) :: bloc(6, 18), ueloc(3, 6), xmodif(18)
    integer :: i, ip, j
    real(kind=8) :: ajac, rbid, zeta, zlamb
!-----------------------------------------------------------------------
    data xmodif/1.d0,0.d0,0.d0,&
     &          0.d0,1.d0,0.d0,&
     &          0.d0,0.d0,1.d0,&
     &          1.d0,1.d0,0.d0,&
     &          0.d0,0.45d0,0.45d0,&
     &          0.45d0,0.d0,0.45d0/
!
!
!CCCCCCCCCCCCC ENTREES CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!          ICLE=7    ON CALCULE LES CONTRAINTES
!    OPTION=SIEF_ELGA    ON CALCULE LES CONTRAINTES
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! INITIALISATIONS
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! INFOS:
! XE EST RANGE COMME CA:
! (XNOEUD1 YNOEUD1 ZNOEUD1, XNOEUD2 YNOEUD2 ZNOEUD2,...)
! DANS SHB8_TEST_NUM: ATTENTION A LA NUMEROTATION DES NOEUDS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!      IF (NOMSHB.EQ.'SHB8') THEN
!
! ON DEFINI LES POINTS GAUSS ET LES POIDS
!
    xxg5(1) = -0.906179845938664d0
    xxg5(2) = -0.538469310105683d0
    xxg5(3) = 0.d0
    xxg5(4) = 0.538469310105683d0
    xxg5(5) = 0.906179845938664d0
!
! -----------------------------------------------------
! ON VERIFIE QUE LA CONNECTIVITE DONNE UN REPERE DIRECT
! SI CE N EST PAS LE CAS ON PERMUTE LES NOEUDS
! -----------------------------------------------------
!
!     ON FAIT UNE COPIE DE XETEMP DANS XE
    do 10 i = 1, 18
        xe(i) = xetemp(i)
10  end do
    do 360 j = 1, 6
        do 350 i = 1, 3
            ue(i,j) = xidepp((j-1)*3+i)
350      continue
360  end do
!
! CALCUL DE BKSIP(3,8,IP) DANS REPERE DE REFERENCE
!      BKSIP(1,*,IP) = VECTEUR BX AU POINT GAUSS IP
!      BKSIP(2,*,IP) = VECTEUR BY AU POINT GAUSS IP
!      BKSIP(3,*,IP) = VECTEUR BZ AU POINT GAUSS IP
!
    call sh6ksi(5, xxg5, bksip)
!
    do 450 ip = 1, 5
!
! DEFINITION DES 4 POINTS  COQUES
!
        zeta = xxg5(ip)
        zlamb = 0.5d0*(1.d0-zeta)
        do 380 i = 1, 3
            do 370 j = 1, 3
                xcoq(j,i) = zlamb*xe((i-1)*3+j) + (1.d0-zlamb)*xe((i- 1+3)*3+j)
370          continue
380      continue
!
! CALCUL DE PPP 3*3 PASSAGE DE GLOBAL A LOCAL,COQUE
! XCENT : COORD GLOBAL DU CENTRE DE L'ELEMENT
!
        call rlosh6(xcoq, xcent, ppp, xl, xxx,&
                    yyy, rbid)
        do 382 i = 1, 3
            do 381 j = 1, 3
                pppt(j,i) = ppp(i,j)
381          continue
382      continue
!
! CALCUL DE B : U_GLOBAL ---> EPS_GLOBAL
!
        call s6calb(bksip(1, 1, ip), xe, b, ajac)
        call mulmat(3, 3, 6, pppt, b,&
                    blocal)
!
! CALCUL DE EPS DANS LE REPERE GLOBAL: 1 POUR DEFORMATIONS LINEAIRES
!                                     2 POUR TERMES CARRES EN PLUS
        do 390 i = 1, 6
            deps(i) = 0.d0
390      continue
! Transformer matrice BLOCAL(3,6) dans le repère local en BLOC(6,18)
! dans le repère local et en tenant
! compte également des modifications sur les termes croisés ZY,ZX :
        call assebg(bloc, blocal, xmodif)
! Transformer les déplacements UE dans le repère global en UELOC local:
        call mulmat(3, 3, 6, pppt, ue,&
                    ueloc)
!
        lag = 0
!
        if (lag .eq. 1) then
! ON AJOUTE LA PARTIE NON-LINEAIRE DE EPS
            call depsh6(2, bloc, ueloc, deps, dusx)
        else
            call depsh6(1, bloc, ueloc, deps, dusx)
        endif
!
        do 420 i = 1, 9
            dusdx(i+ (ip-1)*9) = dusx(i)
420      continue
!
! CONTRAINTES ECRITES SOUS LA FORME:
!               [SIG] = [S_11, S_22, S_33, S_12, S_23, S_13]
        do 440 i = 1, 6
! ON LAISSE LES CONTRAINTES DANS LE REPERE LOCAL POUR LA PLASTICITE
            deploc((ip-1)*6+i) = deps(i)
440      continue
450  end do
!
end subroutine
