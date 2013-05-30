subroutine sh6for(xetemp, para, xidepm, sigma, xivect)
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
!               ELEMENT SHB6
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/assebg.h'
    include 'asterfort/mulmat.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rlosh6.h'
    include 'asterfort/s6calb.h'
    include 'asterfort/sh6ksi.h'
    integer :: lag
    real(kind=8) :: para(*)
    real(kind=8) :: xivect(*), xetemp(*)
    real(kind=8) :: xe(18), xidepm(*), sigma(*), xe1(3, 6), xe2(3, 6)
    real(kind=8) :: xcoq(3, 3), bksip(3, 6, 5)
    real(kind=8) :: xcent(3), ppp(3, 3), pppt(3, 3)
    real(kind=8) :: xl(3, 3), xxx(3), yyy(3), xeloc(18)
    real(kind=8) :: tmpke(18, 18), tmpke2(18, 18)
    real(kind=8) :: xxg5(5), pxg5(5), ftemp(18), bloc(6, 18), blocal(3, 6)
    real(kind=8) :: sigloc(6), sitmp2(6, 6), xmodif(18)
    real(kind=8) :: f(3, 6), fq(18), floc(3, 6), fglob(3, 6)
    integer :: i, ip, j, k, kk
    real(kind=8) :: ajac, poids, rbid, zeta, zlamb
!-----------------------------------------------------------------------
    data xmodif/1.d0,0.d0,0.d0,&
     &          0.d0,1.d0,0.d0,&
     &          0.d0,0.d0,1.d0,&
     &          1.d0,1.d0,0.d0,&
     &          0.d0,0.45d0,0.45d0,&
     &          0.45d0,0.d0,0.45d0/
!
! INFOS:
! XE EST RANGE COMME CA: (XNOEUD1 YNOEUD1 ZNOEUD1, XNOEUD2 YNOEUD2
!... ZNOEUD2)
! DANS SHB15_TEST_NUM: ATTENTION A LA NUMEROTATION DES NOEUDS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! ON DEFINI LES POINTS GAUSS ET LES POIDS
!
    xxg5(1) = -0.906179845938664D0
    xxg5(2) = -0.538469310105683D0
    xxg5(3) = 0.d0
    xxg5(4) = 0.538469310105683D0
    xxg5(5) = 0.906179845938664D0
!
    pxg5(1) = 0.236926885056189D0
    pxg5(2) = 0.478628670499366D0
    pxg5(3) = 0.568888888888889D0
    pxg5(4) = 0.478628670499366D0
    pxg5(5) = 0.236926885056189D0
!
!     ON FAIT UNE COPIE DE XETEMP DANS XE
    do 10 i = 1, 18
        xe(i) = xetemp(i)
10  end do
!
    lag = nint(para(6))
    call r8inir(36, 0.d0, sitmp2, 1)
    do 30 j = 1, 6
        do 20 i = 1, 3
            f(i,j) = 0.d0
20      continue
30  end do
!C
!C CALCUL DE BKSIP(3,15,IP) DANS REPERE DE REFERENCE
!C      BKSIP(1,*,IP) = VECTEUR BX AU POINT GAUSS IP
!C      BKSIP(2,*,IP) = VECTEUR BY AU POINT GAUSS IP
!C      BKSIP(3,*,IP) = VECTEUR BZ AU POINT GAUSS IP
!C
    call sh6ksi(5, xxg5, bksip)
!
    do 170 ip = 1, 5
!C
!C RECHERCHE DE SIGMA DU POINT DE GAUSS GLOBAL
!C
        do 40 i = 1, 6
            sigloc(i)=sigma((ip-1)*6+i)
40      continue
        zeta = xxg5(ip)
        zlamb = 0.5d0*(1.d0-zeta)
        do 60 i = 1, 3
            do 50 j = 1, 3
                xcoq(j,i) = zlamb*xe((i-1)*3+j) + (1.d0-zlamb)*xe(3*i+ 6+j)
50          continue
60      continue
        call rlosh6(xcoq, xcent, ppp, xl, xxx,&
                    yyy, rbid)
!C
!C PASSAGE DES CONTRAINTES AU REPERE GLOBAL
!C
        do 80 i = 1, 3
            do 70 j = 1, 3
                pppt(j,i) = ppp(i,j)
70          continue
80      continue
! Passer les coordonnées globaux XE dans le repère local :
        do 81 i = 1, 6
            xe1(1,i) = xe(3*(i-1)+1)
            xe1(2,i) = xe(3*(i-1)+2)
            xe1(3,i) = xe(3*(i-1)+3)
81      continue
        call mulmat(3, 3, 6, pppt, xe1,&
                    xe2)
        do 82 i = 1, 6
            xeloc(3*(i-1)+1) = xe2(1,i)
            xeloc(3*(i-1)+2) = xe2(2,i)
            xeloc(3*(i-1)+3) = xe2(3,i)
82      continue
!
!
! CALCUL DE B : U_GLOBAL ---> EPS_GLOBAL
        call s6calb(bksip(1, 1, ip), xeloc, blocal, ajac)
!
! Transformer matrice BLOCAL(3,6) dans le repère local en BLOC(6,18)
!  dans le repère local et en tenant
! compte également des modifications sur les termes croisés ZY,ZX :
        call assebg(bloc, blocal, xmodif)
!C
!C CALCUL DE B.SIGMA EN GLOBAL
!C
        poids = 0.5d0*pxg5(ip)*ajac
        call r8inir(18, 0.d0, ftemp, 1)
        do 140 j = 1, 18
            do 130 i = 1, 6
                ftemp(j)= ftemp(j)+bloc(i,j)*sigloc(i)*poids
130          continue
140      continue
        do 150 i = 1, 6
            floc(1,i)=ftemp(i)
            floc(2,i)=ftemp(i+6)
            floc(3,i)=ftemp(i+12)
150      continue
!
! Transformer FLOC(3,6) dans le repère global :
!
        call mulmat(3, 3, 6, ppp, floc,&
                    fglob)
        do 160 k = 1, 6
            f(1,k) = f(1,k) + fglob(1,k)
            f(2,k) = f(2,k) + fglob(2,k)
            f(3,k) = f(3,k) + fglob(3,k)
160      continue
170  continue
!C
!C SI LAGRANGIEN TOTAL: AJOUT DE FQ A F
!C
    if (lag .eq. 1) then
        call r8inir(324, 0.d0, tmpke, 1)
        do 490 kk = 1, 3
            do 480 i = 1, 6
                do 470 j = 1, 6
                    tmpke(i+(kk-1)*6,j+(kk-1)*6) = sitmp2(i,j)
470              continue
480          continue
490      continue
        call r8inir(324, 0.d0, tmpke2, 1)
        do 510 j = 1, 6
            do 500 i = 1, 18
                tmpke2(i,(j-1)*3+1)=tmpke(i,j)
                tmpke2(i,(j-1)*3+2)=tmpke(i,j+6)
                tmpke2(i,(j-1)*3+3)=tmpke(i,j+12)
500          continue
510      continue
        call r8inir(324, 0.d0, tmpke, 1)
        do 530 i = 1, 6
            do 520 j = 1, 18
                tmpke((i-1)*3+1,j)=tmpke2(i,j)
                tmpke((i-1)*3+2,j)=tmpke2(i+6,j)
                tmpke((i-1)*3+3,j)=tmpke2(i+12,j)
520          continue
530      continue
        call mulmat(18, 18, 1, tmpke, xidepm,&
                    fq)
        do 540 k = 1, 6
            f(1,k) = f(1,k) + fq((k-1)*3+1)
            f(2,k) = f(2,k) + fq((k-1)*3+2)
            f(3,k) = f(3,k) + fq((k-1)*3+3)
540      continue
    endif
!C
!C ATTENTION A L'ORDRE DE XIVECT
!C
    do 560 i = 1, 3
        do 550 j = 1, 6
            xivect((j-1)*3+i) = f(i,j)
550      continue
560  continue
end subroutine
