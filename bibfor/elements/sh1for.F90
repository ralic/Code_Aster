subroutine sh1for(xetemp, para, xidepm, sigma, xivect)
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
!               ELEMENT SHB15
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/chrp3d.h'
    include 'asterfort/mulmat.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rlosh6.h'
    include 'asterfort/s1calb.h'
    include 'asterfort/sh1ksi.h'
    integer :: lag
    real(kind=8) :: para(*)
    real(kind=8) :: xivect(*), xetemp(*)
    real(kind=8) :: xe(45), xidepm(*), sigma(*)
    real(kind=8) :: xcoq(3, 3), bksip(3, 15, 15), b(3, 15)
    real(kind=8) :: xcent(3), ppp(3, 3)
    real(kind=8) :: xl(3, 3), xxx(3), yyy(3)
    real(kind=8) :: tmpke(45, 45), tmpke2(45, 45)
    real(kind=8) :: xxg5(15), xyg5(15), xzg5(15), pxg5(15)
    real(kind=8) :: sigloc(6), sitmp1(15, 15), sitmp2(15, 15)
    real(kind=8) :: f(3, 15), sigmag(6), fq(45), poids
!
! INFOS:
! XE EST RANGE COMME CA: (XNOEUD1 YNOEUD1 ZNOEUD1, XNOEUD2 YNOEUD2
!... ZNOEUD2)
! DANS SHB15_TEST_NUM: ATTENTION A LA NUMEROTATION DES NOEUDS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! ON DEFINI LES POINTS GAUSS ET LES POIDS
!
!-----------------------------------------------------------------------
    integer :: i, ip, j, k, kk
    real(kind=8) :: ajac, rbid, zeta, zlamb
!-----------------------------------------------------------------------
    do 10 ip = 1, 5
        xzg5(ip) = 0.5d0
        xyg5(ip) = 0.5d0
        xzg5(ip+5) = 0.5d0
        xyg5(ip+5) = 0.d0
        xzg5(ip+10) = 0.d0
        xyg5(ip+10) = 0.5d0
10  end do
!
    do 20 ip = 1, 3
        xxg5(5*(ip-1)+1) = -0.906179845938664D0
        xxg5(5*(ip-1)+2) = -0.538469310105683D0
        xxg5(5*(ip-1)+3) = 0.d0
        xxg5(5*(ip-1)+4) = 0.538469310105683D0
        xxg5(5*(ip-1)+5) = 0.906179845938664D0
!
        pxg5(5*(ip-1)+1) = 0.236926885056189D0/6.d0
        pxg5(5*(ip-1)+2) = 0.478628670499366D0/6.d0
        pxg5(5*(ip-1)+3) = 0.568888888888889D0/6.d0
        pxg5(5*(ip-1)+4) = 0.478628670499366D0/6.d0
        pxg5(5*(ip-1)+5) = 0.236926885056189D0/6.d0
20  end do
!
!
!     ON FAIT UNE COPIE DE XETEMP DANS XE
    do 30 i = 1, 45
        xe(i) = xetemp(i)
30  end do
!
! TYPE DE LOI DE COMPORTEMENT:
!     IRDC = 1 : SHB6 MEME TYPE QUE SHB8 DANS PLEXUS
!     IRDC = 2 : C.P.
!     IRDC = 3 : 3D COMPLETE
!
!
!      IF(OPTION.EQ.'FORC_NODA') THEN
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C                                                                  C
!C ON CALCULE BSIGMA: SORTIE DANS OUT(24)                           C
!C                    ENTREE DE SIGMA DANS WORK(DIM=30)             C
!C                    ENTREE DU MATERIAU DANS D(1) ET D(2)          C
!C                    ENTREE DE QIALPHA PAS PRECEDENT               C
!C                                      DANS RE(1 A 12)             C
!C                              QIALPHA(J,I)=RE(J+(I-1)*3)          C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
    lag = nint(para(6))
    call r8inir(225, 0.d0, sitmp2, 1)
    do 400 j = 1, 15
        do 390 i = 1, 3
            f(i,j) = 0.d0
390      continue
400  end do
!C
!C CALCUL DE BKSIP(3,15,IP) DANS REPERE DE REFERENCE
!C      BKSIP(1,*,IP) = VECTEUR BX AU POINT GAUSS IP
!C      BKSIP(2,*,IP) = VECTEUR BY AU POINT GAUSS IP
!C      BKSIP(3,*,IP) = VECTEUR BZ AU POINT GAUSS IP
!C
    call sh1ksi(15, xxg5, xyg5, xzg5, bksip)
!
    do 460 ip = 1, 15
!C
!C RECHERCHE DE SIGMA DU POINT DE GAUSS GLOBAL
!C
        do 409 i = 1, 6
!C
! C'EST LES CONTRAINTES LOCALES POUR POUVOIR TRAITER LA PLASTICITE AVANT
! LES CONTRAINTES SONT PASSEES DANS LA CONFI.A LA FIN DU PAS DANS Q8PKCN
!C
            sigloc(i)=sigma((ip-1)*6+i)
409      continue
        zeta = xxg5(ip)
        zlamb = 0.5d0*(1.d0-zeta)
        do 420 i = 1, 3
            do 410 j = 1, 3
                xcoq(j,i) = zlamb*xe((i-1)*3+j) + (1.d0-zlamb)*xe(3*i+ 6+j)
410          continue
420      continue
        call rlosh6(xcoq, xcent, ppp, xl, xxx,&
                    yyy, rbid)
!C
!C PASSAGE DES CONTRAINTES AU REPERE GLOBAL
!C
        call chrp3d(ppp, sigloc, sigmag, 1)
        call s1calb(bksip(1, 1, ip), xe, b, ajac)
!C
!C CALCUL DE BQ.SIGMA SI LAGRANGIEN TOTAL
!C
        if (lag .eq. 1) then
            call r8inir(225, 0.d0, sitmp1, 1)
            do 422 j = 1, 15
                do 421 i = 1, 15
                    sitmp1(i,j) = sigmag(1)*b(1,i)*b(1,j) + sigmag(2)* b(2,i)*b(2,j) + sigmag(3)*&
                                  &b(3,i)*b(3,j) + sigmag( 4)*(b(1,i)*b(2,j)+b(2,i)*b(1,j)) + sig&
                                  &mag(6)* (b( 1,i)*b(3,j)+b(3,i)*b(1,j)) + sigmag(5)* (b(3,i)*b(&
                                  & 2,j)+b(2,i)*b(3,j))
421              continue
422          continue
            do 440 j = 1, 15
                do 430 i = 1, 15
                    sitmp2(i,j) = sitmp2(i,j) + ajac*pxg5(ip)*sitmp1( i,j)
430              continue
440          continue
        endif
!C
!C CALCUL DE B.SIGMA EN GLOBAL
!C
        poids = ajac*pxg5(ip)
        do 450 k = 1, 15
            f(1,k) = f(1,k) + poids * (b(1,k)*sigmag(1)+b(2,k)*sigmag( 4)+b(3,k)*sigmag(6))
            f(2,k) = f(2,k) + poids * (b(1,k)*sigmag(4)+b(2,k)*sigmag( 2)+b(3,k)*sigmag(5))
            f(3,k) = f(3,k) + poids * (b(1,k)*sigmag(6)+b(2,k)*sigmag( 5)+b(3,k)*sigmag(3))
450      continue
!C
!C SI LAGRANGIEN TOTAL: AJOUT DE FQ A F
!C
460  continue
    if (lag .eq. 1) then
        call r8inir(2025, 0.d0, tmpke, 1)
        do 490 kk = 1, 3
            do 480 i = 1, 15
                do 470 j = 1, 15
                    tmpke(i+(kk-1)*15,j+(kk-1)*15) = sitmp2(i,j)
470              continue
480          continue
490      continue
        call r8inir(2025, 0.d0, tmpke2, 1)
        do 510 j = 1, 15
            do 500 i = 1, 45
                tmpke2(i,(j-1)*3+1)=tmpke(i,j)
                tmpke2(i,(j-1)*3+2)=tmpke(i,j+15)
                tmpke2(i,(j-1)*3+3)=tmpke(i,j+30)
500          continue
510      continue
        call r8inir(2025, 0.d0, tmpke, 1)
        do 530 i = 1, 15
            do 520 j = 1, 45
                tmpke((i-1)*3+1,j)=tmpke2(i,j)
                tmpke((i-1)*3+2,j)=tmpke2(i+15,j)
                tmpke((i-1)*3+3,j)=tmpke2(i+30,j)
520          continue
530      continue
        call mulmat(45, 45, 1, tmpke, xidepm,&
                    fq)
        do 540 k = 1, 15
            f(1,k) = f(1,k) + fq((k-1)*3+1)
            f(2,k) = f(2,k) + fq((k-1)*3+2)
            f(3,k) = f(3,k) + fq((k-1)*3+3)
540      continue
    endif
!C
!C ATTENTION A L'ORDRE DE XIVECT
!C
    do 560 i = 1, 3
        do 550 j = 1, 15
            xivect((j-1)*3+i) = f(i,j)
550      continue
560  continue
!
end subroutine
