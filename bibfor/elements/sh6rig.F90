subroutine sh6rig(xetemp, para, re)
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
! TOLE CRP_20
!
!               ELEMENT SHB6
!
    implicit none
    include 'asterfort/assebg.h'
    include 'asterfort/klocgl.h'
    include 'asterfort/matini.h'
    include 'asterfort/matinv.h'
    include 'asterfort/mulmat.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rlosh6.h'
    include 'asterfort/s6calb.h'
    include 'asterfort/s6ksib.h'
    include 'asterfort/sh6ksi.h'
    integer :: irdc
!
    real(kind=8) :: xe(18), re(18, 18), xetemp(18)
    real(kind=8) :: xxg5(5), pxg5(5), xcoq(3, 3), bksip(3, 6, 5), b(3, 6)
    real(kind=8) :: xcent(3), ppp(3, 3), pppt(3, 3)
    real(kind=8) :: xl(3, 3), xxx(3), yyy(3)
    real(kind=8) :: tmptab(6, 18), tmpke(18, 18), cmatlo(6, 6)
    real(kind=8) :: tmpke2(18, 18)
    real(kind=8) :: gb(6, 2), gs(6, 2), xxgb(3, 2)
    real(kind=8) :: lambda, para(11)
!
    real(kind=8) :: dj(3, 3), bksip1(3, 6), xel(18), xrre(18, 18)
    real(kind=8) :: xxh(2, 3), xb01(18), xb02(18), b5(6, 18)
    real(kind=8) :: b1(6, 18), b2(6, 18), b3(3, 6), b4(6, 18)
    real(kind=8) :: btot1(6, 18), btot1t(18, 6)
    real(kind=8) :: btot2(6, 18), btot2t(18, 6)
    real(kind=8) :: xh01(36), xh02(36), uj(3, 3)
!
!CCCCCCCCCCCCC ENTREES CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!    ON CALCULE LA MATRICE DE RAIDEUR
!    XETEMP : Coordonnées des noeuds de l'élément
!    PARA   : Paramètres de matériaux et loi de comportement
!              (VOIR te0473.f)
!CCCCCCCCCCCCC SORTIE CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!    RE        MATRICE DE RAIDEUR
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! INITIALISATIONS
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! INFOS:
! XE EST RANGE COMME CA: (XNOEUD1 YNOEUD1 ZNOEUD1, XNOEUD2 YNOEUD2
!... ZNOEUD2)
! DANS SHB6_TEST_NUM: ATTENTION A LA NUMEROTATION DES NOEUDS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!-----------------------------------------------------------------------
    integer :: i, ia, ip, j, k
    real(kind=8) :: aj, ajac, coela1, coela2, elt, rbid, tt1
    real(kind=8) :: tt2, uns2, uns3, xajac, xcooef, xmu, xnu
    real(kind=8) :: xxl1, xxl2, zeta, zlamb
!-----------------------------------------------------------------------
    data gb/0.d0,-1.d0,0.d0,0.d0,1.d0,0.d0,&
     &        0.d0,0.d0,-1.d0,0.d0,0.d0,1.d0/
!
! VB: COORD DES NOEUDS DANS REPERE DE REFERENCE
!
!      DATA VB/ 0.D0, 1.D0, 0.D0, 0.D0, 1.D0, 0.D0,
!     &         0.D0, 0.D0, 1.D0, 0.D0, 0.D0, 1.D0,
!     &        -1.D0,-1.D0,-1.D0, 1.D0, 1.D0, 1.D0/
!
! ON DEFINIT LES POINTS GAUSS ET LES POIDS
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
    uns3 = 1.d0/3.d0
    uns2 = 1.d0/2.d0
!     ON FAIT UNE COPIE DE XETEMP DANS XE
    do 10 i = 1, 18
        xe(i) = xetemp(i)
10  end do
!
! TYPE DE LOI DE COMPORTEMENT:
!     IRDC = 1 : SHB6 MEME TYPE QUE SHB8 DANS PLEXUS
!     IRDC = 2 : C.P.
!     IRDC = 3 : 3D COMPLETE
!
    irdc = nint(para(5))
!
!      IF(ICLE.EQ.2)THEN
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                  C
! ON CALCULE LA RAIDEUR : SORTIE DANS RE                           C
!                                                                  C
! SI IETAN = 1 , ALORS ON CALCULE AUSSI                            C
!                LA MATRICE TANGENTE PLASTIQUE                     C
!                                                                  C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! INITIALISATION LONGUEUR DES COTES
! CALCUL DES COEFF D ELANCEMENT A METTRE DANS LA MATRICE DE CPT
!
    xxl1 = 0.d0
    xxl2 = 0.d0
    tt1 = 0.d0
    tt2 = 0.d0
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! STABILISATION ADAPTATIVE EN FONCTION DE LA DISTORTION DE L'ELEMENT
!
    do 40 i = 1, 3
! DISTANCE ENTRE 1 ET 4 (EPAISSEUR)
        tt1 = tt1+(xe(i+9)-xe(i))**2
! DISTANCE ENTRE 3 ET 6 (EPAISSEUR)
        tt2 = tt2+(xe(i+15)-xe(i+6))**2
! DISTANCE ENTRE 1 ET 2
        xxl1 = xxl1+(xe(i+3)-xe(i))**2
! DISTANCE ENTRE 2 ET 3
        xxl2 = xxl2+(xe(i+6)-xe(i+3))**2
40  end do
    xxl1 = sqrt(xxl1)
    xxl2 = sqrt(xxl2)
    tt1 = uns2*(sqrt(tt1)+sqrt(tt2))
    coela1 = 5.d0/6.d0
    coela2 = 5.d0/6.d0
! ELANCEMENT DANS DIRECTION 2
    elt = 6.d0*tt1/xxl1
    if (coela1 .gt. elt) coela1=elt
! ELANCEMENT DANS DIRECTION 1
    elt = 6.d0*tt1/xxl2
    if (coela2 .gt. elt) coela2=elt
! POUR L'INSTANT, ON NE MET PAS EN SERVICE:
    coela1 = 1.d0
    coela2 = 1.d0
!
    call r8inir(324, 0.d0, re, 1)
    call r8inir(36, 0.d0, cmatlo, 1)
! ON DEFINIT CMATLOC: MATRICE DE COMPORTEMENT
!
    xnu = para(2)
    lambda = para(1)*para(2)/(1.d0-para(2)*para(2))
    xmu = 0.5d0*para(1)/ (1.d0+para(2))
    cmatlo(1,1) = lambda + 2.d0*xmu
    cmatlo(2,2) = lambda + 2.d0*xmu
    if (irdc .eq. 1) then
! COMPORTEMENT SHB6 PLEXUS
        cmatlo(3,3) = para(1)
    endif
!
    if (irdc .eq. 2) then
! COMPORTEMENT C.P.
        cmatlo(3,3) = 0.d0
    endif
!
    cmatlo(1,2) = lambda
    cmatlo(2,1) = lambda
    cmatlo(4,4) = xmu
    cmatlo(5,5) = xmu
    cmatlo(6,6) = xmu
!
    if (irdc .eq. 3) then
! COMPORTEMENT LOI TRIDIM MMC 3D
        xnu = para(2)
        xcooef = para(1)/((1.d0+xnu)*(1.d0-2.d0*xnu))
        cmatlo(1,1) = (1.d0-xnu)*xcooef
        cmatlo(2,2) = (1.d0-xnu)*xcooef
        cmatlo(3,3) = (1.d0-xnu)*xcooef
        cmatlo(1,2) = xnu*xcooef
        cmatlo(2,1) = xnu*xcooef
        cmatlo(1,3) = xnu*xcooef
        cmatlo(3,1) = xnu*xcooef
        cmatlo(2,3) = xnu*xcooef
        cmatlo(3,2) = xnu*xcooef
        cmatlo(4,4) = (1.d0-2.d0*xnu)*0.5d0*xcooef
        cmatlo(5,5) = (1.d0-2.d0*xnu)*0.5d0*xcooef
        cmatlo(6,6) = (1.d0-2.d0*xnu)*0.5d0*xcooef
    endif
!
    data xb01/1.d0,0.d0,0.d0,&
     &          0.d0,1.d0,0.d0,&
     &          0.d0,0.d0,1.d0,&
     &          1.d0,1.d0,0.d0,&
     &          0.d0,0.d0,0.d0,&
     &          0.d0,0.d0,0.d0/
    data xb02/0.d0,0.d0,0.d0,&
     &         0.d0,0.d0,0.d0,&
     &         0.d0,0.d0,0.d0,&
     &         0.d0,0.d0,0.d0,&
     &         0.d0,0.45d0,0.45d0,&
     &         0.45d0,0.d0,0.45d0/
!
! Définition des coeficients constants C1---->C36 qui servent à faire
! des projections:
    data xh01/1.d0,1.d0,0.d0,0.d0,0.d0,0.d0,&
     &          0.d0,0.d0,1.d0,1.d0,0.d0,0.d0,&
     &          0.d0,0.d0,0.d0,0.d0,1.d0,1.d0,&
     &          1.d0,1.d0,1.d0,1.d0,0.d0,0.d0,&
     &          0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,&
     &          0.d0,0.d0,0.d0,0.d0,0.d0,0.d0/
    data xh02/0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,&
     &          0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,&
     &          0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,&
     &          0.d0,0.d0,0.d0,0.d0,0.d0,0.d0,&
     &          0.d0,0.d0,0.45d0,0.45d0,0.45d0,0.45d0,&
     &          0.45d0,0.45d0,0.d0,0.d0,0.45d0,0.45d0/
!
    do 710 ip = 1, 5
!
! DEFINITION DES 3 POINTS COQUES
!
        zeta = xxg5(ip)
        zlamb = 0.5d0*(1.d0-zeta)
        do 70 i = 1, 3
            do 60 j = 1, 3
                xcoq(j,i) = zlamb*xe((i-1)*3+j) + (1.d0-zlamb)*xe((i- 1+3)*3+j)
60          continue
70      continue
!
! CALCUL DE PPP 3*3 PASSAGE DE GLOBAL A LOCAL,COQUE
! XCENT : COORD GLOBAL DU CENTRE DE L'ELEMENT
!
        call rlosh6(xcoq, xcent, ppp, xl, xxx,&
                    yyy, rbid)
        do 90 i = 1, 3
            do 80 j = 1, 3
                pppt(j,i) = ppp(i,j)
80          continue
90      continue
!
!
! Transformer XE_Global en local XE_LOC
!
        call r8inir(18, 0.d0, xel, 1)
        do 100 i = 1, 6
            xel(3*i-2)=ppp(1,1)*xe(3*i-2)+ppp(2,1)*xe(3*i-1)+ ppp(3,1)&
            *xe(3*i)
            xel(3*i-1)=ppp(1,2)*xe(3*i-2)+ppp(2,2)*xe(3*i-1)+ ppp(3,2)&
            *xe(3*i)
            xel(3*i)=ppp(1,3)*xe(3*i-2)+ppp(2,3)*xe(3*i-1)+ ppp(3,3)*&
            xe(3*i)
100      continue
!
        call s6ksib(bksip1)
!
        call r8inir(18, 0.d0, b3, 1)
        call s6calb(bksip1, xel, b3, xajac)
!
        call assebg(b1, b3, xb01)
!
        call assebg(b4, b3, xb02)
!
        call sh6ksi(5, xxg5, bksip)
!
! CALCUL DES FONCTIONS DE FORME  GS
! XXGB = X  * GB
!
        call r8inir(6, 0.d0, xxgb, 1)
        do 130 j = 1, 3
            do 120 ia = 1, 2
                do 110 k = 1, 6
                    xxgb(j,ia) = xxgb(j,ia)+gb(k,ia)*xel(j+3*k-3)
110              continue
120          continue
130      continue
!
! GS = (BBB)  * XXGB
!
        call r8inir(12, 0.d0, gs, 1)
        do 160 i = 1, 6
            do 150 ia = 1, 2
                do 140 j = 1, 3
                    gs(i,ia)=gs(i,ia) + b3(j,i)*xxgb(j,ia)
140              continue
150          continue
160      continue
!
! GS = GB - GS
!
        do 180 i = 1, 2
            do 170 j = 1, 6
                gs(j,i)= (gb(j,i) - gs(j,i))*uns2
170          continue
180      continue
!
        call s6calb(bksip(1, 1, ip), xel, b, ajac)
!
!
! IL NE RESTE PLUS QU'A FAIRE: BGL_LOCT * C * BGL_LOC
!
!
!---   DJ = BKSIP * TRANSPOSE(XNOE)
!
        call matini(3, 3, 0.d0, dj)
        do 230 i = 1, 3
            do 220 j = 1, 3
                do 210 k = 1, 6
                    dj(j,i)=dj(j,i)+bksip(j,k,ip)*xel((k-1)*3+i)
210              continue
220          continue
230      continue
!
!-----   UJ(J,I)  MATRICE INVERSE DE DJ(J,I)
!
        call matinv('S', 3, dj, uj, aj)
        xxh(1,1)=xxg5(ip)*uj(1,1)+uns3*uj(1,3)
        xxh(1,2)=xxg5(ip)*uj(2,1)+uns3*uj(2,3)
        xxh(1,3)=xxg5(ip)*uj(3,1)+uns3*uj(3,3)
        xxh(2,1)=xxg5(ip)*uj(1,2)+uns3*uj(1,3)
        xxh(2,2)=xxg5(ip)*uj(2,2)+uns3*uj(2,3)
        xxh(2,3)=xxg5(ip)*uj(3,2)+uns3*uj(3,3)
!
! Définition de la matrice B2:
!
        call r8inir(108, 0.d0, b2, 1)
        do 240 j = 1, 6
            b2(1,j)=xh01(1)*xxh(1,1)*gs(j,1)+xh01(2)*xxh(2,1)*gs(j,2)
            b2(1,j+6)= xh01(3)*xxh(1,2)*gs(j,1)+xh01(4)*xxh(2,2)*gs(j,&
            2)
            b2(1,j+12)=xh01(5)*xxh(1,3)*gs(j,1)+xh01(6)*xxh(2,3)*gs(j,&
            2)
!
            b2(2,j)=xh01(7)*xxh(1,1)*gs(j,1)+xh01(8)*xxh(2,1)*gs(j,2)
            b2(2,j+6)=xh01(9)*xxh(1,2)*gs(j,1)+xh01(10)*xxh(2,2)*gs(j,&
            2)
            b2(2,j+12)=xh01(11)*xxh(1,3)*gs(j,1)+xh01(12)*xxh(2,3)*gs(&
            j,2)
!
            b2(3,j)=xh01(13)*xxh(1,1)*gs(j,1)+xh01(14)*xxh(2,1)*gs(j,&
            2)
            b2(3,j+6)=xh01(15)*xxh(1,2)*gs(j,1)+xh01(16)*xxh(2,2)*gs(&
            j,2)
            b2(3,j+12)=xh01(17)*xxh(1,3)*gs(j,1)+xh01(18)*xxh(2,3)*gs(&
            j,2)
!
            b2(4,j)=xh01(19)*xxh(1,2)*gs(j,1)+xh01(20)*xxh(2,2)*gs(j,&
            2)
            b2(4,j+6)=xh01(21)*xxh(1,1)*gs(j,1)+xh01(21)*xxh(2,1)*gs(&
            j,2)
!
            b2(5,j+6)=xh01(27)*xxh(1,3)*gs(j,1)+xh01(28)*xxh(2,3)*gs(&
            j,2)
            b2(5,j+12)=xh01(29)*xxh(1,2)*gs(j,1)+xh01(30)*xxh(2,2)*gs(&
            j,2)
!
            b2(6,j)=xh01(31)*xxh(1,3)*gs(j,1)+xh01(32)*xxh(2,3)*gs(j,&
            2)
            b2(6,j+12)=xh01(35)*xxh(1,1)*gs(j,1)+xh01(36)*xxh(2,1)*gs(&
            j,2)
240      continue
!
!
! Définition de la matrice B5:
!
        call r8inir(108, 0.d0, b5, 1)
        do 250 j = 1, 6
            b5(1,j)=xh02(1)*xxh(1,1)*gs(j,1)+xh02(2)*xxh(2,1)*gs(j,2)
            b5(1,j+6)=xh02(3)*xxh(1,2)*gs(j,1)+xh02(4)*xxh(2,2)*gs(j,&
            2)
            b5(1,j+12)=xh02(5)*xxh(1,3)*gs(j,1)+xh02(6)*xxh(2,3)*gs(j,&
            2)
!
            b5(2,j)=xh02(7)*xxh(1,1)*gs(j,1)+xh02(8)*xxh(2,1)*gs(j,2)
            b5(2,j+6)=xh02(9)*xxh(1,2)*gs(j,1)+xh02(10)*xxh(2,2)*gs(j,&
            2)
            b5(2,j+12)=xh02(11)*xxh(1,3)*gs(j,1)+xh02(12)*xxh(2,3)*gs(&
            j,2)
!
            b5(3,j)=xh02(13)*xxh(1,1)*gs(j,1)+xh02(14)*xxh(2,1)*gs(j,&
            2)
            b5(3,j+6)=xh02(15)*xxh(1,2)*gs(j,1)+xh02(16)*xxh(2,2)*gs(&
            j,2)
            b5(3,j+12)=xh02(17)*xxh(1,3)*gs(j,1)+xh02(18)*xxh(2,3)*gs(&
            j,2)
!
            b5(4,j)=xh02(19)*xxh(1,2)*gs(j,1)+xh02(20)*xxh(2,2)*gs(j,&
            2)
            b5(4,j+6)=xh02(21)*xxh(1,1)*gs(j,1)+xh02(21)*xxh(2,1)*gs(&
            j,2)
!
            b5(5,j+6)=xh02(27)*xxh(1,3)*gs(j,1)+xh02(28)*xxh(2,3)*gs(&
            j,2)
            b5(5,j+12)=xh02(29)*xxh(1,2)*gs(j,1)+xh02(30)*xxh(2,2)*gs(&
            j,2)
!
            b5(6,j)=xh02(31)*xxh(1,3)*gs(j,1)+xh02(32)*xxh(2,3)*gs(j,&
            2)
            b5(6,j+12)=xh02(35)*xxh(1,1)*gs(j,1)+xh02(36)*xxh(2,1)*gs(&
            j,2)
250      continue
!
        call r8inir(108, 0.d0, btot1, 1)
        do 280 i = 1, 6
            do 270 j = 1, 18
                btot1(i,j)= b1(i,j)+b2(i,j)
270          continue
280      continue
!
        do 300 i = 1, 6
            do 290 j = 1, 18
                btot1t(j,i) = btot1(i,j)
290          continue
300      continue
!
        call r8inir(108, 0.d0, btot2, 1)
        do 320 i = 1, 6
            do 310 j = 1, 18
                btot2(i,j)= b4(i,j)+b5(i,j)
310          continue
320      continue
!
        do 340 i = 1, 6
            do 330 j = 1, 18
                btot2t(j,i) = btot2(i,j)
330          continue
340      continue
!
        call r8inir(108, 0.d0, tmptab, 1)
        call r8inir(324, 0.d0, tmpke, 1)
        call r8inir(324, 0.d0, tmpke2, 1)
        call mulmat(6, 6, 18, cmatlo, btot1,&
                    tmptab)
        call mulmat(18, 6, 18, btot1t, tmptab,&
                    tmpke2)
!
! ASSEMBLAGE: KE=KE + POIDS*JACOBIAN*TMPKE
!
        do 400 j = 1, 6
            do 390 i = 1, 18
                tmpke(i,(j-1)*3+1)=tmpke2(i,j)
                tmpke(i,(j-1)*3+2)=tmpke2(i,j+6)
                tmpke(i,(j-1)*3+3)=tmpke2(i,j+12)
390          continue
400      continue
        call r8inir(324, 0.d0, tmpke2, 1)
        do 420 i = 1, 6
            do 410 j = 1, 18
                tmpke2((i-1)*3+1,j)=tmpke(i,j)
                tmpke2((i-1)*3+2,j)=tmpke(i+6,j)
                tmpke2((i-1)*3+3,j)=tmpke(i+12,j)
410          continue
420      continue
!
        do 440 i = 1, 18
            do 430 j = 1, 18
                xrre(i,j)= uns2*ajac*pxg5(ip)*tmpke2(i,j)
430          continue
440      continue
!
        call klocgl(xrre, pppt)
!
        do 460 i = 1, 18
            do 450 j = 1, 18
                re(i,j)= re(i,j)+xrre(i,j)
450          continue
460      continue
!
        call r8inir(108, 0.d0, tmptab, 1)
        call r8inir(324, 0.d0, tmpke, 1)
        call r8inir(324, 0.d0, tmpke2, 1)
        call mulmat(6, 6, 18, cmatlo, btot2,&
                    tmptab)
        call mulmat(18, 6, 18, btot2t, tmptab,&
                    tmpke2)
!
! ASSEMBLAGE: KE=KE + POIDS*JACOBIAN*TMPKE
!
        do 640 j = 1, 6
            do 630 i = 1, 18
                tmpke(i,(j-1)*3+1)=tmpke2(i,j)
                tmpke(i,(j-1)*3+2)=tmpke2(i,j+6)
                tmpke(i,(j-1)*3+3)=tmpke2(i,j+12)
630          continue
640      continue
        call r8inir(324, 0.d0, tmpke2, 1)
        do 660 i = 1, 6
            do 650 j = 1, 18
                tmpke2((i-1)*3+1,j)=tmpke(i,j)
                tmpke2((i-1)*3+2,j)=tmpke(i+6,j)
                tmpke2((i-1)*3+3,j)=tmpke(i+12,j)
650          continue
660      continue
!
        do 680 j = 1, 18
            do 670 i = 1, 18
                xrre(i,j)= uns2*ajac*pxg5(ip)*tmpke2(i,j)
670          continue
680      continue
!
        call klocgl(xrre, pppt)
!
        do 700 j = 1, 18
            do 690 i = 1, 18
                re(i,j)= re(i,j)+xrre(i,j)
690          continue
700      continue
!
        call r8inir(108, 0.d0, tmptab, 1)
        call r8inir(324, 0.d0, tmpke, 1)
        call r8inir(324, 0.d0, tmpke2, 1)
        call mulmat(6, 6, 18, cmatlo, btot1,&
                    tmptab)
        call mulmat(18, 6, 18, btot2t, tmptab,&
                    tmpke2)
!
! ASSEMBLAGE: KE=KE + POIDS*JACOBIAN*TMPKE
!
        do 684 j = 1, 6
            do 683 i = 1, 18
                tmpke(i,(j-1)*3+1)=tmpke2(i,j)
                tmpke(i,(j-1)*3+2)=tmpke2(i,j+6)
                tmpke(i,(j-1)*3+3)=tmpke2(i,j+12)
683          continue
684      continue
        call r8inir(324, 0.d0, tmpke2, 1)
        do 686 i = 1, 6
            do 685 j = 1, 18
                tmpke2((i-1)*3+1,j)=tmpke(i,j)
                tmpke2((i-1)*3+2,j)=tmpke(i+6,j)
                tmpke2((i-1)*3+3,j)=tmpke(i+12,j)
685          continue
686      continue
!
        do 688 j = 1, 18
            do 687 i = 1, 18
                xrre(i,j)= uns2*ajac*pxg5(ip)*tmpke2(i,j)
687          continue
688      continue
!
        call klocgl(xrre, pppt)
!
        do 701 j = 1, 18
            do 689 i = 1, 18
                re(i,j)= re(i,j)+xrre(i,j)
689          continue
701      continue
!
        call r8inir(108, 0.d0, tmptab, 1)
        call r8inir(324, 0.d0, tmpke, 1)
        call r8inir(324, 0.d0, tmpke2, 1)
        call mulmat(6, 6, 18, cmatlo, btot2,&
                    tmptab)
        call mulmat(18, 6, 18, btot1t, tmptab,&
                    tmpke2)
!
! ASSEMBLAGE: KE=KE + POIDS*JACOBIAN*TMPKE
!
        do 703 j = 1, 6
            do 702 i = 1, 18
                tmpke(i,(j-1)*3+1)=tmpke2(i,j)
                tmpke(i,(j-1)*3+2)=tmpke2(i,j+6)
                tmpke(i,(j-1)*3+3)=tmpke2(i,j+12)
702          continue
703      continue
        call r8inir(324, 0.d0, tmpke2, 1)
        do 705 i = 1, 6
            do 704 j = 1, 18
                tmpke2((i-1)*3+1,j)=tmpke(i,j)
                tmpke2((i-1)*3+2,j)=tmpke(i+6,j)
                tmpke2((i-1)*3+3,j)=tmpke(i+12,j)
704          continue
705      continue
!
        do 707 j = 1, 18
            do 706 i = 1, 18
                xrre(i,j)= uns2*ajac*pxg5(ip)*tmpke2(i,j)
706          continue
707      continue
!
        call klocgl(xrre, pppt)
!
        do 709 j = 1, 18
            do 708 i = 1, 18
                re(i,j)= re(i,j)+xrre(i,j)
708          continue
709      continue
710  end do
end subroutine
