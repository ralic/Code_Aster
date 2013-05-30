subroutine sh1rig(xetemp, para, re)
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
    include 'asterfort/asbgl1.h'
    include 'asterfort/mulmat.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rlosh6.h'
    include 'asterfort/s1calb.h'
    include 'asterfort/sh1ksi.h'
    include 'asterfort/tran63.h'
    integer :: p, q, irdc
    real(kind=8) :: para(11)
    real(kind=8) :: xe(45), re(45, 45), lambda
    real(kind=8) :: xcoq(3, 3), bksip(3, 15, 15), b(3, 15)
    real(kind=8) :: xcent(3), ppp(3, 3), ppt(3, 3)
    real(kind=8) :: xl(3, 3), xxx(3), yyy(3)
    real(kind=8) :: tmptab(6, 45), tmpke(45, 45), cmatlo(6, 6)
    real(kind=8) :: tmpke2(45, 45), som, ppt1, ppt2, ppt3
    real(kind=8) :: bgl(6, 45), bglt(45, 6), em(6, 6), em3333(3, 3, 3, 3)
    real(kind=8) :: em2(3, 3, 3, 3), xxg5(15), xyg5(15), xzg5(15), pxg5(15)
    real(kind=8) :: xetemp(*)
!
!
!
!CCCCCCCCCCCCC ENTREES CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!          ICLE=2    ON CALCULE LA MATRICE DE RAIDEUR
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
! ON DEFINIT LES POINTS GAUSS ET LES POIDS
!
!
!-----------------------------------------------------------------------
    integer :: i, ip, j, k, l, m, n
!
    real(kind=8) :: ajac, coela1, coela2, elt, rbid, tt1, tt2
    real(kind=8) :: xcooef, xmu, xnu, xxl1, xxl2, zeta, zlamb
!
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
!      WRITE(6,*),'SHB15'
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
    irdc = nint(para(5))
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                  C
! ON CALCULE LA RAIDEUR : SORTIE DANS RE                           C
!                                                                  C
! SI IETAN = 1 , ALORS ON CALCULE AUSSI                            C
!                LA MATRICE TANGENTE PLASTIQUE                     C
!                                                                  C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! INTIALISATION LONGUEUR DES COTES
! CALCUL DES COEFF D ELANCEMENT A METTRE DANS LA MATRICE DE CPT
!
    xxl1 = 0.d0
    xxl2 = 0.d0
    tt1 = 0.d0
    tt2 = 0.d0
!
! STABILISATION ADAPTATIVE EN FONCTION DE LA DISTORTION DE L'ELEMENT
!
    do 40 i = 1, 3
! DISTANCE ENTRE 1 ET 4 (EPAISSEUR)
        tt1 = tt1+(xe(i+9)-xe(i))**2
! DISTANCE ENTRE 2 ET 5 (EPAISSEUR)
        tt2 = tt2+(xe(i+12)-xe(i+3))**2
! DISTANCE ENTRE 1 ET 2
        xxl1 = xxl1+(xe(i+3)-xe(i))**2
! DISTANCE ENTRE 2 ET 3
        xxl2 = xxl2+(xe(i+6)-xe(i+3))**2
40  end do
    xxl1 = sqrt(xxl1)
    xxl2 = sqrt(xxl2)
    tt1 = 0.5d0*(sqrt(tt1)+sqrt(tt2))
    coela1 = 5.d0/6.d0
    coela2 = 5.d0/6.d0
! ELANCEMENT DANS DIRECTION 2
!        WRITE(6,*) 'XXL1',XXL1
    elt = 6.d0*tt1/xxl1
    if (coela1 .gt. elt) coela1=elt
! ELANCEMENT DANS DIRECTION 1
    elt = 6.d0*tt1/xxl2
    if (coela2 .gt. elt) coela2=elt
! POUR L'INSTANT, ON NE MET PAS EN SERVICE:
    coela1 = 1.d0
    coela2 = 1.d0
!
    call r8inir(2025, 0.d0, re, 1)
    call r8inir(36, 0.d0, cmatlo, 1)
!
! ON DEFINI CMATLO: MATRICE DE COMPORTEMENT
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
!C
! CALCUL DE BKSIP(3,15,IP) DANS REPERE DE REFERENCE
!      BKSIP(1,*,IP) = VECTEUR Ni,ksi  AU POINT GAUSS IP
!      BKSIP(2,*,IP) = VECTEUR Ni,eta  AU POINT GAUSS IP
!      BKSIP(3,*,IP) = VECTEUR Ni,zeta AU POINT GAUSS IP
!
    call sh1ksi(15, xxg5, xyg5, xzg5, bksip)
!
! DEBUT DE LA BOUCLE SUR LES 15 PTS GAUSS
!
    do 270 ip = 1, 15
!
! DEFINITION DES 3 POINTS  COQUES
!
        zeta = xxg5(ip)
        zlamb = 0.5d0*(1.d0-zeta)
        do 60 i = 1, 3
            do 50 j = 1, 3
                xcoq(j,i) = zlamb*xe((i-1)*3+j) + (1.d0-zlamb)*xe(3*i+ 6+j)
50          continue
60      continue
!
! CALCUL DE PPP 3*3 PASSAGE DE GLOBAL A LOCAL,COQUE
! XCENT : COORD GLOBAL DU CENTRE DE L'ELEMENT
!
        call rlosh6(xcoq, xcent, ppp, xl, xxx,&
                    yyy, rbid)
!
! CALCUL DE B EN GLOBAL
!
! ATTENTION A L'ORDRE DE EPSILON:
!  FARID DANS SON PAPIER: 11 22 33 12 13 23
!  FARID DANS PLEXUS:     11 22 33 12 23 13
!
        call s1calb(bksip(1, 1, ip), xe, b, ajac)
        call r8inir(270, 0.d0, bgl, 1)
        call asbgl1(bgl, b)
!
! IL FAUT TRANSPOSER BGL_LOC
!
        call r8inir(270, 0.d0, bglt, 1)
        do 80 i = 1, 6
            do 70 j = 1, 45
                bglt(j,i) = bgl(i,j)
70          continue
80      continue
!
!
! IL NE RESTE PLUS QU'A FAIRE: BGLT * C * BGL
!
        do 100 i = 1, 6
            do 90 j = 1, 6
                em(i,j)=cmatlo(i,j)
90          continue
100      continue
!
! Passer EM du repère d'élément au repère global
!
        call r8inir(81, 0.d0, em3333, 1)
!      CALL R8INIR(81,0.D0,EM2,1)
        call tran63(em, em3333, 2)
        do 120 i = 1, 3
            do 110 j = 1, 3
                ppt(j,i) = ppp(i,j)
110          continue
120      continue
        do 200 k = 1, 3
            do 190 l = 1, 3
                do 180 p = 1, 3
                    do 170 q = 1, 3
!
                        som=0.d0
                        do 160 j = 1, 3
                            ppt1 = ppt(j,l)
                            do 150 i = 1, 3
                                ppt2 = ppt(i,k)
                                do 140 m = 1, 3
                                    ppt3 = ppt(m,p)
                                    do 130 n = 1, 3
                                        som=som+ ppt1*ppt2*em3333(i,j,&
                                        m,n)*ppt3*ppt(n,q)
130                                  continue
140                              continue
150                          continue
160                      continue
                        em2(k,l,p,q)=som
!
170                  continue
180              continue
190          continue
200      continue
        call tran63(em, em2, 1)
!
        call r8inir(270, 0.d0, tmptab, 1)
        call r8inir(2025, 0.d0, tmpke, 1)
        call r8inir(2025, 0.d0, tmpke2, 1)
        call mulmat(6, 6, 45, em, bgl,&
                    tmptab)
        call mulmat(45, 6, 45, bglt, tmptab,&
                    tmpke2)
!
! ASSEMBLAGE: KE=KE + POIDS*JACOBIAN*TMPKE
!
        do 220 j = 1, 15
            do 210 i = 1, 45
                tmpke(i,(j-1)*3+1)=tmpke2(i,j)
                tmpke(i,(j-1)*3+2)=tmpke2(i,j+15)
                tmpke(i,(j-1)*3+3)=tmpke2(i,j+30)
210          continue
220      continue
        call r8inir(2025, 0.d0, tmpke2, 1)
        do 240 i = 1, 15
            do 230 j = 1, 45
                tmpke2((i-1)*3+1,j)=tmpke(i,j)
                tmpke2((i-1)*3+2,j)=tmpke(i+15,j)
                tmpke2((i-1)*3+3,j)=tmpke(i+30,j)
230          continue
240      continue
        do 260 j = 1, 45
            do 250 i = 1, 45
                re(i,j)=re(i,j)+ajac*pxg5(ip)*tmpke2(i,j)
250          continue
260      continue
270  continue
end subroutine
