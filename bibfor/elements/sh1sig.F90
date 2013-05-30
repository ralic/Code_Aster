subroutine sh1sig(xetemp, para, xidepp, dusx, sigma)
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
    include 'asterfort/dsdx3d.h'
    include 'asterfort/mulmat.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rlosh6.h'
    include 'asterfort/s1calb.h'
    include 'asterfort/sh1ksi.h'
    integer :: lag, irdc
    real(kind=8) :: sigma(*), para(11)
    real(kind=8) :: xe(45), lambda, dusx(*), xidepp(*)
    real(kind=8) :: xcoq(3, 3), bksip(3, 15, 15), b(3, 15)
    real(kind=8) :: xcent(3), ppp(3, 3), ppt(3, 3)
    real(kind=8) :: xl(3, 3), xxx(3), yyy(3)
    real(kind=8) :: cmatlo(6, 6)
    real(kind=8) :: xxg5(15), xyg5(15), xzg5(15)
    real(kind=8) :: deps(6), dusdx(9), ue(3, 15), rr2(3, 3)
    real(kind=8) :: depslo (6), sigloc(6), rr12(3, 3), xetemp(*)
!
!
!
! INITIALISATIONS
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! INFOS:
! XE EST RANGE COMME CA: (XNOEUD1 YNOEUD1 ZNOEUD1, XNOEUD2 YNOEUD2
!... ZNOEUD2)
! DANS SHB15_TEST_NUM: ATTENTION A LA NUMEROTATION DES NOEUDS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! ON DEFINI LES POINTS GAUSS ET LES POIDS
!
! 5 points sur la facette 1-2-3:
!
!
!-----------------------------------------------------------------------
    integer :: i, ip, j
    real(kind=8) :: ajac, rbid, xcooef, xmu, xnu, zeta, zlamb
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
!         PXG5(5*(IP-1)+1) =  0.236926885056189D0/6.D0
!         PXG5(5*(IP-1)+2) =  0.478628670499366D0/6.D0
!         PXG5(5*(IP-1)+3) =  0.568888888888889D0/6.D0
!         PXG5(5*(IP-1)+4) =  0.478628670499366D0/6.D0
!         PXG5(5*(IP-1)+5) =  0.236926885056189D0/6.D0
20  end do
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
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!C                                                                  C
!C ON CALCULE LES CONTRAINTES : SORTIE DANS OUT(30)                 C
!C                       CONTRAINTES LOCALES DANS CHAQUE COUCHE     C
!C                       SUR LA CONFIGURATION 1                     C
!C  LE DEPLACEMENT NODAL A L'AIR D'ETRE DANS WORK(1 A 45)           C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
    call r8inir(36, 0.d0, cmatlo, 1)
!C
!C UE: INCREMENT DE DEPLACEMENT NODAL, REPERE GLOBAL
!C
!C XE: DEBUT DU PAS
    do 290 j = 1, 15
        do 280 i = 1, 3
            ue(i,j)=xidepp((j-1)*3+i)
280      continue
290  end do
!
    lag = nint(para(6))
!C
!C ON DEFINIT CMATLO LOI MODIFIEE SHB15
!C
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
!C CALCUL DE BKSIP(3,8,IP) DANS REPERE DE REFERENCE
!C      BKSIP(1,*,IP) = VECTEUR BX AU POINT GAUSS IP
!C      BKSIP(2,*,IP) = VECTEUR BY AU POINT GAUSS IP
!C      BKSIP(3,*,IP) = VECTEUR BZ AU POINT GAUSS IP
!C
    call sh1ksi(15, xxg5, xyg5, xzg5, bksip)
!C
    do 380 ip = 1, 15
!C
!C DEFINITION DES 4 POINTS  COQUES
!C
        zeta = xxg5(ip)
        zlamb = 0.5d0*(1.d0-zeta)
        do 310 i = 1, 3
            do 300 j = 1, 3
                xcoq(j,i) = zlamb*xe((i-1)*3+j) + (1.d0-zlamb)*xe(3*i+ 6+j)
300          continue
310      continue
!C
!C CALCUL DE PPP 3*3 PASSAGE DE GLOBAL A LOCAL,COQUE
!C XCENT : COORD GLOBAL DU CENTRE DE L'ELEMENT
!C
        call rlosh6(xcoq, xcent, ppp, xl, xxx,&
                    yyy, rbid)
!C
!C CALCUL DE B : U_GLOBAL ---> EPS_GLOBAL
!C
        call s1calb(bksip(1, 1, ip), xe, b, ajac)
!C
!C CALCUL DE EPS DANS LE REPERE GLOBAL: 1 POUR DEFORMATIONS LINEAIRES
!C                                     2 POUR TERMES CARRES EN PLUS
        do 320 i = 1, 6
            deps(i)=0.d0
320      continue
        if (lag .eq. 1) then
!C ON AJOUTE LA PARTIE NON-LINEAIRE DE EPS
            call dsdx3d(2, b, ue, deps, dusdx,&
                        15)
        else
            call dsdx3d(1, b, ue, deps, dusdx,&
                        15)
        endif
!C
        do 340 i = 1, 3
            do 330 j = 1, 3
                ppt(j,i) = ppp(i,j)
330          continue
340      continue
        rr12(1,1) = dusdx(1)
        rr12(1,2) = dusdx(2)
        rr12(1,3) = dusdx(3)
        rr12(2,1) = dusdx(4)
        rr12(2,2) = dusdx(5)
        rr12(2,3) = dusdx(6)
        rr12(3,1) = dusdx(7)
        rr12(3,2) = dusdx(8)
        rr12(3,3) = dusdx(9)
        call mulmat(3, 3, 3, ppt, rr12,&
                    rr2)
        call mulmat(3, 3, 3, rr2, ppp,&
                    rr12)
        dusdx(1) = rr12(1,1)
        dusdx(2) = rr12(1,2)
        dusdx(3) = rr12(1,3)
        dusdx(4) = rr12(2,1)
        dusdx(5) = rr12(2,2)
        dusdx(6) = rr12(2,3)
        dusdx(7) = rr12(3,1)
        dusdx(8) = rr12(3,2)
        dusdx(9) = rr12(3,3)
!
        do 350 i = 1, 9
            dusx(i+(ip-1)*9)=dusdx(i)
350      continue
        do 360 i = 1, 6
            depslo (i) = 0.d0
            sigloc(i) = 0.d0
360      continue
        call chrp3d(ppp, deps, depslo, 2)
!C
!C CALCUL DE SIGMA DANS LE REPERE LOCAL
!C
        call mulmat(6, 6, 1, cmatlo, depslo,&
                    sigloc)
!C
!C CONTRAINTES ECRITES SOUS LA FORME:
!C               [SIG] = [S_11, S_22, S_33, S_12, S_23, S_13]
        do 370 i = 1, 6
!C ON LAISSE LES CONTRAINTES DANS LE REPERE LOCAL POUR LA PLASTICITE
            sigma((ip-1)*6+i)=sigloc(i)
370      continue
380  end do
end subroutine
