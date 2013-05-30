subroutine sh8sig(xetemp, para, xidepp, dusx, sigma)
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
!               ELEMENT SHB8
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/chrp3d.h'
    include 'asterfort/dsdx3d.h'
    include 'asterfort/mulmat.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rloshb.h'
    include 'asterfort/shbksi.h'
    include 'asterfort/shcalb.h'
    integer :: lag, irdc
    real(kind=8) :: sigma(*), para(11)
    real(kind=8) :: xe(24), dusx(*), xidepp(*)
    real(kind=8) :: xxg5(5), xcoq(3, 4), bksip(3, 8, 5), b(3, 8)
    real(kind=8) :: xcent(3), ppp(3, 3), pppt(3, 3)
    real(kind=8) :: xl(3, 4), xxx(3), yyy(3)
    real(kind=8) :: cmatlo(6, 6), rr2(3, 3), lambda
    real(kind=8) :: deps(6), dusdx(9), ue(3, 8)
    real(kind=8) :: depslo(6), sigloc(6)
    real(kind=8) :: rr12(3, 3)
    real(kind=8) :: xetemp(*)
!      REAL*8 PXG5(5)
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
!-----------------------------------------------------------------------
    integer :: i, ip, j
    real(kind=8) :: ajac, rbid, xcooef, xmu, xnu, zeta, zlamb
!
!-----------------------------------------------------------------------
    xxg5(1) = -0.906179845938664D0
    xxg5(2) = -0.538469310105683D0
    xxg5(3) = 0.d0
    xxg5(4) = 0.538469310105683D0
    xxg5(5) = 0.906179845938664D0
!
!      PXG5(1) = 0.236926885056189D0
!      PXG5(2) = 0.478628670499366D0
!      PXG5(3) = 0.568888888888889D0
!      PXG5(4) = 0.478628670499366D0
!      PXG5(5) = 0.236926885056189D0
!
! -----------------------------------------------------
! ON VERIFIE QUE LA CONNECTIVITE DONNE UN REPERE DIRECT
! SI CE N EST PAS LE CAS ON PERMUTE LES NOEUDS
! -----------------------------------------------------
!
!     ON FAIT UNE COPIE DE XETEMP DANS XE
    do 10 i = 1, 24
        xe(i) = xetemp(i)
10  end do
! TYPE DE LOI DE COMPORTEMENT:
!     IRDC = 1 : SHB8 TYPE PLEXUS
!     IRDC = 2 : C.P.
!     IRDC = 3 : 3D COMPLETE
!***         IRDC = OUT(1)
    irdc = nint(para(5))
    call r8inir(36, 0.d0, cmatlo, 1)
!
! UE: INCREMENT DE DEPLACEMENT NODAL, REPERE GLOBAL
!
! XE: DEBUT DU PAS
    do 360 j = 1, 8
        do 350 i = 1, 3
            ue(i,j) = xidepp((j-1)*3+i)
350      continue
360  end do
    lag = nint(para(6))
! ON DEFINIT CMATLO LOI MODIFIEE SHB8
!
    lambda = para(1)*para(2)/ (1-para(2)*para(2))
    xmu = 0.5d0*para(1)/ (1+para(2))
    cmatlo(1,1) = lambda + 2*xmu
    cmatlo(2,2) = lambda + 2*xmu
    if (irdc .eq. 1) then
! COMPORTEMENT SHB8 PLEXUS
!         CMATLO(3,3) = PROPEL(1)
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
!
        xnu = para(2)
        xcooef = para(1)/ ((1+xnu)* (1-2*xnu))
        cmatlo(1,1) = (1-xnu)*xcooef
        cmatlo(2,2) = (1-xnu)*xcooef
        cmatlo(3,3) = (1-xnu)*xcooef
        cmatlo(1,2) = xnu*xcooef
        cmatlo(2,1) = xnu*xcooef
        cmatlo(1,3) = xnu*xcooef
        cmatlo(3,1) = xnu*xcooef
        cmatlo(2,3) = xnu*xcooef
        cmatlo(3,2) = xnu*xcooef
        cmatlo(4,4) = (1-2*xnu)*0.5d0*xcooef
        cmatlo(5,5) = (1-2*xnu)*0.5d0*xcooef
        cmatlo(6,6) = (1-2*xnu)*0.5d0*xcooef
    endif
!
! CALCUL DE BKSIP(3,8,IP) DANS REPERE DE REFERENCE
!      BKSIP(1,*,IP) = VECTEUR BX AU POINT GAUSS IP
!      BKSIP(2,*,IP) = VECTEUR BY AU POINT GAUSS IP
!      BKSIP(3,*,IP) = VECTEUR BZ AU POINT GAUSS IP
!
    call shbksi(5, xxg5, bksip)
!
    do 450 ip = 1, 5
!
! DEFINITION DES 4 POINTS  COQUES
!
        zeta = xxg5(ip)
        zlamb = 0.5d0* (1.d0-zeta)
        do 380 i = 1, 4
            do 370 j = 1, 3
                xcoq(j,i) = zlamb*xe((i-1)*3+j) + (1.d0-zlamb)*xe((i- 1+4)*3+j)
370          continue
380      continue
!
! CALCUL DE PPP 3*3 PASSAGE DE GLOBAL A LOCAL,COQUE
! XCENT : COORD GLOBAL DU CENTRE DE L'ELEMENT
!
        call rloshb(xcoq, xcent, ppp, xl, xxx,&
                    yyy, rbid)
!
! CALCUL DE B : U_GLOBAL ---> EPS_GLOBAL
!
        call shcalb(bksip(1, 1, ip), xe, b, ajac)
!
! CALCUL DE EPS DANS LE REPERE GLOBAL: 1 POUR DEFORMATIONS LINEAIRES
!                                     2 POUR TERMES CARRES EN PLUS
        do 390 i = 1, 6
            deps(i) = 0.d0
390      continue
        if (lag .eq. 1) then
! ON AJOUTE LA PARTIE NON-LINEAIRE DE EPS
            call dsdx3d(2, b, ue, deps, dusdx,&
                        8)
        else
            call dsdx3d(1, b, ue, deps, dusdx,&
                        8)
        endif
!
! SORTIE DE DUSDX DANS PROPEL(1 A 9 * 5 PT DE GAUSS)
! POUR UTILISATION ULTERIEURE DANS Q8PKCN_SHB8
        do 410 i = 1, 3
            do 400 j = 1, 3
                pppt(j,i) = ppp(i,j)
400          continue
410      continue
        rr12(1,1) = dusdx(1)
        rr12(1,2) = dusdx(2)
        rr12(1,3) = dusdx(3)
        rr12(2,1) = dusdx(4)
        rr12(2,2) = dusdx(5)
        rr12(2,3) = dusdx(6)
        rr12(3,1) = dusdx(7)
        rr12(3,2) = dusdx(8)
        rr12(3,3) = dusdx(9)
        call mulmat(3, 3, 3, pppt, rr12,&
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
        do 420 i = 1, 9
            dusx(i+ (ip-1)*9) = dusdx(i)
420      continue
        do 430 i = 1, 6
            depslo(i) = 0.d0
            sigloc(i) = 0.d0
430      continue
        call chrp3d(ppp, deps, depslo, 2)
!
! CALCUL DE SIGMA DANS LE REPERE LOCAL
!
        call mulmat(6, 6, 1, cmatlo, depslo,&
                    sigloc)
!
! CONTRAINTES ECRITES SOUS LA FORME:
!               [SIG] = [S_11, S_22, S_33, S_12, S_23, S_13]
        do 440 i = 1, 6
! ON LAISSE LES CONTRAINTES DANS LE REPERE LOCAL POUR LA PLASTICITE
            sigma((ip-1)*6+i) = sigloc(i)
440      continue
450  end do
!
end subroutine
