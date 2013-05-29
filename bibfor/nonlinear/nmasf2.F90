subroutine nmasf2(nno, npg, ipoids, ivf, idfde,&
                  geom, typmod, sigm, dfdi, vectu)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    include 'asterfort/dfda2d.h'
    include 'asterfort/iniqs4.h'
    include 'asterfort/nmgeom.h'
    include 'asterfort/r8inir.h'
    integer :: nno, npg
    character(len=8) :: typmod(*)
    integer :: ipoids, ivf, idfde
    real(kind=8) :: geom(2, nno)
    real(kind=8) :: dfdi(nno, 2)
    real(kind=8) :: sigm(10, npg)
    real(kind=8) :: vectu(2, nno)
!
!.......................................................................
!
!     BUT:  CALCUL  DES OPTIONS FORC_NODA
!           EN HYPO-ELASTICITE EN 2D POUR LE QUAD4 SOUS INTEGRE
!           STABILITE PAR ASSUMED STRAIN
!.......................................................................
! IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  IPOIDS  : INDICE POIDS DES POINTS DE GAUSS
! IN  IVFF     :INDICE VALEUR  DES FONCTIONS DE FORME
! IN  IDFDE  :INDICE DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  GEOM    : COORDONEES DES NOEUDS
! IN  TYPMOD  : TYPE DE MODEELISATION
! IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
! OUT DFDI    : DERIVEE DES FONCTIONS DE FORME  AU DERNIER PT DE GAUSS
! OUT VECTU   : FORCES NODALES
!.......................................................................
!
!
    logical :: grand, axi
    integer :: kpg, n, i, j, kl, kpgs, proj, npgs
    real(kind=8) :: f(3, 3), eps(6), r
    real(kind=8) :: poids
    real(kind=8) :: rac2
!
!     AJ. VARIABLESPOIDSG
    real(kind=8) :: jac, sigas(4, 4), defc(4, 4, 2)
    real(kind=8) :: dh(8), gamma(8), coopg(8)
    real(kind=8) :: sdkdx(4), sdkdy(4), sdedx(4), sdedy(4), poi2sg(4)
    real(kind=8) :: sdfdy(4, 4), sdfdx(4, 4), sdfde(4, 4), sdfdk(4, 4)
    real(kind=8) :: qplus(6), defn(4, 4, 2), kron(3, 3), depbid(2, 4)
!
    data kron/1.d0,0.d0,0.d0,0.d0,1.d0,0.d0,0.d0,0.d0,1.d0/
!
!
! - INITIALISATION
!   ==============
!
!    PROJ : INDICATEUR DE LA PROJECTION
!           0 AUCUNE
!           1 OPTIMAL BENDING
!           2 INCOMPRESSIBLE
    proj = 2
    rac2 = sqrt(2.d0)
    call r8inir(8, 0.d0, depbid, 1)
    grand = .false.
    axi = typmod(1) .eq. 'AXIS'
!
    do 20 i = 1, 3
        do 10 j = 1, 3
            f(i,j) = kron(i,j)
10      continue
20  end do
!
!
! - INITIALISATION QUAS4
    call iniqs4(nno, sdfde, sdfdk, poi2sg, coopg)
!
! - CALCUL DU VECTEUR GAMMA
    gamma(1) = (&
               geom(1,4)* (geom(2,2)-geom(2,3))+ geom(1,2)* (geom(2,3)-geom(2,4))+ geom(1,3)* (ge&
               &om(2,4)-geom(2,2)))/ (2* (((geom(1,4)-geom(1,2))* (geom(2,1)-geom(2, 3)))+ (geom(&
               &1,1)-geom(1,3))* (geom(2,2)-geom(2,4)))&
               )
!
    gamma(2) = (&
               geom(1,4)* (geom(2,3)-geom(2,1))+ geom(1,3)* (geom(2,1)-geom(2,4))+ geom(1,1)* (ge&
               &om(2,4)-geom(2,3)))/ (2* (((geom(1,4)-geom(1,2))* (geom(2,1)-geom(2, 3)))+ (geom(&
               &1,1)-geom(1,3))* (geom(2,2)-geom(2,4)))&
               )
!
    gamma(3) = (&
               geom(1,4)* (geom(2,1)-geom(2,2))+ geom(1,1)* (geom(2,2)-geom(2,4))+ geom(1,2)* (ge&
               &om(2,4)-geom(2,1)))/ (2* (((geom(1,4)-geom(1,2))* (geom(2,1)-geom(2, 3)))+ (geom(&
               &1,1)-geom(1,3))* (geom(2,2)-geom(2,4)))&
               )
!
    gamma(4) = (&
               geom(1,3)* (geom(2,1)-geom(2,2))+ geom(1,1)* (geom(2,2)-geom(2,3))+ geom(1,2)* (ge&
               &om(2,3)-geom(2,1)))/ (2* (((geom(1,2)-geom(1,4))* (geom(2,1)-geom(2, 3)))- (geom(&
               &1,1)-geom(1,3))* (geom(2,2)-geom(2,4)))&
               )
!
!
! - CALCUL POUR LE POINT DE GAUSS CENTRAL
    kpg = 1
!
!
!
! - CALCUL DES ELEMENTS GEOMETRIQUES
!
!   CALCUL DE DFDI,R(EN AXI) ET POIDS
!
    call nmgeom(2, nno, axi, grand, geom,&
                kpg, ipoids, ivf, idfde, depbid,&
                .true., poids, dfdi, f, eps,&
                r)
!
!
!    OPERATEUR DE GRADIENT AU CENTRE
    do 90 n = 1, nno
        do 80 i = 1, 2
            defc(1,n,i) = f(i,1)*dfdi(n,1)
            defc(2,n,i) = f(i,2)*dfdi(n,2)
            defc(3,n,i) = 0.d0
            defc(4,n,i) = (f(i,1)*dfdi(n,2)+f(i,2)*dfdi(n,1))/rac2
80      continue
90  end do
!
!
!
! - CALCUL DE LA FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY
!
!    INITIALISATION
    npgs = 4
!
!    CONTRAINTES GENERALISEES
    do 180 i = 1, 6
        qplus(i) = sigm(i+4,kpg)
180  end do
!
!
!
!    OPERATEUR DE STABILISATION DU GRADIENT AU 4 POINTS DE GAUSS
    do 290 kpgs = 1, npgs
!
!
        call dfda2d(kpgs, nno, poi2sg(kpgs), sdfde, sdfdk,&
                    sdedx, sdedy, sdkdx, sdkdy, sdfdx,&
                    sdfdy, geom, jac)
!
        dh(2*kpgs-1) = coopg(2*kpgs-1)*sdkdx(kpgs) + coopg(2*kpgs)* sdedx(kpgs)
        dh(2*kpgs) = coopg(2*kpgs-1)*sdkdy(kpgs) + coopg(2*kpgs)* sdedy(kpgs)
!
!
        do 220 n = 1, nno
            do 210 i = 1, 2
!
!         QUAS4 SANS PROJECTION
!         ---------------------
                if (proj .eq. 0) then
                    defn(1,n,i) = f(i,1)*gamma(n)*dh(2*kpgs-1)
                    defn(2,n,i) = f(i,2)*gamma(n)*dh(2*kpgs)
                    defn(3,n,i) = 0.d0
                    defn(4,n,i) = ( f(i,1)*gamma(n)*dh(2*kpgs)+ f(i,2)*gamma(n)*dh(2*kpgs-1) )
!
!       OPTIMAL BENDING
!       ---------------
                else if (proj.eq.1) then
                    defn(1,n,i) = f(i,1)*gamma(n)*dh(2*kpgs-1)
                    defn(2,n,i) = f(i,2)*gamma(n)*dh(2*kpgs)
                    defn(3,n,i) = 0.d0
                    defn(4,n,i) = 0.d0
!
!       INCOMPRESSIBLE
!       --------------
                else if (proj.eq.2) then
                    defn(1,n,i) = f(i,1)*gamma(n)*dh(2*kpgs-1)* ( 0.5d0) + f(i,2)*gamma(n)*dh(2*k&
                                  &pgs)* (-0.5d0)
                    defn(2,n,i) = f(i,2)*gamma(n)*dh(2*kpgs)*0.5d0 + f(i,1)*gamma(n)*dh(2*kpgs-1)&
                                  &* (-0.5d0)
                    defn(3,n,i) = 0.d0
                    defn(4,n,i) = 0.d0
                endif
!
210          continue
220      continue
!
!
!    CONTRAINTES DE HOURGLASS
!
!       QUAS4 SANS PROJECTION
!       ---------------------
        if (proj .eq. 0) then
            sigas(1,kpgs) = qplus(1)*dh(2*kpgs-1) + qplus(2)*dh(2* kpgs)
            sigas(2,kpgs) = qplus(3)*dh(2*kpgs-1) + qplus(4)*dh(2* kpgs)
            sigas(3,kpgs) = 0.d0
            sigas(4,kpgs) = (qplus(5)*dh(2*kpgs)+qplus(6)*dh(2*kpgs-1) )/2
!
!       OPTIMAL BENDING
!       ---------------
        else if (proj.eq.1) then
            sigas(1,kpgs) = qplus(1)*dh(2*kpgs-1) + qplus(2)*dh(2* kpgs)
            sigas(2,kpgs) = qplus(3)*dh(2*kpgs-1) + qplus(4)*dh(2* kpgs)
            sigas(3,kpgs) = 0.d0
            sigas(4,kpgs) = 0.d0
!
!       INCOMPRESSIBLE
!       --------------
        else if (proj.eq.2) then
            sigas(1,kpgs) = (qplus(1)*dh(2*kpgs-1)+qplus(2)*dh(2*kpgs) )
            sigas(2,kpgs) = (qplus(3)*dh(2*kpgs-1)+qplus(4)*dh(2*kpgs) )
            sigas(3,kpgs) = 0.d0
            sigas(4,kpgs) = 0.d0
        endif
!
!
!
!   CALCUL DES FORCES INTERNES
!
        do 250 n = 1, nno
            do 240 i = 1, 2
                do 230 kl = 1, 3
                    vectu(i,n) = vectu(i,n) + defc(kl,n,i)*sigas(kl, kpgs)* jac + defn(kl,n,i)*si&
                                 &gas(kl,kpgs)*jac
230              continue
                vectu(i,n) = vectu(i,n) + defc(4,n,i)*sigas(4,kpgs)* jac* rac2 + defn(4,n,i)*siga&
                             &s(4,kpgs)*jac
240          continue
250      continue
!
        do 280 n = 1, nno
            do 270 i = 1, 2
                do 260 kl = 1, 3
                    vectu(i,n) = vectu(i,n) + defc(kl,n,i)*sigm(kl, kpg)*jac + defn(kl,n,i)*sigm(&
                                 &kl,kpg)*jac
260              continue
                vectu(i,n) = vectu(i,n) + defc(4,n,i)*sigm(4,kpg)* rac2*jac + defn(4,n,i)*sigm(4,&
                             &kpg)*jac
270          continue
280      continue
290  end do
!
!
end subroutine
