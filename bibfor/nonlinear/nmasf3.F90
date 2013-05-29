subroutine nmasf3(nno, nbpg1, ipoids, ivf, idfde,&
                  imate, geom, deplm, sigm, vectu,&
                  compor)
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! TOLE CRS_1404
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/cast3d.h'
    include 'asterfort/dfdm3d.h'
    include 'asterfort/elraga.h'
    include 'asterfort/elref4.h'
    include 'asterfort/invjac.h'
    include 'asterfort/nmgeom.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rccoma.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/u2mess.h'
    integer :: nno, nbpg1, imate
    integer :: ipoids, ivf, idfde
    integer :: ipoid2, ivf2, idfde2
    character(len=16) :: compor(4)
    real(kind=8) :: geom(3, nno)
    real(kind=8) :: deplm(3, nno), dfdi(nno, 3)
    real(kind=8) :: def(6, 3, nno)
    real(kind=8) :: sigm(78, nbpg1)
    real(kind=8) :: vectu(3, nno)
!.......................................................................
!
!     BUT:  CALCUL  DE L' OPTION FORC_NODA
!           EN HYPO-ELASTICITE EN 3D POUR LE HEXA8 SOUS INTEGRE
!           STABILITE PAR ASSUMED STRAIN
!.......................................................................
! IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  NBPG1   : NOMBRE DE POINTS DE GAUSS
! IN  POIDSG  : POIDS DES POINTS DE GAUSS
! IN  VFF     : VALEUR  DES FONCTIONS DE FORME
! IN  DFDE    : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  IMATE   : ADRESSE MATERIAU CODE
! IN  GEOM    : COORDONEES DES NOEUDS
! IN  DEPLM   : DEPLACEMENT A L'INSTANT PRECEDENT
! IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
! OUT VECTU   : FORCES NODALES
!.......................................................................
!
!
    logical :: grand, calbn, axi
    integer :: codre
    character(len=8) :: nomres(2)
    character(len=16) :: phenom
    integer :: kpg, i, ii, ino, ia, j, k, kl, proj, nbpg2
    integer :: ndim, nnos, jgano, kp, iaa
    real(kind=8) :: d(6, 6), f(3, 3), eps(6), r, s
    real(kind=8) :: poids, poipg2(8)
    real(kind=8) :: jac, sigas(6, 8), invja(3, 3), bi(3, 8), hx(3, 4)
    real(kind=8) :: gam(4, 8), coopg2(24), h(8, 4), dh(4, 24)
    real(kind=8) :: qplus(72)
    real(kind=8) :: bn(6, 3, 8)
    real(kind=8) :: dfdx(8), dfdy(8), dfdz(8)
    real(kind=8) :: nu, nub, rac2, den, bid
    real(kind=8) :: valres(2)
    data h/ 1.d0, 1.d0, -1.d0,-1.d0,-1.d0,-1.d0, 1.d0, 1.d0,&
     &        1.d0,-1.d0, -1.d0, 1.d0,-1.d0, 1.d0, 1.d0,-1.d0,&
     &        1.d0,-1.d0,  1.d0,-1.d0, 1.d0,-1.d0, 1.d0,-1.d0,&
     &       -1.d0, 1.d0, -1.d0, 1.d0, 1.d0,-1.d0, 1.d0,-1.d0/
!
! - INITIALISATION
!   ==============
!
!    PROJ : INDICATEUR DE LA PROJECTION
!           0 AUCUNE
!           1 ADS
!           2 ASQBI
!
    if (compor(1) .eq. 'ELAS            ') then
        proj= 2
    else
        proj= 1
    endif
    rac2 = sqrt(2.d0)
    grand = .false.
!
! - INITIALISATION HEXAS8
    call elraga('HE8', 'FPG8    ', ndim, nbpg2, coopg2,&
                poipg2)
    call elref4('HE8', 'MASS', ndim, nno, nnos,&
                nbpg2, ipoid2, ivf2, idfde2, jgano)
!
! - CALCUL DES COEFFICIENTS BI (MOYENNE DES DERIVEES DES FCTS DE FORME)
    call r8inir(3*nno, 0.d0, bi, 1)
    den = 0.d0
    do 2 kpg = 1, nbpg2
        call dfdm3d(nno, kpg, ipoid2, idfde2, geom,&
                    dfdx, dfdy, dfdz, jac)
        den = den + jac
        do 3 ino = 1, nno
            bi(1,ino) = bi(1,ino) + jac * dfdx(ino)
            bi(2,ino) = bi(2,ino) + jac * dfdy(ino)
            bi(3,ino) = bi(3,ino) + jac * dfdz(ino)
 3      continue
 2  end do
    do 4 i = 1, 3
        do 5 ino = 1, nno
            bi(i,ino) = bi(i,ino)/ den
 5      continue
 4  end do
!
! - CALCUL DES COEFFICIENTS GAMMA
!
    do 6 i = 1, 4
        do 7 k = 1, 3
            hx(k,i) = 0.d0
            do 8 j = 1, nno
                hx(k,i) = hx(k,i) + h(j,i) * geom(k,j)
 8          continue
 7      continue
 6  end do
!
    do 9 i = 1, 4
        do 10 j = 1, nno
            s = 0.d0
            do 11 k = 1, 3
                s = s + hx(k,i) * bi(k,j)
11          continue
            gam(i,j) = 0.125d0 * (h(j,i) - s)
10      continue
 9  end do
!
! - CALCUL POUR LE POINT DE GAUSS CENTRAL
    kpg = 1
!
!  RECUP DU COEF DE POISSON POUR ASQBI
!
    call rccoma(imate, 'ELAS', 1, phenom, codre)
    nomres(1)='E'
    if (phenom .eq. 'ELAS') then
        nomres(2)='NU'
    else if (phenom.eq.'ELAS_ISTR') then
        nomres(2)='NU_LT'
    else if (phenom.eq.'ELAS_ORTH') then
        nomres(2)='NU_LT'
    else
        call assert(.false.)
    endif
!
    call rcvalb('FPG1', 1, 1, '+', imate,&
                ' ', phenom, 0, ' ', 0.d0,&
                1, nomres(2), valres(2), codre, 1)
    if (codre .eq. 0) then
        nu = valres(2)
    else
        call u2mess('F', 'ELEMENTS4_72')
    endif
!
    nub = nu/(1.d0-nu)
!
    axi = .false.
    call nmgeom(3, nno, axi, grand, geom,&
                kpg, ipoids, ivf, idfde, deplm,&
                .true., poids, dfdi, f, eps,&
                r)
!
!      CALCUL DES PRODUITS SYMETR. DE F PAR N,
    do 41 i = 1, nno
        do 31 j = 1, 3
            def(1,j,i) = f(j,1)*dfdi(i,1)
            def(2,j,i) = f(j,2)*dfdi(i,2)
            def(3,j,i) = f(j,3)*dfdi(i,3)
            def(4,j,i) = (f(j,1)*dfdi(i,2) + f(j,2)*dfdi(i,1))/rac2
            def(5,j,i) = (f(j,1)*dfdi(i,3) + f(j,3)*dfdi(i,1))/rac2
            def(6,j,i) = (f(j,2)*dfdi(i,3) + f(j,3)*dfdi(i,2))/rac2
31      continue
41  continue
!
    do 180 i = 1, 72
        qplus(i) = sigm(i+6,kpg)
180  continue
!
    call r8inir(3*nno, 0.d0, vectu, 1)
    call r8inir(6*nbpg2, 0.d0, sigas, 1)
!
    calbn = .true.
!
!      OPERATEUR DE STABILISATION DU GRADIENT AUX 8 POINTS DE GAUSS
!
    do 290 kpg = 1, nbpg2
        kp = 3*(kpg-1)
        call invjac(nno, kpg, ipoid2, idfde2, geom,&
                    invja, jac)
!
        do 165 i = 1, 3
            dh(1,3*(kpg-1)+i) = coopg2(3*kpg-1) * invja(3,i) + coopg2(3*kpg) * invja(2,i)
165      continue
!
        do 166 i = 1, 3
            dh(2,3*(kpg-1)+i) = coopg2(3*kpg-2) * invja(3,i) + coopg2(3*kpg) * invja(1,i)
166      continue
!
        do 167 i = 1, 3
            dh(3,3*(kpg-1)+i) = coopg2(3*kpg-2) * invja(2,i) + coopg2(3*kpg-1) * invja(1,i)
167      continue
!
        do 168 i = 1, 3
            dh(4,3*(kpg-1)+i) = coopg2(3*kpg-2) * coopg2(3*kpg-1) * invja(3,i) + coopg2(3*kpg-1) &
                                &* coopg2(3*kpg) * invja(1,i) + coopg2(3*kpg-2) * coopg2(3*kpg) *&
                                & invja(2,i)
168      continue
!
!
!  CALCUL DE BN AU POINT DE GAUSS KPG
!
        call cast3d(proj, gam, dh, def, nno,&
                    kpg, nub, nu, d, calbn,&
                    bn, jac, bid)
!
!    CONTRAINTES DE HOURGLASS
!
        do 32 i = 1, 6
            ii = 12*(i-1)
            do 34 ia = 1, 4
                iaa = 3*(ia-1)
                do 35 j = 1, 3
                    sigas(i,kpg) = sigas(i,kpg) + qplus(ii+iaa+j) * dh(ia,kp+j)
35              continue
34          continue
32      continue
!
!     CALCUL DES FORCES INTERNES
!
        do 250 i = 1, nno
            do 240 j = 1, 3
                do 230 kl = 1, 3
                    vectu(j,i) = vectu(j,i) + (def(kl,j,i)+ bn(kl,j,i) )* (sigas(kl,kpg)+sigm(kl,&
                                 &1))*jac + (rac2*def(kl+ 3,j,i)+ bn(kl+3,j,i))* (sigas(kl+3,kpg)&
                                 &+sigm(kl+3, 1))*jac
230              continue
240          continue
250      continue
!
290  continue
!
end subroutine
