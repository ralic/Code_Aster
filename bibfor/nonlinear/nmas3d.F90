subroutine nmas3d(fami, nno, nbpg1, ipoids, ivf,&
                  idfde, geom, typmod, option, imate,&
                  compor, lgpg, crit, instam, instap,&
                  deplm, deplp, angmas, sigm, vim,&
                  dfdi, def, sigp, vip, matuu,&
                  vectu, codret)
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
! aslint: disable=W1504
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/caatdb.h'
    include 'asterfort/calcdq.h'
    include 'asterfort/cast3d.h'
    include 'asterfort/codere.h'
    include 'asterfort/dfdm3d.h'
    include 'asterfort/elraga.h'
    include 'asterfort/elref4.h'
    include 'asterfort/invjac.h'
    include 'asterfort/lcegeo.h'
    include 'asterfort/matini.h'
    include 'asterfort/nmcomp.h'
    include 'asterfort/nmgeom.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/u2mess.h'
    integer :: nno, imate, lgpg, codret, nbpg1
    integer :: ipoids, ivf, idfde
    integer :: ipoid2, ivf2, idfde2
    character(len=*) :: fami
    character(len=8) :: typmod(*)
    character(len=16) :: option, compor(4)
    real(kind=8) :: instam, instap
    real(kind=8) :: geom(3, nno), crit(3)
    real(kind=8) :: deplm(3, nno), deplp(3, nno), dfdi(nno, 3)
    real(kind=8) :: def(6, 3, nno)
    real(kind=8) :: sigm(78, nbpg1), sigp(78, nbpg1)
    real(kind=8) :: vim(lgpg, nbpg1), vip(lgpg, nbpg1)
    real(kind=8) :: matuu(*), vectu(3, nno), angmas(3)
!.......................................................................
!
!     BUT:  CALCUL  DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
!           EN HYPO-ELASTICITE EN 3D POUR LE HEXA8 SOUS INTEGRE
!           STABILITE PAR ASSUMED STRAIN
!.......................................................................
! IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  NBPG1   : NOMBRE DE POINTS DE GAUSS
! IN  POIDSG  : POIDS DES POINTS DE GAUSS
! IN  VFF     : VALEUR  DES FONCTIONS DE FORME
! IN  DFDE    : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  DFDK    : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  GEOM    : COORDONEES DES NOEUDS
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  OPTION  : OPTION DE CALCUL
! IN  IMATE   : MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT
! IN  LGPG    : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
!               CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  INSTAM  : INSTANT PRECEDENT
! IN  INSTAP  : INSTANT DE CALCUL
! IN  DEPLM   : DEPLACEMENT A L'INSTANT PRECEDENT
! IN  DEPLP   : INCREMENT DE DEPLACEMENT
! IN  ANGMAS  : LES TROIS ANGLES DU MOT_CLEF MASSIF (AFFE_CARA_ELEM)
! IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
! IN  VIM     : VARIABLES INTERNES A L'INSTANT PRECEDENT
! OUT DFDI    : DERIVEE DES FONCTIONS DE FORME  AU DERNIER PT DE GAUSS
! OUT DEF     : PRODUIT DER. FCT. FORME PAR F   AU DERNIER PT DE GAUSS
! OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
! OUT VIP     : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA)
! OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
! OUT VECTU   : FORCES NODALES (RAPH_MECA ET FULL_MECA)
!.......................................................................
!
    logical :: grand, calbn, axi
    integer :: kpg, i, ii, ino, ia, j, k, kl, proj, cod(9), nbpg2
    integer :: ndim, nnos, jgano, kp, iaa
    real(kind=8) :: d(6, 6), f(3, 3), eps(6), deps(6), r, s, sigma(6), sign(6)
    real(kind=8) :: poids, poipg2(8), rbid
    real(kind=8) :: elgeom(10, 9)
    real(kind=8) :: jac, sigas(6, 8), invja(3, 3), bi(3, 8), hx(3, 4)
    real(kind=8) :: gam(4, 8), coopg2(24), h(8, 4), dh(4, 24)
    real(kind=8) :: qplus(72), qmoins(72), dq(72)
    real(kind=8) :: bn(6, 3, 8)
    real(kind=8) :: pqx(4), pqy(4), pqz(4)
    real(kind=8) :: dfdx(8), dfdy(8), dfdz(8)
    real(kind=8) :: valres(2), nu, nub, rac2, den, bid
    integer :: icodre
    character(len=8) :: nomres(2)
    character(len=16) :: optios
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
    calbn = .false.
!
! - CALCUL DES ELEMENTS GEOMETRIQUES SPECIFIQUES AU COMPORTEMENT
    call lcegeo(nno, nbpg1, ipoids, ivf, idfde,&
                geom, typmod, compor, 3, dfdi,&
                deplm, deplp, elgeom)
!
! - INITIALISATION CODES RETOURS
    do 1 kpg = 1, nbpg1
        cod(kpg) = 0
 1  end do
!
! - INITIALISATION HEXAS8
    call elraga('HE8', 'FPG8    ', ndim, nbpg2, coopg2,&
                poipg2)
    call elref4('HE8', 'MASS', ndim, nno, nnos,&
                nbpg2, ipoid2, ivf2, idfde2, jgano)
!
! - CALCUL DES COEFFICIENTS BI (MOYENNE DES DERIVEES DES FCTS DE FORME)
!
    call matini(3, nno, 0.d0, bi)
!
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
!
!
! - CALCUL DES ELEMENTS GEOMETRIQUES
!
!     CALCUL DE DFDI,F,EPS,DEPS ET POIDS
!
    do 70 j = 1, 6
        eps(j) = 0.d0
        deps(j) = 0.d0
70  end do
    axi = .false.
    call nmgeom(3, nno, axi, grand, geom,&
                kpg, ipoids, ivf, idfde, deplm,&
                .true., poids, dfdi, f, eps,&
                r)
!
!     CALCUL DE DEPS
    call nmgeom(3, nno, axi, grand, geom,&
                kpg, ipoids, ivf, idfde, deplp,&
                .false., poids, dfdi, f, deps,&
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
    do 100 i = 1, 3
        sign(i) = sigm(i,kpg)
100  end do
    do 65 i = 4, 6
        sign(i) = sigm(i,kpg)*rac2
65  end do
!
! - LOI DE COMPORTEMENT
    if (option(1:9) .eq. 'RAPH_MECA') then
        optios = 'FULL_MECA'
    else
        optios = option
    endif
!
    call nmcomp(fami, kpg, 1, 3, typmod,&
                imate, compor, crit, instam, instap,&
                6, eps, deps, 6, sign,&
                vim(1, kpg), optios, angmas, 10, elgeom(1, kpg),&
                sigma, vip(1, kpg), 36, d, 1,&
                rbid, cod(kpg))
!
! - ERREUR D'INTEGRATION
    if (cod(kpg) .eq. 1) then
        goto 320
    endif
!
!  RECUP DU COEF DE POISSON POUR ASQBI
!
    if (proj .eq. 2) then
        nomres(1)='E'
        if (compor(1) .eq. 'ELAS') then
            nomres(2)='NU'
        else if (compor(1).eq.'ELAS_ISTR') then
            nomres(2)='NU_LT'
        else if (compor(1).eq.'ELAS_ORTH') then
            nomres(2)='NU_LT'
        else
            call assert(.false.)
        endif
!
!
        call rcvalb(fami, kpg, 1, '-', imate,&
                    ' ', compor(1), 0, ' ', 0.d0,&
                    1, nomres(2), valres(2), icodre, 1)
        if (icodre .eq. 0) then
            nu = valres(2)
        else
            call u2mess('F', 'ELEMENTS4_72')
        endif
!
        nub = nu/(1.d0-nu)
    endif
!
    if (option(1:10) .eq. 'RIGI_MECA_' .or. option(1:9) .eq. 'FULL_MECA') then
!
        call r8inir(300, 0.d0, matuu, 1)
!
!     CALCUL DE KC (MATRICE DE RIGIDITE AU CENTRE)
!     --------------------------------------------
        call caatdb(nno, def, d, def, poids,&
                    matuu)
!
!           CORRECTION DE LA MATRICE DE RIGIDITE
!                 CALCUL DE KSTAB
!     --------------------------------------------
!
!        CALCUL DES TERMES EVALUES AUX 8 POINTS DE GAUSS
        do 160 kpg = 1, nbpg2
            call invjac(nno, kpg, ipoid2, idfde2, geom,&
                        invja, jac)
!
            do 161 i = 1, 3
                dh(1,3*(kpg-1)+i) = coopg2(3*kpg-1) * invja(3,i) + coopg2(3*kpg) * invja(2,i)
161          continue
!
            do 162 i = 1, 3
                dh(2,3*(kpg-1)+i) = coopg2(3*kpg-2) * invja(3,i) + coopg2(3*kpg) * invja(1,i)
162          continue
!
            do 163 i = 1, 3
                dh(3,3*(kpg-1)+i) = coopg2(3*kpg-2) * invja(2,i) + coopg2(3*kpg-1) * invja(1,i)
163          continue
!
            do 164 i = 1, 3
                dh(4,3*(kpg-1)+i) = coopg2(3*kpg-2) * coopg2(3*kpg-1) * invja(3,i) + coopg2(3*kpg&
                                    &-1) * coopg2(3*kpg) * invja(1,i) + coopg2(3*kpg-2) * coopg2(&
                                    &3*kpg) * invja(2,i)
164          continue
!
            call cast3d(proj, gam, dh, def, nno,&
                        kpg, nub, nu, d, calbn,&
                        bn, jac, matuu)
!
160      continue
!
    endif
!
! - CALCUL DES FORCES INTERNES ET DES CONTRAINTES DE CAUCHY
!
    if (option(1:9) .eq. 'FULL_MECA' .or. option(1:9) .eq. 'RAPH_MECA') then
!
!     INITIALISATION
        nbpg2 = 8
        do 12 ia = 1, 4
            pqx(ia) = 0.d0
            pqy(ia) = 0.d0
            pqz(ia) = 0.d0
12      continue
!
!     DEFORMATIONS GENERALISEES
        do 169 ia = 1, 4
            do 170 kl = 1, nno
                pqx(ia) = pqx(ia) + gam(ia,kl)*deplp(1,kl)
                pqy(ia) = pqy(ia) + gam(ia,kl)*deplp(2,kl)
                pqz(ia) = pqz(ia) + gam(ia,kl)*deplp(3,kl)
170          continue
169      continue
!
!      INCREMENT DES CONTRAINTES GENERALISEES
!
        call calcdq(proj, nub, nu, d, pqx,&
                    pqy, pqz, dq)
!
        do 180 i = 1, 72
            qmoins(i) = sigm(i+6,1)
            qplus(i) = qmoins(i) + dq(i)
180      continue
!
        call matini(3, nno, 0.d0, vectu)
        call matini(6, nbpg2, 0.d0, sigas)
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
165          continue
!
            do 166 i = 1, 3
                dh(2,3*(kpg-1)+i) = coopg2(3*kpg-2) * invja(3,i) + coopg2(3*kpg) * invja(1,i)
166          continue
!
            do 167 i = 1, 3
                dh(3,3*(kpg-1)+i) = coopg2(3*kpg-2) * invja(2,i) + coopg2(3*kpg-1) * invja(1,i)
167          continue
!
            do 168 i = 1, 3
                dh(4,3*(kpg-1)+i) = coopg2(3*kpg-2) * coopg2(3*kpg-1) * invja(3,i) + coopg2(3*kpg&
                                    &-1) * coopg2(3*kpg) * invja(1,i) + coopg2(3*kpg-2) * coopg2(&
                                    &3*kpg) * invja(2,i)
168          continue
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
35                  continue
34              continue
32          continue
!
!     CALCUL DES FORCES INTERNES
!
            do 250 i = 1, nno
                do 240 j = 1, 3
                    do 230 kl = 1, 3
                        vectu(j,i) = vectu(j,i) + (def(kl,j,i)+ bn(kl, j,i))* (sigas(kl,kpg)+sigm&
                                     &a(kl))*jac + (rac2* def(kl+3,j,i)+ bn(kl+3,j,i))* (sigas(kl&
                                     &+3,kpg) +sigma(kl+3)/rac2)*jac
230                  continue
240              continue
250          continue
!
290      continue
!
        do 300 kl = 1, 3
            sigp(kl,1) = sigma(kl)
            sigp(kl+3,1) = sigma(kl+3)/rac2
300      continue
!
        do 310 i = 1, 72
            sigp(i+6,1) = qplus(i)
310      continue
!
    endif
!
320  continue
! - SYNTHESE DES CODES RETOURS
    call codere(cod, nbpg1, codret)
!
end subroutine
