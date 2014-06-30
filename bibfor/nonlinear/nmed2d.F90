subroutine nmed2d(nno, npg, ipoids, ivf, idfde,&
                  geom, typmod, option, imate, compor,&
                  lgpg, crit, ideplm, iddepl, sigm,&
                  vim, dfdi, def, sigp, vip,&
                  matuu, ivectu, codret)
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
! aslint: disable=W1504
    implicit none
!
#include "jeveux.h"
#include "asterfort/codere.h"
#include "asterfort/nmedco.h"
#include "asterfort/nmedel.h"
#include "asterfort/nmedsq.h"
#include "asterfort/nmgeom.h"
#include "asterfort/r8inir.h"
#include "asterfort/utmess.h"
!
    integer :: nno, npg, imate, lgpg, codret, cod(9)
    integer :: ipoids, ivf, idfde, ivectu, ideplm, iddepl
    character(len=8) :: typmod(*)
    character(len=16) :: option, compor(*)
    real(kind=8) :: geom(2, nno), crit(3)
    real(kind=8) :: dfdi(nno, 2), def(4, nno, 2)
    real(kind=8) :: sigm(4, npg), sigp(4, npg)
    real(kind=8) :: vim(lgpg, npg), vip(lgpg, npg)
    real(kind=8) :: matuu(*)
!
!----------------------------------------------------------------------
!     BUT:  CALCUL DES OPTIONS RIGI_MECA_TANG, RAPH_MECA ET FULL_MECA
!           POUR L'ELEMENT A DISCONTINUITE INTERNE EN 2D
!----------------------------------------------------------------------
! IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  IPOIDS  : POINTEUR SUR LES POIDS DES POINTS DE GAUSS
! IN  IVF     : POINTEUR SUR LES VALEURS DES FONCTIONS DE FORME
! IN  IDFDE   : POINTEUR SUR DERIVEES DES FONCT DE FORME DE ELEM REFE
! IN  GEOM    : COORDONEES DES NOEUDS
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  OPTION  : OPTION DE CALCUL
! IN  IMATE   : MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT
! IN  LGPG    : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
!               CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  DEPLM   : DEPLACEMENT A L'INSTANT PRECEDENT
! IN  DDEPL   : INCREMENT DE DEPLACEMENT
! IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
! IN  VIM     : VARIABLES INTERNES A L'INSTANT PRECEDENT
! OUT DFDI    : DERIVEE DES FONCTIONS DE FORME  AU DERNIER PT DE GAUSS
! OUT DEF     : PRODUIT DER. FCT. FORME PAR F   AU DERNIER PT DE GAUSS
! OUT SIGP    : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
! OUT VIP     : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA)
! OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
! OUT VECTU   : FORCES NODALES (RAPH_MECA ET FULL_MECA)
!-----------------------------------------------------------------------
!
!
    logical(kind=1) :: grand, axi, resi, rigi, elas
!
    integer :: kpg, kk, kkd, n, i, m, j, j1, kl, k
!
    real(kind=8) :: dsidep(6, 6), f(3, 3), deps(6), r, sigma(6), sign(6)
    real(kind=8) :: poids, tmp, sig(6)
    real(kind=8) :: bum(6), bdu(6)
    real(kind=8) :: rac2
    real(kind=8) :: alpham(2), alphap(2)
    real(kind=8) :: s(2), q(2, 2), dsdu(2, 8), sg(2), qg(2, 2), dsdug(2, 8)
    real(kind=8) :: d(4, 2)
    real(kind=8) :: dda, xa, xb, ya, yb
    real(kind=8) :: rot(2, 2), cotmp, sitmp, co, si, drot, rtemp(4, 2)
    real(kind=8) :: dalfu(2, 8), dh(4, 8), dalfs(2, 2)
!
! - INITIALISATIONS
!------------------
!
    resi = option.eq.'RAPH_MECA' .or. option.eq.'FULL_MECA'
    rigi = option.eq.'FULL_MECA' .or. option.eq.'RIGI_MECA_TANG'
!
    if (.not. resi .and. .not. rigi) then
        call utmess('F', 'ALGORITH7_61', sk=option)
    endif
!
    call r8inir(2, 0.d0, s, 1)
    call r8inir(4, 0.d0, q, 1)
    call r8inir(16, 0.d0, dsdu, 1)
!
    rac2 = sqrt(2.d0)
    grand = .false.
    axi = typmod(1) .eq. 'AXIS'
!
!     INITIALISATION CODES RETOURS
    do 1955 kpg = 1, npg
        cod(kpg)=0
1955  end do
!
!
! MATRICE DE CHANGEMENT DE REPERE : DU GLOBAL AU LOCAL  :
!    ROT X = XLOC
! SOIT A ET B LES MILIEUX DES COTES [14] ET [23]
! t TANGENT AU COTE [AB]
!
    xa = ( geom(1,1) + geom(1,4) ) / 2
    ya = ( geom(2,1) + geom(2,4) ) / 2
!
    xb = ( geom(1,2) + geom(1,3) ) / 2
    yb = ( geom(2,2) + geom(2,3) ) / 2
!
    cotmp = (yb - ya)
    sitmp = (xa - xb)
!
    co = cotmp / sqrt(cotmp*cotmp + sitmp*sitmp)
    si = sitmp / sqrt(cotmp*cotmp + sitmp*sitmp)
!
    rot(1,1) = co
    rot(2,1) = -si
    rot(1,2) = si
    rot(2,2) = co
!
!
! - 1ERE BOUCLE SUR LES POINTS DE GAUSS
! -------------------------------------
!   (CALCUL DE S ET Q NECESSAIRE POUR LE CALCUL DU SAUT ALPHA)
!
    do 800 kpg = 1, npg
!
!       CALCUL DE DFDI,F,EPS (BUM),DEPS (BDU),R(EN AXI) ET POIDS
!
        call r8inir(6, 0.d0, bum, 1)
        call r8inir(6, 0.d0, bdu, 1)
!
        call nmgeom(2, nno, axi, grand, geom,&
                    kpg, ipoids, ivf, idfde, zr( ideplm),&
                    .true._1, poids, dfdi, f, bum,&
                    r)
        call nmgeom(2, nno, axi, grand, geom,&
                    kpg, ipoids, ivf, idfde, zr( iddepl),&
                    .true._1, poids, dfdi, f, bdu,&
                    r)
!
!
!       CALCUL DE D (LES AUTRES TERMES SONT NULS):
!
        call r8inir(8, 0.d0, d, 1)
!
        d(1,1) = - (dfdi(1,1) + dfdi(2,1))
        d(4,1) = - rac2*(dfdi(1,2) + dfdi(2,2))/2
        d(2,2) = - (dfdi(1,2) + dfdi(2,2))
        d(4,2) = - rac2*(dfdi(1,1) + dfdi(2,1))/2
!
!       CHANGEMENT DE REPERE DANS D : ON REMPLACE D PAR DRt :
!
        call r8inir(8, 0.d0, rtemp, 1)
!
        do 32 i = 1, 4
            do 33 j = 1, 2
                drot = 0.d0
                do 34 k = 1, 2
                    drot = drot + d(i,k)*rot(j,k)
34              continue
                rtemp(i,j) = drot
33          continue
32      continue
!
        do 38 i = 1, 4
            do 39 j = 1, 2
                d(i,j) = rtemp(i,j)
39          continue
38      continue
!
!       CALCUL DES PRODUITS SYMETR. DE F PAR N,
!
        call r8inir(32, 0.d0, def, 1)
        do 40 n = 1, nno
            do 30 i = 1, 2
                def(1,n,i) = f(i,1)*dfdi(n,1)
                def(2,n,i) = f(i,2)*dfdi(n,2)
                def(3,n,i) = 0.d0
                def(4,n,i) = (f(i,1)*dfdi(n,2) + f(i,2)*dfdi(n,1))/ rac2
30          continue
40      continue
!
!       TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
!
        if (axi) then
            do 50 n = 1, nno
                def(3,n,1) = f(3,3)*zr(ivf+n+(kpg-1)*nno-1)/r
50          continue
        endif
!
        do 60 i = 1, 3
            sign(i) = sigm(i,kpg)
60      continue
        sign(4) = sigm(4,kpg)*rac2
!
!       CALCUL DE S ET Q AU POINT DE GAUSS COURANT I.E. SG ET QG :
!
        call nmedsq(sg, qg, dsdug, d, npg,&
                    typmod, imate, bum, bdu, sign,&
                    vim, option, geom, nno, lgpg,&
                    kpg, def)
!
!       CALCUL DES S ET Q POUR L'ELEMENT :
!
        do 64 i = 1, 2
            s(i) = s(i) + poids*sg(i)
            do 65 j = 1, 2
                q(i,j) = q(i,j) + poids*qg(i,j)
65          continue
            do 66 j = 1, 8
                dsdu(i,j) = dsdu(i,j) + poids*dsdug(i,j)
66          continue
64      continue
!
800  end do
!
!
! - APPEL DU COMPORTEMENT :
!--------------------------
!
    call nmedco(compor, option, imate, npg, lgpg,&
                s, q, vim, vip, alphap,&
                dalfs)
!
!
! - 2ND BOUCLE SUR LES POINTS DE GAUSS
! -------------------------------------------------
!   (CALCUL DE LA CONTRAINTE , FORCE INT ET MATRICE TANGENTE)
!
    do 801 kpg = 1, npg
!
!       CALCUL DE DFDI,F,BUM, BDU ,R(EN AXI) ET POIDS
!
        call r8inir(6, 0.d0, bum, 1)
        call r8inir(6, 0.d0, bdu, 1)
!
        call nmgeom(2, nno, axi, grand, geom,&
                    kpg, ipoids, ivf, idfde, zr( ideplm),&
                    .true._1, poids, dfdi, f, bum,&
                    r)
!
        call nmgeom(2, nno, axi, grand, geom,&
                    kpg, ipoids, ivf, idfde, zr( iddepl),&
                    .true._1, poids, dfdi, f, bdu,&
                    r)
!
!
!       CALCUL DE D (LES AUTRES TERMES SONT NULS):
!
        call r8inir(8, 0.d0, d, 1)
!
        d(1,1) = - (dfdi(1,1) + dfdi(2,1))
        d(4,1) = - rac2*(dfdi(1,2) + dfdi(2,2))/2
        d(2,2) = - (dfdi(1,2) + dfdi(2,2))
        d(4,2) = - rac2*(dfdi(1,1) + dfdi(2,1))/2
!
!       CHANGEMENT DE REPERE DANS D : ON REMPLACE D PAR DRt :
!
        call r8inir(8, 0.d0, rtemp, 1)
!
        do 35 i = 1, 4
            do 36 j = 1, 2
                drot = 0.d0
                do 37 k = 1, 2
                    drot = drot + d(i,k)*rot(j,k)
37              continue
                rtemp(i,j) = drot
36          continue
35      continue
!
        do 52 i = 1, 4
            do 53 j = 1, 2
                d(i,j) = rtemp(i,j)
53          continue
52      continue
!
!       CALCUL DES PRODUITS SYMETR. DE F PAR N,
!
        call r8inir(32, 0.d0, def, 1)
        do 41 n = 1, nno
            do 31 i = 1, 2
                def(1,n,i) = f(i,1)*dfdi(n,1)
                def(2,n,i) = f(i,2)*dfdi(n,2)
                def(3,n,i) = 0.d0
                def(4,n,i) = (f(i,1)*dfdi(n,2) + f(i,2)*dfdi(n,1))/ rac2
31          continue
41      continue
!
!       TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
!
        if (axi) then
            do 51 n = 1, nno
                def(3,n,1) = f(3,3)*zr(ivf+n+(kpg-1)*nno-1)/r
51          continue
        endif
!
        do 61 i = 1, 3
            sign(i) = sigm(i,kpg)
61      continue
        sign(4) = sigm(4,kpg)*rac2
!
!       LA VARIATION DE DEF DEPS : DEVIENT LA SOMME DES VARIATION
!       DE DEF LIEE AU DEPL : 'BDU' PLUS CELLE LIEE AU SAUT : 'DDA'
!
        alpham(1) = vim(1,kpg)
        alpham(2) = vim(2,kpg)
        do 80 i = 1, 4
            dda = 0.d0
            do 70 j = 1, 2
                dda = dda + d(i,j)*(alphap(j)-alpham(j))
70          continue
            deps(i) = bdu(i) + dda
80      continue
!
! CALCUL DE LA CONTRAINTE
!------------------------
!
!       APPEL DE LA LDC ELASTIQUE : SIGMA=A.EPS AVEC EPS=BU-DA
!       ON PASSE EN ARG LA VARIATION DE DEF 'DEPS' ET LA CONTRAINTE -
!       'SIGN' ET ON SORT LA CONTAINTE + 'SIGMA'
        call nmedel(2, typmod, imate, deps, sign,&
                    option, sigma, dsidep)
!
! CALCUL DES EFFORTS INTERIEURS ET MATRICE TANGENTE :
! ---------------------------------------------------
!
        if (rigi) then
!
! CALCUL DE DH : TERME MANQUANT DANS LA MATRICE TANGENTE :
!
            if (option .eq. 'RIGI_MECA_TANG') then
                elas=(nint(vim(4,kpg)).eq.0)
            else
                elas=(nint(vip(4,kpg)).eq.0)
            endif
!
            if ((elas) .and. (vim(3,kpg).eq.0.d0)) then
!
                call r8inir(32, 0.d0, dh, 1)
!
            else
!
! CALCUL DE LA DERIVE DE ALPHA PAR RAPPORT A U : 'DALFU' EN UTILISANT LA
! DERIVEE ALPHA PAR RAPPORT A S : 'DALFS' (CALCULE DANS LE COMPORTEMENT
! CF NMEDCO.F) ET DE LA DERIVEE DE S PAR RAPPORT A U : 'DSDU'.
!
                call r8inir(16, 0.d0, dalfu, 1)
                do 73 i = 1, 2
                    do 74 j = 1, 8
                        do 75 k = 1, 2
                            dalfu(i,j) = dalfu(i,j) + dalfs(i,k)*dsdu( k,j)
75                      continue
74                  continue
73              continue
!
!
! ON MET LE PRODUIT D.DALFU DANS DH :
! DH  =  D  DALFU
! 4x8 = 4x2  2x8
!
                call r8inir(32, 0.d0, dh, 1)
                do 76 i = 1, 4
                    do 77 j = 1, 8
                        do 78 k = 1, 2
                            dh(i,j) = dh(i,j) + d(i,k)*dalfu(k,j)
78                      continue
77                  continue
76              continue
!
            endif
!
! - CALCUL DE LA MATRICE DE RIGIDITE
!
            do 160 n = 1, nno
                do 150 i = 1, 2
                    do 151,kl=1,4
                    sig(kl)=0.d0
                    sig(kl)=sig(kl) + def(1,n,i)*dsidep(1,kl)
                    sig(kl)=sig(kl) + def(2,n,i)*dsidep(2,kl)
                    sig(kl)=sig(kl) + def(3,n,i)*dsidep(3,kl)
                    sig(kl)=sig(kl) + def(4,n,i)*dsidep(4,kl)
151                  continue
                    do 140 j = 1, 2
                        do 130 m = 1, n
                            if (m .eq. n) then
                                j1 = i
                            else
                                j1 = 2
                            endif
!
!                 RIGIDITE ELASTIQUE + TERME LIE AU SAUT :
!                 TMP = Bt E (B + DH)
!
                            tmp=0.d0
                            tmp=tmp + sig(1)*( def(1,m,j) + dh(1, 2*(&
                            m-1)+j) )
                            tmp=tmp + sig(2)*( def(2,m,j) + dh(2, 2*(&
                            m-1)+j) )
                            tmp=tmp + sig(3)*( def(3,m,j) + dh(3, 2*(&
                            m-1)+j) )
                            tmp=tmp + sig(4)*( def(4,m,j) + dh(4, 2*(&
                            m-1)+j) )
!
!                 STOCKAGE EN TENANT COMPTE DE LA SYMETRIE
                            if (j .le. j1) then
                                kkd = (2*(n-1)+i-1) * (2*(n-1)+i) /2
                                kk = kkd + 2*(m-1)+j
                                matuu(kk) = matuu(kk) + tmp*poids
                            endif
!
130                      continue
140                  continue
150              continue
160          continue
!
        endif
!
! - CALCUL DE LA FORCE INTERIEURE ET DES CONTRAINTES DE CAUCHY
!
        if (resi) then
!
            do 230 n = 1, nno
                do 220 i = 1, 2
                    do 210 kl = 1, 4
!                VECTU(I,N) = VECTU(I,N) + DEF(KL,N,I)*SIGMA(KL)*POIDS
                        zr(ivectu-1+2*(n-1)+i)= zr(ivectu-1+2*(n-1)+i)&
                        +def(kl,n,i)*sigma(kl)*poids
210                  continue
220              continue
230          continue
!
            do 310 kl = 1, 3
                sigp(kl,kpg) = sigma(kl)
310          continue
            sigp(4,kpg) = sigma(4)/rac2
!
        endif
!
801  end do
!
!
!
! - SYNTHESE DES CODES RETOURS
    call codere(cod, npg, codret)
!
end subroutine
