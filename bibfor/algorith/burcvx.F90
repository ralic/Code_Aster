subroutine burcvx(mod, nmat, materd, materf, timed,&
                  timef, nvi, vind, nr, sigd,&
                  deps, yd, yf, toler, seuil)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: alexandre.foucault at edf.fr
!=====================================================================
!  BETON_BURGER_FP : CALCUL SOLUTION ESSAI ELASTIQUE
!  LA SOLUTION D'ESSAI EST ETABLIE SUIVANT UNE APPROCHE LINEARISEE
!  D'ORDRE 1 AUTOUR DE L'ETAT MATERIAU A L'INSTANT T
!  IN  NMAT   : DIMENSION MATER
!      MATERD : COEFFICIENTS MATERIAU A T
!      MATERF : COEFFICIENTS MATERIAU A T+DT
!      MOD    : TYPE DE MODELISATION
!      TIMED  : INSTANT T
!      TIMEF  : INSTANT T+DT
!      NVI    : DIMENSION VECTEUR VARIABLES INTERNES
!      VIND   : VECTEUR VARIABLES INTERNES A T
!      NR     : DIMENSION VECTEUR INCONNUES (YF)
!      SIGD   : VECTEUR CONTRAINTES A T
!      DEPS   : INCREMENT DE DEFORMATIONS TOTALES
!      TOLER  : TOLERANCE A CONVERGENCE
!  OUT SEUIL  : SEUIL > 0 -> PASSAGE DANS LCPLAS
!               SEUIL < 0 -> PASSAGE DANS LCELPL
!      YD     : VECTEUR SOLUTION A T
!      YF     : VECTEUR SOLUTION A T+DT
!=====================================================================
    implicit none
!     ----------------------------------------------------------------
#include "asterc/r8prem.h"
#include "asterfort/burafd.h"
#include "asterfort/burafi.h"
#include "asterfort/burafr.h"
#include "asterfort/burdfi.h"
#include "asterfort/burres.h"
#include "asterfort/bursif.h"
#include "asterfort/lceqvn.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lcsovn.h"
#include "asterfort/vecini.h"
    common /tdim/   ndt ,ndi
!     ----------------------------------------------------------------
    integer :: nmat, nvi, ndt, ndi, nr, i, j
    real(kind=8) :: materf(nmat, 2), materd(nmat, 2), toler
    real(kind=8) :: timed, timef, vind(nvi), deps(6), sigd(6)
    real(kind=8) :: yd(13), dy(13), yf(13), r(13)
    real(kind=8) :: afd(6), bfd(6, 6), cfd(6, 6)
    real(kind=8) :: afr(6), bfr(6, 6), cfr(6, 6)
    real(kind=8) :: afi(6), bfi(6, 6), cfi(6, 6)
    real(kind=8) :: an(6), bn(6, 6), cn(6, 6)
    real(kind=8) :: dsig(6), maxi, mini
    real(kind=8) :: epsfi(6), eisp, er, seuil, nfif, epsfif(6)
    character(len=8) :: mod
!
! === =================================================================
! --- RECUPERATION DES TERMES AN(6), BN(6,6), CN(6,6)
! === =================================================================
! === =================================================================
! --- RECUPERATION DES TERMES ANFD, BNFD, CNFD (FD: FLUAGE DESSICATION)
! === =================================================================
    call burafd(materd, materf, nmat, afd, bfd,&
                cfd)
! === =================================================================
! --- RECUPERATION DES TERMES ANR(6), BNR(6,6), CNR(6,6)
! === =================================================================
    call burafr(vind, nvi, materd, materf, nmat,&
                timed, timef, afr, bfr, cfr)
! === =================================================================
! --- RECUPERATION DES TERMES ANI(6), BNI(6,6), CNI(6,6) LINEARISES
! === =================================================================
    call burafi(vind, nvi, materd, materf, nmat,&
                timed, timef, afi, bfi, cfi)
! === =================================================================
! --- ASSEMBLAGE DES TERMES AN(6), BN(6,6), CN(6,6) LINEARISES
! === =================================================================
    do 1 i = 1, ndt
        an(i) = afr(i)+afi(i)+afd(i)
        do 2 j = 1, ndt
            bn(i,j) = bfr(i,j)+bfi(i,j)+bfd(i,j)
            cn(i,j) = cfr(i,j)+cfi(i,j)+cfd(i,j)
 2      end do
 1  end do
! === =================================================================
! --- INITIALISATION DE YD ET DY A ZERO
! === =================================================================
    call lceqvn(ndt, sigd, yd)
! === ============================================================
!     CONSTRUCTION DES DEFORMATIONS IRREVERSIBLES DE FLUAGE PROPRE
! === ============================================================
! --- RECUPERATION PARTIE SPHERIQUE
! === ============================================================
    eisp = vind(2)
! === ============================================================
! --- RECUPERATION PARTIE DEVIATOIRE
! === ============================================================
    epsfi(1) = vind(4)
    epsfi(2) = vind(6)
    epsfi(3) = vind(8)
    epsfi(4) = vind(13)
    epsfi(5) = vind(15)
    epsfi(6) = vind(17)
! === ============================================================
! --- ASSEMBLAGE PARTIE DEVIATOIRE ET SPHERIQUE
! === ============================================================
    do 200 i = 1, ndi
        epsfi(i)=epsfi(i)+eisp
200  end do
! === ============================================================
! --- AFFECTATION DES VALEURS AU VECTEUR YD(NDT+I)
! === ============================================================
    do 210 i = 1, ndt
        yd(ndt+i) = epsfi(i)
210  end do
!
! --- DY EGAL 0.D0
    call vecini(nr, 0.d0, dy)
! === =================================================================
! --- CALCUL DE SIGF - PUIS DSIG = SIGF - SIGD
! === =================================================================
    call bursif(materd, materf, nmat, an, bn,&
                cn, deps, nr, yd, dsig)
    do 3 i = 1, ndt
        dy(i) = dsig(i)
 3  end do
! === =================================================================
! --- CALCUL ESSAI : EPSFI(T+DT) - PUIS DEPSFI = EPSFI(T+DT) - EPSFI(T)
! === =================================================================
    call burdfi(bfi, cfi, nr, yd, dy)
!
! === =================================================================
! --- TRAITEMENT DU BRUIT NUMERIQUE PAR R8PREM
! === =================================================================
    maxi = 0.d0
    do 11 i = 1, nr
        if(abs(dy(i)).gt.maxi)maxi = abs(dy(i))
11  end do
    mini = r8prem() * maxi
    do 12 i = 1, nr
        if(abs(dy(i)).lt.mini)dy(i) = 0.d0
12  end do
!
! === =================================================================
! --- CALCUL RESIDU POUR LIMITER DES ITERATIONS DE NEWTON INUTILES
! === =================================================================
    call lcsovn(nr, yd, dy, yf)
    call burres(mod, nmat, materd, materf, timed,&
                timef, nvi, vind, yd, yf,&
                deps, dy, nr, r)
! === =================================================================
! --- CALCUL NORME RESIDU
! === =================================================================
    er = 0.0d0
    do 13 i = ndt+1, nr
        er = er + r(i)*r(i)
13  end do
    er = sqrt(er)
!
! === =================================================================
! --- ASSIGNATION DE LA VALEUR AU SEUIL
! === =================================================================
    seuil = 1.d0
    if(er.lt.toler)seuil = -1.d0
!
! === =================================================================
! --- CONTROLE DU TERME PORTANT SUR LES DEFORMATIONS IRREVERSIBLES
! --- SI LA NORME OBTENUE EST INFERIEURE A VIND(21) ALORS LA SOLUTION
! --- LINEARISEE EST EXACTE
! === =================================================================
    do 14 i = 1, ndt
        epsfif(i) = yf(ndt+i)
14  end do
!
    call lcprsc(epsfif, epsfif, nfif)
    nfif = sqrt(nfif)
!
    if (nfif .lt. vind(21)) then
        seuil = -1.d0
    endif
!
end subroutine
