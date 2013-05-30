subroutine bcoudc(igau, icou, isect, h, a,&
                  m, omega, xpg, nno, ncou,&
                  nsect, ff, df1, df2, rayon,&
                  theta, mmt, b)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    include 'jeveux.h'
    include 'asterc/r8pi.h'
    real(kind=8) :: h, a, b(4, *), ff(*), df1(*), df2(*), rayon, theta, omega
    integer :: nno, ncou, nsect, m, igau, icou, isect, mmt
!
! ......................................................................
!
!   BUT      : CALCUL DE LA MATRICE DE DEFORMATION B POUR LES ELEMENTS
!              TUYAU
!
!   ENTREES : <----
!           IGAU,ICOU,ISECT : LES INDICES DES POINTS D'INTEGRATION
!                RESPECTIVEMENT SUR LA LONGUEUR, DANS L'EPAISSEUR ET
!                SUR LA CIRCONFERENCE
!           NNO : NBRE DE NOEUDS
!           NCOU,NSECT : NBRES DE COUCHES, NBRES DE SECTEURS
!           FF : VALEURS DES FONCTIONS DE FORMES
!           DF1: VALEURS DES DERIVEES 1ERES DES FONCTIONS DE FORMES
!           DF2: VALEURS DES DERIVEES 2EMES DES FONCTIONS DE FORMES
!           M : NBRES DE MODES DE FOURIER
!           H : SON EPAISSEUR
!           A : SON RAYON MOYEN
!           XPG : COORDONNES DES POINTS DE GAUSS
!   SORTIES : ---->
!           B : LA MATRICE DE DEFORMATIONS
! ......................................................................
!
    real(kind=8) :: pi, deuxpi, cosfi, sinfi
    real(kind=8) :: zeta, r, hk, dhk, d2hk, fi, tk(4)
    real(kind=8) :: te, coste, sinte, cosmte, sinmte
    integer :: k, icolon, n, ibloc
    real(kind=8) :: xpg(4), ck, sk, dena, denr, dent
!
!
!
    pi = r8pi()
    deuxpi = 2.d0*pi
!
    zeta = (icou-1)*h/ (2.d0*ncou) - h/2.d0
    fi = (isect-1)*deuxpi/ (2.d0*nsect)
    cosfi = cos(fi)
    sinfi = sin(fi)
    te = fi - omega
    coste = cos(te)
    sinte = sin(te)
    if (mmt .eq. 0) then
        r = a
    else
        r = a + zeta
    endif
    denr = rayon + r*sinfi
    dena = rayon + a*sinfi
    dent = (1.d0/denr+a/ (r*dena))
    if (nno .eq. 3) then
        tk(1) = 0.d0
        tk(2) = theta
        tk(3) = theta/2.d0
    else if (nno.eq.4) then
        tk(1) = 0.d0
        tk(2) = theta
        tk(3) = theta/3.d0
        tk(4) = 2.d0*theta/3.d0
    endif
!
!  REMLISSAGE DE LA MATRICE
!
    do 30 k = 1, nno
!
        hk = ff(nno* (igau-1)+k)
        dhk = df1(nno* (igau-1)+k)* (2.d0/theta)
        d2hk = df2(nno* (igau-1)+k)* (2.d0/theta)* (2.d0/theta)
        ck = cos((1.d0+xpg(igau))*theta/2.d0-tk(k))
        sk = sin((1.d0+xpg(igau))*theta/2.d0-tk(k))
!
!
! LE 2/L EST DU AU PASSAGE DE L'ELEMENT DE REFERENCE A L'ELEMENT
! REEL
!
        ibloc = (9+6* (m-1))* (k-1)
!
!  PARTIE POUTRE
!
!   1 ERE LIGNE
!
        b(1,ibloc+1) = dhk*ck/denr
        b(1,ibloc+2) = dhk*sk/denr
        b(1,ibloc+3) = 0.d0
        b(1,ibloc+4) = r*cosfi*dhk*sk/denr
        b(1,ibloc+5) = -r*cosfi*dhk*ck/denr
        b(1,ibloc+6) = r*sinfi*dhk/denr
!
!   2 EME LIGNE
!
        b(2,ibloc+1) = 0.d0
        b(2,ibloc+2) = 0.d0
        b(2,ibloc+3) = 0.d0
        b(2,ibloc+4) = 0.d0
        b(2,ibloc+5) = 0.d0
        b(2,ibloc+6) = 0.d0
!
!   3 EME LIGNE
!
        b(3,ibloc+1) = cosfi*dhk*sk/denr
        b(3,ibloc+2) = -cosfi*dhk*ck/denr
        b(3,ibloc+3) = sinfi*dhk/denr
        b(3,ibloc+4) = - (hk*sk*rayon*sinfi+r*dhk*ck)/denr
        b(3,ibloc+5) = (hk*ck*rayon*sinfi-r*dhk*sk)/denr
        b(3,ibloc+6) = rayon*cosfi*hk/denr
!
!   4 EME LIGNE
!
        b(4,ibloc+1) = sinfi*dhk*sk/denr
        b(4,ibloc+2) = -sinfi*dhk*ck/denr
        b(4,ibloc+3) = -cosfi*dhk/denr
        b(4,ibloc+4) = rayon*cosfi*hk*sk/denr
        b(4,ibloc+5) = -rayon*cosfi*hk*ck/denr
        b(4,ibloc+6) = rayon*sinfi*hk/denr
!
!  FIN PARTIE POUTRE ET DEBUT PARTIE SUPPLIMENTAIRE
!
!      PARTIE IN-PLANE
!
!
        do 10 n = 2, m
            icolon = ibloc + 6 + 3* (n-2)
!C         COSMTE = COS(N*FI-OMEGA)
!C         SINMTE = SIN(N*FI-OMEGA)
            cosmte = cos(n* (fi-omega))
            sinmte = sin(n* (fi-omega))
!
            b(1,icolon+1) = dhk*cosmte/dena
            b(1,icolon+2) = hk*sinmte*cosfi* (1.d0+zeta/a)/denr
            b(1,icolon+3) = -zeta*d2hk*cosmte/ (denr*dena) + hk* cosmte*sinfi/denr + zeta*n*hk*si&
                            &nmte*cosfi/ (a*denr)
!
            b(2,icolon+1) = 0.d0
            b(2,icolon+2) = (n/r)*hk*cosmte* (1.d0+zeta/a)
            b(2,icolon+3) = (1.d0/r)*hk*cosmte* (1.d0+zeta*n*n/a)
!
!
            b(3,icolon+1) = - (n/r)*hk*sinmte - hk*cosmte*cosfi/denr - zeta*hk*cosfi*sinfi*cosmte&
                            &/dena*dent + zeta*hk* (cosmte* cosfi-n*sinmte*sinfi)/ (r*dena)
            b(3,icolon+2) = dhk*sinmte* (1+zeta/a)/denr
            b(3,icolon+3) = zeta*n*dhk*sinmte* (1.d0/ (a*denr)+1.d0/ ( r*dena)&
                            ) + zeta*dhk*cosfi*cosmte/dena*dent
!
!
            b(4,icolon+1) = 0.d0
            b(4,icolon+2) = 0.d0
            b(4,icolon+3) = 0.d0
!
!
10      continue
!
!  FIN PARTIE IN-PLANE DEBUT PARTIE OUT-OF-PLANE
!
        do 20 n = 2, m
            icolon = ibloc + 6 + 3* (m-1) + 3* (n-2)
!C          COSMTE = COS(N*FI-OMEGA)
!C          SINMTE = SIN(N*FI-OMEGA)
            cosmte = cos(n* (fi-omega))
            sinmte = sin(n* (fi-omega))
!
            b(1,icolon+1) = dhk*sinmte/dena
            b(1,icolon+2) = hk*cosmte*cosfi* (1.d0+zeta/a)/denr
            b(1,icolon+3) = -zeta*d2hk*sinmte/ (denr*dena) + hk* sinmte*sinfi/denr - zeta*n*hk*co&
                            &smte*cosfi/ (a*denr)
!
            b(2,icolon+1) = 0.d0
            b(2,icolon+2) = - (n/r)*hk*sinmte* (1.d0+zeta/a)
            b(2,icolon+3) = (1.d0/r)*hk*sinmte* (1.d0+zeta*n*n/a)
!
!
            b(3,icolon+1) = (n/r)*hk*cosmte - hk*sinmte*cosfi/denr - zeta*hk*cosfi*sinfi*sinmte/d&
                            &ena*dent + zeta*hk* (sinmte* cosfi+n*cosmte*sinfi)/ (r*dena)
            b(3,icolon+2) = dhk*cosmte* (1.d0+zeta/a)/denr
            b(3,icolon+3) = -zeta*n*dhk*cosmte* (1.d0/ (a*denr)+1.d0/ (r*dena)&
                            ) + zeta*dhk*cosfi*sinmte/dena*dent
!
!
            b(4,icolon+1) = 0.d0
            b(4,icolon+2) = 0.d0
            b(4,icolon+3) = 0.d0
!
!
20      continue
!
!  FIN OUT-OF-PLANE DEBUT PARTIE GONFLEMENT
!
        icolon = ibloc + 6* (m-1) + 6
        b(1,icolon+1) = hk*sinfi/denr - zeta*d2hk/ (dena*denr)
        b(2,icolon+1) = hk/r
        b(3,icolon+1) = zeta*dhk*cosfi/dena*dent
        b(4,icolon+1) = 0.d0
!
!  FIN PARTIE GONFLEMENT
!
!  1ERS MODES EN W
!
        coste = cos(te)
        sinte = sin(te)
        b(1,icolon+2) = -zeta*d2hk*coste/ (denr*dena) + 2.d0*hk*cosfi* sinte/denr*zeta/a + hk* (c&
                        &oste*sinfi+cosfi*sinte)/denr
        b(2,icolon+2) = (2.d0/r)*hk*coste* (1.d0+zeta/a)
        b(3,icolon+2) = dhk*sinte* ((1.d0+2.d0*zeta/a)/denr+ zeta/ (r* dena)&
                        ) + zeta*dhk*coste*cosfi/dena*dent
        b(4,icolon+2) = 0.d0
!
        b(1,icolon+3) = -zeta*d2hk*sinte/ (dena*denr) + hk* (sinfi* sinte-cosfi*coste)/denr - 2.d&
                        &0*zeta*hk*cosfi*coste/ (a*denr)
        b(2,icolon+3) = (2.d0/r)*hk*sinte* (1.d0+zeta/a)
        b(3,icolon+3) = -dhk*coste* ((1.d0+2.d0*zeta/a)/denr+ zeta/ ( r*dena)&
                        ) + zeta*dhk*sinte*cosfi/dena*dent
        b(4,icolon+3) = 0.d0
!
!
!  FIN PARTIE GONFLEMENT
!
! FIN DES 1ERS MODES EN W
!
30  end do
!
!
! FIN REMPLISSAGE DE LA MATRICE
!
!
end subroutine
