subroutine betini(materf, nmat, sig, sigeq, sigh,&
                  coefa, coefb, coefar, coefbr, coneco,&
                  conetr)
    implicit none
!       ================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!       ----------------------------------------------------------------
!       BETON_DOUBLE_DP: CONVEXE ELASTO PLASTIQUE POUR (MATER,SIG,P1,P2)
!                   AVEC UN SEUIL EN COMPRESSION ET UN SEUIL EN TRACTION
!       CALCUL DES TERMES CONSTANTS DU SYSTEME LINEAIRE A RESOUDRE
!       ET DES TERMES CORRESPONDANT AU COMPLEMENTAIRES DES CRITERES
!       MODIFIES
!       IN  MATERF :  COEFFICIENTS MATERIAU A T+DT
!           NMAT   :  DIMENSION MATER
!           SIG    :  CONTRAINTE A T+DT (PREDICTION ELASTIQUE)
!       OUT SIGEQ  :  CONTRAINTE EQUIVALENTE
!           SIGH   :  CONTRAINTE HYDROSTATIQUE
!           COEFA  :  TERMES CONSTANTS DU SYSTEME LINEAIRE A RESOUDRE
!           COEFB  :  TERMES MULTIPLICATEURS DES INCREMENTS DE
!                     MULTIPLICATEURS PLASTIQUES DU SYSTEME LINEAIRE
!           COEFAR :  TERMES CONSTANTS DU SYSTEME LINEAIRE
!                     CORRESPONDANT AU COMPLEMENTAIRES DES CRITERES
!           COEFBR :  TERMES MULTIPLICATEURS DES INCREMENTS DE
!                     MULTIPLICATEURS PLASTIQUES DU SYSTEME LINEAIRE
!                     CORRESPONDANT AU COMPLEMENTAIRES DES CRITERES
!           CONECO :  LIMITE SUPERIEURE DE FT POUR LA PROJECTION AU
!                     SOMMET DU CONE DE COMPRESSION
!           CONETR :  LIMITE SUPERIEURE DE FT POUR LA PROJECTION AU
!                     SOMMET DU CONE DE TRACTION
!       ----------------------------------------------------------------
    include 'asterfort/lcdevi.h'
    include 'asterfort/lchydr.h'
    include 'asterfort/lcprsc.h'
    integer :: nmat
    real(kind=8) :: materf(nmat, 2)
    real(kind=8) :: coefa(2, 2), coefb(2), coefar(2, 2), coefbr(2)
    real(kind=8) :: mun, un, d23, d13, rac2, deux, trois, zero
    parameter       ( mun  = -1.d0  )
    parameter       ( zero =  0.d0   )
    parameter       ( un   =  1.d0   )
    parameter       ( deux =  2.d0   )
    parameter       ( trois = 3.d0   )
    parameter       ( d23  =  .66666666666666d0 )
    parameter       ( d13  =  .33333333333333d0 )
    real(kind=8) :: dev(6), sig(6)
    real(kind=8) :: sigeq, sigh, p, conetr, coneco
    real(kind=8) :: k, lambda, mu, e, nu, beta
    real(kind=8) :: a, b, c, d
!       ----------------------------------------------------------------
    integer :: ndt, ndi
    common /tdim/   ndt , ndi
!       ----------------------------------------------------------------
!
! --- INITIALISATION
!
    rac2 = sqrt (deux)
    e = materf(1,1)
    nu = materf(2,1)
    beta = materf(3,2)
!
    a = rac2 * (beta - un) / (deux * beta - un)
    b = rac2 / trois * beta / (deux * beta - un)
    c = rac2
    d = deux * rac2 / trois
!
! --- CONTRAINTE EQUIVALENTE
!
    call lcdevi(sig, dev)
    call lcprsc(dev, dev, p)
    sigeq = sqrt (1.5d0 * p)
!
! --- CONTRAINTE HYDROSTATIQUE
!
    call lchydr(sig, sigh)
!
! --- COEFFICIENTS DE LAME
!
    lambda = (nu * e)/((un + nu)*(un - deux * nu))
    mu = e /(deux*(un + nu))
!
! --- MODULE DE COMPRESSION HYDROSTATIQUE
!
    k = lambda + d23 * mu
!
! --- COEFFICIENTS CONSTANTS ET COEFFICIENTS MULTIPLICATEUR DES
!     MULTIPLICATEURS PLASTIQUES DANS LE SYSTEME NON LINEAIRE
!
    coefa(1,1) = zero
    coefa(1,2) = zero
    coefa(2,1) = zero
    coefa(2,2) = zero
    coefar(1,1) = zero
    coefar(1,2) = zero
    coefar(2,1) = zero
    coefar(2,2) = zero
    coefb(1) = zero
    coefb(2) = zero
    coefbr(1) = zero
    coefbr(2) = zero
!
! --- CRITERE EN COMPRESSION ACTIF
!
    coefa(1,1) = mun * (d23 * mu + k * a * a) / (b * b)
    coefa(1,2) = mun * (d23 * mu + k * a * c) / (b * d)
!
    coefb(1) = (d13 * rac2 * sigeq + a * sigh) / b
!
    coefar(1,1) = mun * k * a * a / (b * b)
    coefar(1,2) = mun * k * a * c / (b * d)
!
    coefbr(1) = a * sigh / b
!
! --- CRITERE EN TRACTION ACTIF
!
    coefa(2,1) = mun * (d23 * mu + k * a * c) / (b * d)
    coefa(2,2) = mun * (d23 * mu + k * c * c) / (d * d)
!
    coefb(2) = (d13 * rac2 * sigeq + c * sigh) / d
!
    coefar(2,1) = mun * k * a * c / (b * d)
    coefar(2,2) = mun * k * c * c / (d * d)
!
    coefbr(2) = c * sigh / d
!
! --- LIMITE SUPERIEURE DE FT POUR LA PROJECTION AU SOMMET DU CONE
! --- DE TRACTION ET DU CONE DE COMPRESSION
!
    coneco = sigh * a / b - sigeq * a * a * k /(rac2 * mu * b)
    conetr = sigh * c / d - sigeq * c * c * k /(rac2 * mu * d)
!
end subroutine
