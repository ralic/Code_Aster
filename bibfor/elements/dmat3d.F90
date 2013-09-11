subroutine dmat3d(fami, mater, instan, poum, igau,&
                  isgau, repere, xyzgau, d)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!      DMAT3D --   CALCUL DE LA MATRICE DE HOOKE POUR LES ELEMENTS
!                  MASSIFS EN 3D OU EN SERIE DE FOURIER
!                  POUR DES MATERIAUX ISOTROPE, ORTHOTROPE
!                  ET ISOTROPE TRANSVERSE
!
!   ARGUMENT        E/S  TYPE         ROLE
!    FAMI           IN     K4       FAMILLE DU POINT DE GAUSS
!    MATER          IN     I        MATERIAU
!    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
!    POUM           IN     K1       + OU -
!    IGAU           IN     I        POINT DE GAUSS
!    ISGAU          IN     I        SOUS-POINT DE GAUSS
!    REPERE(7)      IN     R        VALEURS DEFINISSANT LE REPERE
!                                   D'ORTHOTROPIE
!    XYZGAU(3)      IN     R        COORDONNEES DU POINT D'INTEGRATION
!    D(6,6)         OUT    R        MATRICE DE HOOKE
!
!
!.======================================================================
    implicit none
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
#include "asterfort/assert.h"
#include "asterfort/dpassa.h"
#include "asterfort/hypmat.h"
#include "asterfort/matini.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/u2mesk.h"
#include "asterfort/utbtab.h"
    character(len=*) :: fami, poum
    integer :: mater, igau, isgau
    real(kind=8) :: repere(7), xyzgau(3), d(6, 6), instan
! -----  VARIABLES LOCALES
    integer :: nbres, nbv, irep, i, j
    parameter (nbres=9)
!
    integer :: icodre(nbres)
    character(len=8) :: nomres(nbres), nompar
    character(len=16) :: phenom
!
    real(kind=8) :: valres(nbres), valpar
    real(kind=8) :: passag(6, 6), dorth(6, 6), work(6, 6)
    real(kind=8) :: nu, nu12, nu21, nu13, nu31, nu23, nu32
    real(kind=8) :: zero, undemi, un, deux, c1, e, c10, c01, c20, k
    real(kind=8) :: coef, coef1, coef2, coef3, e1, e2, e3, delta, g, g1, g2, g3
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! ---- INITIALISATIONS
!      ---------------
    zero = 0.0d0
    undemi = 0.5d0
    un = 1.0d0
    deux = 2.0d0
    nompar = 'INST'
    valpar = instan
!
    call matini(6, 6, zero, d)
    call matini(6, 6, zero, dorth)
    call matini(6, 6, zero, work)
!
!
! ---- RECUPERATION DU TYPE DU MATERIAU DANS PHENOM
!      --------------------------------------------
    call rccoma(mater, 'ELAS', 1, phenom, icodre(1))
!
!      ------------
! ---- CAS ISOTROPE
!      ------------
    if (phenom .eq. 'ELAS') then
!
        nomres(1) = 'E'
        nomres(2) = 'NU'
        nbv = 2
!
! ----   RECUPERATION DES CARACTERISTIQUES MECANIQUES
!        -----------
        call rcvalb(fami, igau, isgau, poum, mater,&
                    ' ', phenom, 1, nompar, valpar,&
                    nbv, nomres, valres, icodre, 1)
!
        e = valres(1)
        nu = valres(2)
!
        coef = un/ ((un+nu)* (un-deux*nu))
        coef1 = e* (un-nu)*coef
        coef2 = e*nu*coef
        coef3 = e/ (un+nu)
!
        d(1,1) = coef1
        d(1,2) = coef2
        d(1,3) = coef2
!
        d(2,1) = coef2
        d(2,2) = coef1
        d(2,3) = coef2
!
        d(3,1) = coef2
        d(3,2) = coef2
        d(3,3) = coef1
!
        d(4,4) = 0.5d0*coef3
        d(5,5) = 0.5d0*coef3
        d(6,6) = 0.5d0*coef3
!
!      ------------
! ---- CAS ELAS_HYPER
!      ------------
    else if (phenom.eq.'ELAS_HYPER') then
!
        call hypmat(fami, igau, isgau, poum, mater,&
                    c10, c01, c20, k)
!
        nu =(3.d0*k-4.0d0*(c10+c01))/(6.d0*k+4.0d0*(c10+c01))
        e = 4.d0*(c10+c01)*(un+nu)
!
        coef = un/ ((un+nu)* (un-deux*nu))
        coef1 = e* (un-nu)*coef
        coef2 = e*nu*coef
        coef3 = e/ (un+nu)
!
        d(1,1) = coef1
        d(1,2) = coef2
        d(1,3) = coef2
!
        d(2,1) = coef2
        d(2,2) = coef1
        d(2,3) = coef2
!
        d(3,1) = coef2
        d(3,2) = coef2
        d(3,3) = coef1
!
        d(4,4) = 0.5d0*coef3
        d(5,5) = 0.5d0*coef3
        d(6,6) = 0.5d0*coef3
!
!      --------------
! ---- CAS ORTHOTROPE
!      --------------
    else if (phenom.eq.'ELAS_ORTH') then
!
        nomres(1) = 'E_L'
        nomres(2) = 'E_T'
        nomres(3) = 'E_N'
        nomres(4) = 'NU_LT'
        nomres(5) = 'NU_LN'
        nomres(6) = 'NU_TN'
        nomres(7) = 'G_LT'
        nomres(8) = 'G_LN'
        nomres(9) = 'G_TN'
        nbv = 9
!
! ----   RECUPERATION DES CARACTERISTIQUES MECANIQUES
!        -----------
        call rcvalb(fami, igau, isgau, poum, mater,&
                    ' ', phenom, 1, nompar, valpar,&
                    nbv, nomres, valres, icodre, 1)
!
        e1 = valres(1)
        e2 = valres(2)
        e3 = valres(3)
        nu12 = valres(4)
        nu13 = valres(5)
        nu23 = valres(6)
        g1 = valres(7)
        g2 = valres(8)
        g3 = valres(9)
!
        nu21 = e2*nu12/e1
        nu31 = e3*nu13/e1
        nu32 = e3*nu23/e2
        delta = un-nu23*nu32-nu31*nu13-nu21*nu12-deux*nu23*nu31*nu12
!
        dorth(1,1) = (un - nu23*nu32)*e1/delta
        dorth(1,2) = (nu21 + nu31*nu23)*e1/delta
        dorth(1,3) = (nu31 + nu21*nu32)*e1/delta
        dorth(2,2) = (un - nu13*nu31)*e2/delta
        dorth(2,3) = (nu32 + nu31*nu12)*e2/delta
        dorth(3,3) = (un - nu21*nu12)*e3/delta
        dorth(2,1) = dorth(1,2)
        dorth(3,1) = dorth(1,3)
        dorth(3,2) = dorth(2,3)
!
        dorth(4,4) = g1
        dorth(5,5) = g2
        dorth(6,6) = g3
! ----   CALCUL DE LA MATRICE DE PASSAGE DU REPERE D'ORTHOTROPIE AU
! ----   REPERE GLOBAL POUR LE TENSEUR D'ELASTICITE
!        ------------------------------------------
        call dpassa(xyzgau, repere, irep, passag)
!
! ----   TENSEUR D'ELASTICITE DANS LE REPERE GLOBAL :
! ----    D_GLOB = PASSAG_T * D_ORTH * PASSAG
! ----    (ON NE FAIT REELLEMENT LE PRODUIT QUE SI LA MATRICE
! ----     DE PASSAGE N'EST PAS L'IDENTITE)
!        ----------------------------------
        ASSERT((irep.eq.1).or.(irep.eq.0))
        if (irep .eq. 1) then
            call utbtab('ZERO', 6, 6, dorth, passag,&
                        work, d)
        else if (irep.eq.0) then
            do 40 i = 1, 6
                do 30 j = 1, 6
                    d(i,j) = dorth(i,j)
30              continue
40          continue
        endif
!
!      -----------------------
! ---- CAS ISOTROPE-TRANSVERSE
!      -----------------------
    else if (phenom.eq.'ELAS_ISTR') then
!
        nomres(1) = 'E_L'
        nomres(2) = 'E_N'
        nomres(3) = 'NU_LT'
        nomres(4) = 'NU_LN'
        nomres(5) = 'G_LN'
        nbv = 5
!
! ----   RECUPERATION DES CARACTERISTIQUES MECANIQUES
!        -----------
        call rcvalb(fami, igau, isgau, poum, mater,&
                    ' ', phenom, 1, nompar, valpar,&
                    nbv, nomres, valres, icodre, 1)
!
        e1 = valres(1)
        e3 = valres(2)
        nu12 = valres(3)
        nu13 = valres(4)
        nu31 = nu13*e3/e1
        g = valres(5)
!
        c1 = e1/ (un+nu12)
        delta = un - nu12 - deux*nu13*nu31
!
        dorth(1,1) = c1* (un-nu13*nu31)/delta
        dorth(1,2) = c1* ((un-nu13*nu31)/delta-un)
        dorth(1,3) = e3*nu13/delta
        dorth(2,1) = dorth(1,2)
        dorth(2,2) = dorth(1,1)
        dorth(2,3) = dorth(1,3)
        dorth(3,1) = dorth(1,3)
        dorth(3,2) = dorth(2,3)
        dorth(3,3) = e3* (un-nu12)/delta
        dorth(4,4) = undemi*c1
        dorth(5,5) = g
        dorth(6,6) = dorth(5,5)
!
! ----   CALCUL DE LA MATRICE DE PASSAGE DU REPERE D'ORTHOTROPIE AU
! ----   REPERE GLOBAL POUR LE TENSEUR D'ELASTICITE
!        ------------------------------------------
        call dpassa(xyzgau, repere, irep, passag)
!
! ----   TENSEUR D'ELASTICITE DANS LE REPERE GLOBAL :
! ----    D_GLOB = PASSAG_T * D_ORTH * PASSAG
! ----    (ON NE FAIT REELLEMENT LE PRODUIT QUE SI LA MATRICE
! ----     DE PASSAGE N'EST PAS L'IDENTITE)
!        ----------------------------------
        ASSERT((irep.eq.1).or.(irep.eq.0))
        if (irep .eq. 1) then
            call utbtab('ZERO', 6, 6, dorth, passag,&
                        work, d)
        else if (irep.eq.0) then
            do 60 i = 1, 6
                do 50 j = 1, 6
                    d(i,j) = dorth(i,j)
50              continue
60          continue
        endif
!
    else
        call u2mesk('F', 'ELEMENTS_15', 1, phenom)
    endif
!.============================ FIN DE LA ROUTINE ======================
end subroutine
