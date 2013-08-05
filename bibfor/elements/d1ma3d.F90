subroutine d1ma3d(fami, mater, instan, poum, kpg,&
                  ksp, repere, xyzgau, d1)
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
!.======================================================================
    implicit none
!
!     D1MA3D  --   CALCUL DE L'INVERSE DE LA MATRICE DE HOOKE
!                  POUR LES ELEMENTS MASSIFS EN 3D OU EN SERIE DE
!                  FOURIER POUR DES MATERIAUX ISOTROPE, ORTHOTROPE
!                  ET ISOTROPE TRANSVERSE
!
!   ARGUMENT        E/S  TYPE         ROLE
!    FAMI           IN     K*       FAMILLE DU POINT DE GAUSS
!    MATER          IN     I        MATERIAU
!    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
!    POUM           IN     K1       T ou T+DT
!    KPG            IN     I        POINT DE GAUSS
!    KSP            IN     I        SOUS-POINT DE GAUSS
!    REPERE(7)      IN     R        VALEURS DEFINISSANT LE REPERE
!                                   D'ORTHOTROPIE
!    XYZGAU(3)      IN     R        COORDONNEES DU POINT D'INTEGRATION
!    D1(6,6)        OUT    R        INVERSE DE LA MATRICE DE HOOKE
!
!
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
#include "asterfort/assert.h"
#include "asterfort/d1pa3d.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/u2mesk.h"
#include "asterfort/utbtab.h"
    character(len=*) :: poum, fami
    integer :: kpg, ksp
    real(kind=8) :: repere(7), xyzgau(3), d1(6, 6), instan
! -----  VARIABLES LOCALES
!-----------------------------------------------------------------------
    integer :: i, irep, j, mater, nbres, nbv
    real(kind=8) :: coef1, coef2, coef3, deux, e, e1, e2
    real(kind=8) :: e3, un, zero
!-----------------------------------------------------------------------
    parameter (nbres = 9)
!
    integer :: icodre(nbres)
    character(len=8) :: nomres(nbres), nompar(2)
    character(len=16) :: phenom
!
    real(kind=8) :: valres(nbres), valpar(1)
    real(kind=8) :: passag(6, 6), d1orth(6, 6), work(6, 6)
    real(kind=8) :: nu, nu12, nu21, nu13, nu23, nu31, nu32
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! ---- INITIALISATIONS
!      ---------------
    zero = 0.0d0
    un = 1.0d0
    deux = 2.0d0
!
    nompar(1) = 'INST'
    valpar(1) = instan
!
    do 10 i = 1, 6
        do 10 j = 1, 6
            d1(i,j) = zero
            d1orth(i,j) = zero
            work(i,j) = zero
10      continue
!
! ---- RECUPERATION DU TYPE DU MATERIAU DANS PHENOM
!      --------------------------------------------
    call rccoma(mater, 'ELAS', 1, phenom, icodre)
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
! ----   INTERPOLATION DES COEFFICIENTS EN FONCTION DE LA TEMPERATURE
! ----   ET DU TEMPS
!        -----------
        call rcvalb(fami, kpg, ksp, poum, mater,&
                    ' ', phenom, 1, nompar, valpar,&
                    nbv, nomres, valres, icodre, 1)
!
        e = valres(1)
        nu = valres(2)
!
        coef1 = un/e
        coef2 = -nu/e
        coef3 = deux*(un + nu)/e
!
        d1(1,1) = coef1
        d1(1,2) = coef2
        d1(1,3) = coef2
!
        d1(2,1) = coef2
        d1(2,2) = coef1
        d1(2,3) = coef2
!
        d1(3,1) = coef2
        d1(3,2) = coef2
        d1(3,3) = coef1
!
        d1(4,4) = coef3
        d1(5,5) = coef3
        d1(6,6) = coef3
!
!      --------------
! ---- CAS ORTHOTROPE
!      --------------
    else if (phenom.eq.'ELAS_ORTH') then
!
        nomres(1)='E_L'
        nomres(2)='E_T'
        nomres(3)='E_N'
        nomres(4)='NU_LT'
        nomres(5)='NU_LN'
        nomres(6)='NU_TN'
        nomres(7)='G_LT'
        nomres(8)='G_LN'
        nomres(9)='G_TN'
        nbv = 9
!
! ----   INTERPOLATION DES COEFFICIENTS EN FONCTION DE LA TEMPERATURE
! ----   ET DU TEMPS
!        -----------
        call rcvalb(fami, kpg, ksp, poum, mater,&
                    ' ', phenom, 1, nompar, valpar,&
                    nbv, nomres, valres, icodre, 1)
!
        e1 = valres(1)
        e2 = valres(2)
        e3 = valres(3)
        nu12 = valres(4)
        nu13 = valres(5)
        nu23 = valres(6)
        nu21 = e2*nu12/e1
        nu31 = e3*nu13/e1
        nu32 = e3*nu23/e2
!
        d1orth(1,1) = un/e1
        d1orth(1,2) = -nu21/e2
        d1orth(1,3) = -nu31/e3
        d1orth(2,2) = un/e2
        d1orth(2,3) = -nu32/e3
        d1orth(3,3) = un/e3
        d1orth(2,1) = d1orth(1,2)
        d1orth(3,1) = d1orth(1,3)
        d1orth(3,2) = d1orth(2,3)
!
        d1orth(4,4) = un/valres(7)
        d1orth(5,5) = un/valres(8)
        d1orth(6,6) = un/valres(9)
!
! ----   CALCUL DE LA MATRICE DE PASSAGE DU REPERE D'ORTHOTROPIE AU
! ----   REPERE GLOBAL POUR L'INVERSE DE LA MATRICE DE HOOKE
!        ---------------------------------------------------
        call d1pa3d(xyzgau, repere, irep, passag)
!
! ----   'INVERSE' DU TENSEUR D'ELASTICITE DANS LE REPERE GLOBAL :
! ----    D1_GLOB = PASSAG_T * D1_ORTH * PASSAG
! ----    (ON NE FAIT REELLEMENT LE PRODUIT QUE SI LA MATRICE
! ----     DE PASSAGE N'EST PAS L'IDENTITE)
!        ----------------------------------
        ASSERT((irep.eq.1).or.(irep.eq.0))
        if (irep .eq. 1) then
            call utbtab('ZERO', 6, 6, d1orth, passag,&
                        work, d1)
        else if (irep.eq.0) then
            do 20 i = 1, 6
                do 20 j = 1, 6
                    d1(i,j) = d1orth(i,j)
20              continue
        endif
!
!      -----------------------
! ---- CAS ISOTROPE-TRANSVERSE
!      -----------------------
    else if (phenom.eq.'ELAS_ISTR') then
!
        nomres(1)='E_L'
        nomres(2)='E_N'
        nomres(3)='NU_LT'
        nomres(4)='NU_LN'
        nomres(5)='G_LN'
        nbv = 5
!
! ----   INTERPOLATION DES COEFFICIENTS EN FONCTION DE LA TEMPERATURE
! ----   ET DU TEMPS
!        -----------
        call rcvalb(fami, kpg, ksp, poum, mater,&
                    ' ', phenom, 1, nompar, valpar,&
                    nbv, nomres, valres, icodre, 1)
!
        e1 = valres(1)
        e3 = valres(2)
        nu12 = valres(3)
        nu13 = valres(4)
        nu31 = e3*nu13/e1
!
        d1orth(1,1) = un/e1
        d1orth(1,2) = -nu12/e1
        d1orth(1,3) = -nu31/e3
        d1orth(2,1) = d1orth(1,2)
        d1orth(2,2) = un/e1
        d1orth(2,3) = -nu31/e3
        d1orth(3,1) = d1orth(1,3)
        d1orth(3,2) = d1orth(2,3)
        d1orth(3,3) = un/e3
        d1orth(4,4) = deux*(un+nu12)/e1
        d1orth(5,5) = un/valres(5)
        d1orth(6,6) = d1orth(5,5)
!
! ----   CALCUL DE LA MATRICE DE PASSAGE DU REPERE D'ORTHOTROPIE AU
! ----   REPERE GLOBAL POUR L'INVERSE DE LA MATRICE DE HOOKE
!        ---------------------------------------------------
        call d1pa3d(xyzgau, repere, irep, passag)
!
! ----   'INVERSE' DU TENSEUR D'ELASTICITE DANS LE REPERE GLOBAL :
! ----    D_GLOB = PASSAG_T * D_ORTH * PASSAG
! ----    (ON NE FAIT REELLEMENT LE PRODUIT QUE SI LA MATRICE
! ----     DE PASSAGE N'EST PAS L'IDENTITE)
!        ----------------------------------
        ASSERT((irep.eq.1).or.(irep.eq.0))
        if (irep .eq. 1) then
            call utbtab('ZERO', 6, 6, d1orth, passag,&
                        work, d1)
        else if (irep.eq.0) then
            do 30 i = 1, 6
                do 30 j = 1, 6
                    d1(i,j) = d1orth(i,j)
30              continue
        endif
!
    else
        call u2mesk('F', 'ELEMENTS_15', 1, phenom)
    endif
!.============================ FIN DE LA ROUTINE ======================
end subroutine
