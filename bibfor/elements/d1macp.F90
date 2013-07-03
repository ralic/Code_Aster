subroutine d1macp(fami, mater, instan, poum, kpg,&
                  ksp, repere, d1)
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
!      D1MACP --   CALCUL DE L'INVERSE DE LA MATRICE DE HOOKE
!                  POUR LES ELEMENTS MASSIFS 2D EN CONTRAINTES PLANES
!                  POUR DES MATERIAUX ISOTROPE, ORTHOTROPE
!                  ET ISOTROPE TRANSVERSE
!
!   ARGUMENT        E/S  TYPE         ROLE
!    FAMI           IN     K*       FAMILLE DU POINT DE GAUSS
!    MATER          IN     I        MATERIAU
!    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
!    POUM           IN     K        TEMPERATURE +/-
!    KPG            IN     I        POINT DE GAUSS
!    KSP            IN     I        SOUS-POINT DE GAUSS
!    REPERE(3)      IN     R        VALEURS DEFINISSANT LE REPERE
!                                   D'ORTHOTROPIE
!    D1(4,4)        OUT    R        INVERSE DE LA MATRICE DE HOOKE
!
!
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
#include "asterfort/assert.h"
#include "asterfort/d1pa2d.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/u2mesk.h"
#include "asterfort/utbtab.h"
    character(len=*) :: fami, poum
    integer :: kpg, ksp
    real(kind=8) :: repere(7), d1(4, *), instan
! -----  VARIABLES LOCALES
!-----------------------------------------------------------------------
    integer :: i, irep, j, mater, nbres, nbv
    real(kind=8) :: deux, e, e1, e2, un, zero
!-----------------------------------------------------------------------
    parameter (nbres = 7)
!
    integer :: icodre(nbres)
    character(len=8) :: nomres(nbres), nompar
    character(len=16) :: phenom
!
    real(kind=8) :: valres(nbres), valpar
    real(kind=8) :: passag(4, 4), d1orth(4, 4), work(4, 4)
    real(kind=8) :: nu, nu12, nu21
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! ---- INITIALISATIONS
!      ---------------
    zero = 0.0d0
    un = 1.0d0
    deux = 2.0d0
!
    nompar = 'INST'
    valpar = instan
!
    do 10 i = 1, 4
        do 10 j = 1, 4
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
! ----   INTERPOLATION DES COEFFICIENTS EN FONCTION DU TEMPS
!        -----------
        call rcvalb(fami, kpg, ksp, poum, mater,&
                    ' ', phenom, 1, nompar, valpar,&
                    nbv, nomres, valres, icodre, 1)
!
        e = valres(1)
        nu = valres(2)
!
        d1(1,1) = un/e
        d1(1,2) = -nu/e
!
        d1(2,1) = d1(1,2)
        d1(2,2) = d1(1,1)
!
        d1(4,4) = deux*(un + nu)/e
!
!      --------------
! ---- CAS ORTHOTROPE
!      --------------
    else if (phenom.eq.'ELAS_ORTH') then
!
        nomres(1)='E_L'
        nomres(2)='E_T'
        nomres(3)='NU_LT'
        nomres(4)='G_LT'
        nbv = 4
!
! ----   INTERPOLATION DES COEFFICIENTS EN FONCTION DU TEMPS
!        -----------
        call rcvalb(fami, kpg, ksp, '+', mater,&
                    ' ', phenom, 1, nompar, valpar,&
                    nbv, nomres, valres, icodre, 1)
!
        e1 = valres(1)
        e2 = valres(2)
        nu12 = valres(3)
        nu21 = e2*nu12/e1
!
        d1orth(1,1) = un/e1
        d1orth(1,2) = -nu21/e2
        d1orth(2,2) = un/e2
        d1orth(2,1) = d1orth(1,2)
!
        d1orth(4,4) = un/valres(4)
!
! ----   CALCUL DE LA MATRICE DE PASSAGE DU REPERE D'ORTHOTROPIE AU
! ----   REPERE GLOBAL POUR L'INVERSE DE LA MATRICE DE HOOKE
!        ---------------------------------------------------
        call d1pa2d(repere, irep, passag)
!
! ----   'INVERSE' DU TENSEUR D'ELASTICITE DANS LE REPERE GLOBAL :
! ----    D1_GLOB = PASSAG_T * D1_ORTH * PASSAG
! ----    (ON NE FAIT REELLEMENT LE PRODUIT QUE SI LA MATRICE
! ----     DE PASSAGE N'EST PAS L'IDENTITE)
!        ----------------------------------
        call assert((irep.eq.1).or.(irep.eq.0))
        if (irep .eq. 1) then
            call utbtab('ZERO', 4, 4, d1orth, passag,&
                        work, d1)
        else if (irep.eq.0) then
            do 20 i = 1, 4
                do 20 j = 1, 4
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
        nomres(2)='NU_LT'
        nbv = 2
!
! ----   INTERPOLATION DES COEFFICIENTS EN FONCTION DU TEMPS
!        -----------
        call rcvalb(fami, kpg, ksp, '+', mater,&
                    ' ', phenom, 1, nompar, valpar,&
                    nbv, nomres, valres, icodre, 1)
!
        e = valres(1)
        nu = valres(2)
!
        d1(1,1) = un/e
        d1(1,2) = -nu/e
        d1(2,1) = d1(1,2)
        d1(2,2) = d1(1,1)
        d1(4,4) = deux*(un+nu)/e
!
    else
        call u2mesk('F', 'ELEMENTS_15', 1, phenom)
    endif
!.============================ FIN DE LA ROUTINE ======================
end subroutine
