subroutine dmatcp(fami, mater, instan, poum, igau,&
                  isgau, repere, d)
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
!
!      DMATCP --   CALCUL DE LA MATRICE DE HOOKE POUR LES ELEMENTS
!                  MASSIFS 2D EN CONTRAINTES PLANES
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
!    REPERE(3)      IN     R        VALEURS DEFINISSANT LE REPERE
!                                   D'ORTHOTROPIE
!    D(4,4)         OUT    R        MATRICE DE HOOKE
!
!.======================================================================
!
    implicit none
! -----  ARGUMENTS
#include "asterfort/assert.h"
#include "asterfort/dpao2d.h"
#include "asterfort/hypmat.h"
#include "asterfort/matini.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utbtab.h"
#include "asterfort/utmess.h"
    character(len=*) :: fami, poum
    real(kind=8) :: repere(7), d(4, 4), instan
    integer :: mater, igau, isgau
! -----  VARIABLES LOCALES
    integer :: nbres, i, j, nbv, irep
    parameter (nbres=7)
!
    integer :: icodre(nbres)
    character(len=8) :: nomres(nbres), nompar
    character(len=16) :: phenom
!
    real(kind=8) :: valres(nbres), zero, undemi, un, valpar
    real(kind=8) :: passag(4, 4), dorth(4, 4), work(4, 4), e, nu
    real(kind=8) :: e1, e2, nu12, nu21, c1, delta, g12, c10, c01, c20, k
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
! ---- INITIALISATIONS
!      ---------------
    zero = 0.0d0
    undemi = 0.5d0
    un = 1.0d0
    nompar = 'INST'
    valpar = instan
!
    call matini(4, 4, zero, d)
    call matini(4, 4, zero, dorth)
    call matini(4, 4, zero, work)
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
                    ' ', phenom, 1, nompar, [valpar],&
                    nbv, nomres, valres, icodre, 1)
!
        e = valres(1)
        nu = valres(2)
!
        d(1,1) = e/ (un-nu*nu)
        d(1,2) = d(1,1)*nu
!
        d(2,1) = d(1,2)
        d(2,2) = d(1,1)
!
        d(4,4) = undemi*e/ (un+nu)
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
!
        d(1,1) = e/ (un-nu*nu)
        d(1,2) = d(1,1)*nu
!
        d(2,1) = d(1,2)
        d(2,2) = d(1,1)
!
        d(4,4) = undemi*e/ (un+nu)
!
!      --------------
! ---- CAS ORTHOTROPE
!      --------------
    else if (phenom.eq.'ELAS_ORTH') then
!
        nomres(1) = 'E_L'
        nomres(2) = 'E_T'
        nomres(3) = 'NU_LT'
        nomres(4) = 'G_LT'
        nbv = 4
!
! ----   RECUPERATION DES CARACTERISTIQUES MECANIQUES
!        -----------
        call rcvalb(fami, igau, isgau, poum, mater,&
                    ' ', phenom, 1, nompar, [valpar],&
                    nbv, nomres, valres, icodre, 1)
!
!
        e1 = valres(1)
        e2 = valres(2)
        nu12 = valres(3)
        g12 = valres(4)
        nu21 = e2*nu12/e1
        delta = un-nu12*nu21
!
        dorth(1,1) = e1/delta
        dorth(1,2) = nu12*e2/delta
        dorth(2,2) = e2/delta
        dorth(2,1) = dorth(1,2)
!
        dorth(4,4) = g12
!
! ----   CALCUL DE LA MATRICE DE PASSAGE DU REPERE D'ORTHOTROPIE AU
! ----   REPERE GLOBAL POUR LE TENSEUR D'ELASTICITE
!        ------------------------------------------
        call dpao2d(repere, irep, passag)
!
! ----   TENSEUR D'ELASTICITE DANS LE REPERE GLOBAL :
! ----    D_GLOB = PASSAG_T * D_ORTH * PASSAG
! ----    (ON NE FAIT REELLEMENT LE PRODUIT QUE SI LA MATRICE
! ----     DE PASSAGE N'EST PAS L'IDENTITE)
!        ----------------------------------
        ASSERT((irep.eq.1).or.(irep.eq.0))
        if (irep .eq. 1) then
            call utbtab('ZERO', 4, 4, dorth, passag,&
                        work, d)
        else if (irep.eq.0) then
            do 40 i = 1, 4
                do 30 j = 1, 4
                    d(i,j) = dorth(i,j)
30              continue
40          continue
        endif
!
!
!      -----------------------
! ---- CAS ISOTROPE-TRANSVERSE
!      -----------------------
    else if (phenom.eq.'ELAS_ISTR') then
!
        nomres(1) = 'E_L'
        nomres(2) = 'NU_LT'
        nbv = 2
!
! ----   RECUPERATION DES CARACTERISTIQUES MECANIQUES
!        -----------
        call rcvalb(fami, igau, isgau, poum, mater,&
                    ' ', phenom, 1, nompar, [valpar],&
                    nbv, nomres, valres, icodre, 1)
!
        e = valres(1)
        nu = valres(2)
!
        c1 = e/ (un+nu)
        delta = un - nu*nu
!
        d(1,1) = e/delta
        d(1,2) = nu*d(1,1)
        d(2,1) = d(1,2)
        d(2,2) = d(1,1)
        d(4,4) = undemi*c1
!
    else
        call utmess('F', 'ELEMENTS_15', sk=phenom)
    endif
!.============================ FIN DE LA ROUTINE ======================
end subroutine
