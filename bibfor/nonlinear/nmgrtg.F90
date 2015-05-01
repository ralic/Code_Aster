subroutine nmgrtg(ndim, nno, poids, kpg, vff,&
                  dfdi, def, pff, option, axi,&
                  r, fm, f, dsidep, sign,&
                  sigma, matsym, matuu, vectu)
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
    implicit none
!
#include "asterf_types.h"
#include "asterfort/nmfdff.h"
#include "asterfort/nmgrt2.h"
#include "asterfort/nmgrt3.h"
#include "blas/dcopy.h"
    integer :: ndim, nno, kpg
    character(len=16) :: option
    real(kind=8) :: pff(*), def(*), r, dsidep(6, 6), poids, vectu(*)
    real(kind=8) :: sigma(6), sign(6), matuu(*), vff(*)
    real(kind=8) :: fm(3, 3), f(3, 3), fr(3, 3), dfdi(*)
    aster_logical :: matsym, axi, resi, rigi
!
!.......................................................................
!     BUT:  CALCUL DE LA MATRICE TANGENTE EN CONFIGURATION LAGRANGIENNE
!           OPTIONS RIGI_MECA_TANG ET FULL_MECA
!.......................................................................
! IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
! IN  NDIM    : DIMENSION DU PB
! IN  POIDS   : POIDS DES POINTS DE GAUSS
! IN  KPG     : NUMERO DU POINT DE GAUSS
! IN  VFF     : VALEUR  DES FONCTIONS DE FORME
! IN  DEF     : PRODUIT DE F PAR LA DERIVEE DES FONCTIONS DE FORME
! IN  PFF     : PRODUIT DES FONCTIONS DE FORME
! IN  OPTION  : OPTION DE CALCUL
! IN  AXI     : .TRUE. SI AXIS
! IN  R       : RAYON DU POINT DE GAUSS COURANT (EN AXI)
! IN  DSIDEP  : OPERATEUR TANGENT ISSU DU COMPORTEMENT
! IN  SIGN    : CONTRAINTES PK2 A L'INSTANT PRECEDENT (AVEC RAC2)
! IN  SIGMA   : CONTRAINTES PK2 A L'INSTANT ACTUEL    (AVEC RAC2)
! IN  MATSYM  : VRAI SI LA MATRICE DE RIGIDITE EST SYMETRIQUE
! OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
!.......................................................................
!
    resi = option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL'
    rigi = option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL'
!
!     CALCUL DES PRODUITS SYMETR. DE F PAR N
!
    if (resi) then
        call dcopy(9, f, 1, fr, 1)
    else
        call dcopy(9, fm, 1, fr, 1)
    endif
!
    call nmfdff(ndim, nno, axi, kpg, r,&
                rigi, matsym, fr, vff, dfdi,&
                def, pff)
!
!     DEUX ROUTINES DIFFERENTES POUR OPTIMISER lE TEMPS CPU
!
    if (ndim .eq. 3) then
!
        call nmgrt3(nno, poids, kpg, vff, def,&
                    pff, option, axi, r, resi,&
                    rigi, dsidep, sign, sigma, matsym,&
                    matuu, vectu)
!
    else if (ndim.eq.2) then
!
        call nmgrt2(nno, poids, kpg, vff, def,&
                    pff, option, axi, r, resi,&
                    rigi, dsidep, sign, sigma, matsym,&
                    matuu, vectu)
!
    endif
!
end subroutine
