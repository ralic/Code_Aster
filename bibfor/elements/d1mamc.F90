subroutine d1mamc(fami, mater, instan, poum, kpg,&
                  ksp, repere, xyzgau, nbsig, d1)
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
!.======================================================================
    implicit none
!
!      D1MAMC :   CALCUL DE L'INVERSE DE LA MATRICE DE HOOKE
!                 POUR LES ELEMENTS ISOPARAMETRIQUES POUR DES
!                 MATERIAUX ISOTROPE, ORTHOTROPE ET ISOTROPE TRANSVERSE
!
!   ARGUMENT        E/S  TYPE         ROLE
!    MATER          IN     I        MATERIAU
!    INSTAN         IN     R        INSTANT DE CALCUL (0 PAR DEFAUT)
!    REPERE(7)      IN     R        VALEURS DEFINISSANT LE REPERE
!                                   D'ORTHOTROPIE
!    XYZGAU(3)      IN     R        COORDONNEES DU POINT D'INTEGRATION
!    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE A
!                                   L'ELEMENT
!    D1(NBSIG,1)    OUT    R        MATRICE DE HOOKE
!
!
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  ARGUMENTS
#include "asterfort/d1ma3d.h"
#include "asterfort/d1macp.h"
#include "asterfort/d1madp.h"
#include "asterfort/lteatt.h"
#include "asterfort/utmess.h"
    character(len=*) :: fami, poum
    integer :: kpg, ksp
    integer :: mater, nbsig
    real(kind=8) :: repere(7), xyzgau(*), d1(nbsig, 1), instan
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
!       ------------------------
! ----  CAS MASSIF 3D ET FOURIER
!       ------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    if (lteatt('DIM_TOPO_MAILLE','3') .or. lteatt('FOURIER','OUI')) then
!
        call d1ma3d(fami, mater, instan, poum, kpg,&
                    ksp, repere, xyzgau, d1)
!
!       ----------------------------------------
! ----  CAS DEFORMATIONS PLANES ET AXISYMETRIQUE
!       ----------------------------------------
        elseif(lteatt('D_PLAN','OUI').or. lteatt('AXIS', 'OUI'))&
    then
!
        call d1madp(fami, mater, instan, poum, kpg,&
                    ksp, repere, d1)
!
!       ----------------------
! ----  CAS CONTRAINTES PLANES
!       ----------------------
    else if (lteatt('C_PLAN','OUI')) then
!
        call d1macp(fami, mater, instan, poum, kpg,&
                    ksp, repere, d1)
!
    else
        call utmess('F', 'ELEMENTS_11')
    endif
!.============================ FIN DE LA ROUTINE ======================
end subroutine
