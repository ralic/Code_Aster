subroutine poslag(typlaz, ilag1, ilag2)
    implicit none
#include "asterfort/u2mesk.h"
    character(len=*) :: typlaz
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!     BUT : INDICATEUR DE LA POSITION A AFFECTER AUX MULTIPLICATEURS
!           DE LAGRANGE ASSOCIES A UNE RELATION
!
! TYPLAG        - IN - K2  - : TYPE DES MULTIPLICATEURS DE LAGRANGE
!                              ASSOCIES A LA RELATION :
!                              SI = '12'  LE PREMIER LAGRANGE EST AVANT
!                                         LE NOEUD PHYSIQUE
!                                         LE SECOND LAGRANGE EST APRES
!                              SI = '22'  LE PREMIER LAGRANGE EST APRES
!                                         LE NOEUD PHYSIQUE
!                                         LE SECOND LAGRANGE EST APRES
! ILAG1         - OUT - I  - : INDICATEUR DE POSITION DU PREMIER
!                              LAGRANGE :
!                              SI AVANT LE NOEUD PHYSIQUE ILAG1 = +1
!                              SI APRES LE NOEUD PHYSIQUE ILAG1 = -1
! ILAG2         - OUT - I  - : INDICATEUR DE POSITION DU SECOND
!                              LAGRANGE :
!                              SI APRES LE NOEUD PHYSIQUE ILAG2 = -2
!                              (C'EST LA SEULE POSSIBILITE POUR LE
!                               MOMENT)
!-----------------------------------------------------------------------
!
    character(len=2) :: typlag
!
!-----------------------------------------------------------------------
    integer :: ilag1, ilag2
!-----------------------------------------------------------------------
    typlag = typlaz
!
    if (typlag(1:1) .eq. '1') then
        ilag1 = 1
    else if (typlag(1:1).eq.'2') then
        ilag1 = -1
    else
        call u2mesk('F', 'MODELISA6_30', 1, typlag)
    endif
!
    if (typlag(2:2) .eq. '2') then
        ilag2 = -2
    else
        call u2mesk('F', 'MODELISA6_30', 1, typlag)
    endif
!
end subroutine
