subroutine cgveth(typfis, cas)
    implicit none
!
#include "asterfort/u2mess.h"
    character(len=8) :: typfis
    character(len=16) :: cas
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
!     SOUS-ROUTINE DE L'OPERATEUR CALC_G
!
!     BUT : VERIFICATION DES DONNEES RELATIVES AU(X) CHAMP(S) THETA
!
!  IN :
!    TYPFIS : TYPE DE LA SD DECRIVANT LE FOND DE FISSURE
!            ('THETA' OU 'FONDIFSS' OU 'FISSURE')
!    CAS    : '2D', '3D LOCAL' OU '3D GLOBAL'
! ======================================================================
!
!     SI LE CHAMP THETA EST FOURNI
    if (typfis .eq. 'THETA') then
!
!       ON NE DOIT PAS ETRE DANS UN CALCUL 3D LOCAL
        if (cas .eq. '3D_LOCAL') call u2mess('F', 'RUPTURE0_57')
!
!     SI LE CHAMP THETA N'EST PAS FOURNI
!      ELSE
!
    endif
!
end subroutine
