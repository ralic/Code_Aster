subroutine rsexis(nomsd, ier)
    implicit none
#include "asterfort/jeexin.h"
    character(len=*) :: nomsd
    integer :: ier
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     VERIFICATION DE L'EXISTENCE D'UNE STRUCTURE DE DONNEES
!                  "RESULTAT-COMPOSE".
!     ------------------------------------------------------------------
! IN  NOMSD  : NOM DE LA STRUCTURE "RESULTAT" A CREER.
! OUT IER    : CODE D'EXISTENCE
!            = 0 N'EXISTE PAS
!           /= 0 EXISTE
!     ------------------------------------------------------------------
    character(len=24) :: desc
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    data         desc/'                   .DESC'/
!     ------------------------------------------------------------------
    desc(1:8) = nomsd
    call jeexin(desc, ier)
end subroutine
