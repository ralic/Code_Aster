subroutine ibimpr()
    implicit none
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     DEFINITION DES UNITES LOGIQUES DES IMPRESSIONS
!     ------------------------------------------------------------------
!
!     CONSERVER LA COHERENCE AVEC IUNIFI ET ULOPEN
#include "asterfort/uldefi.h"
    integer :: mximpr
    parameter   ( mximpr = 3)
    character(len=16) :: nompr (mximpr)
    integer :: unitpr (mximpr)
    character(len=1) :: autpr(mximpr)
    integer :: i, passe
    save          passe
    data          passe  /    0     /
    data          nompr  /'MESSAGE'  , 'RESULTAT', 'ERREUR'/
    data          unitpr /    6      ,     8     ,      9  /
    data          autpr /    'N'    ,     'O'     ,    'N' /
!     ------------------------------------------------------------------
    passe = passe + 1
!
! --- DEFINITION DES UNITES STANDARDS
    if (passe .eq. 1) then
        do 5 i = 1, mximpr
            call uldefi(unitpr(i), ' ', nompr(i), 'A', 'N',&
                        autpr(i))
 5      continue
    endif
    call uldefi(15, ' ', 'CODE', 'A', 'A',&
                'O')
!
end subroutine
