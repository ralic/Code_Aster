subroutine fin999()
    implicit none
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
! person_in_charge: mathieu.courtois at edf.fr
!-----------------------------------------------------------------------
!
!     MENAGE DANS :
!        * LES BIBLIOTHEQUES
!        * LES ALARMES/ERREURS
!        * LES COMMUNICATEURS MPI
!
!-----------------------------------------------------------------------
#include "asterf.h"
#include "asterc/chkmsg.h"
#include "asterc/dllcls.h"
#include "asterc/lcdiscard.h"
#include "asterfort/apetsc.h"
#include "asterfort/asmpi_checkalarm.h"
    integer :: ichk
#ifdef _HAVE_PETSC
    integer :: iret
    real(kind=8) :: r8b
#endif
!-----------------------------------------------------------------------
!
! --- FERMETURE DE PETSC
!
#ifdef _HAVE_PETSC
    call apetsc('FIN', ' ', ' ', [0.d0], ' ',&
                0, 0, iret)
#endif
!
! --- LIBERATION DE TOUS LES COMPOSANTS CHARGES DYNAMIQUEMENT
!
    call dllcls()
!
! --- VERIFICATION DES ALARMES EN PARALLELE
!
    call asmpi_checkalarm()
!
! --- TEST ERREUR E SANS ERREUR F
!
    call chkmsg(1, ichk)
!
    call lcdiscard(" ")
!
end subroutine
