subroutine ldsp1(pc, ierr)
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
    implicit none
! person_in_charge: thomas.desoza at edf.fr
!----------------------------------------------------------------
!
!  PRECONDITIONNEUR ISSU D'UNE FACTORISATION SIMPLE PRECISION
!
!----------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/pcmump.h"
#ifdef _HAVE_PETSC
#   include "aster_petsc.h"
!----------------------------------------------------------------
!     VARIABLES LOCALES
    integer :: jrefa, iret
!----------------------------------------------------------------
!     Variables PETSc
    PetscInt :: ierr
    PC :: pc
!----------------------------------------------------------------
!
    call jemarq()
!
! --  LA MATRICE A ETE TRAITEE PAR PETSC DONC ELLE EST CONSIDEREE
! --  COMME FACTORISEE, SI ON LA LAISSE COMME CA MUMPH VA LA
! --  CONSIDERER COMME TELLE ET NE FERA PAS DE FACTO SP
    call jeveuo(spmat//'.REFA', 'E', jrefa)
    zk24(jrefa-1+8) = ' '
!
! --  APPEL A LA ROUTINE DE FACTO SP POUR LE PRECONDITIONNEMENT
    call pcmump(spmat, spsolv, iret)
!
    ierr = iret
!
    call jedema()
!
#else
!
!      DECLARATION BIDON POUR ASSURER LA COMPILATION
    integer :: pc, ierr
!
#endif
!
end subroutine
