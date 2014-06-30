subroutine apbloc(matass, solveu, tbloc)
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
! person_in_charge: thomas.de-soza at edf.fr
#include "asterf.h"
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=19) :: matass, solveu
    integer :: tbloc
!----------------------------------------------------------------
!
!  DETERMINATION DU NOMBRE DE DDLS PAR NOEUD SELON LE PC
!
!  IN  K19 MATASS : MATRICE ASSEMBLEE
!  IN  K19 SOLVEU : SD SOLVEUR
!  OUT I   TBLOC  : NOMBRE DE DDLS PAR NOEUD PHYSIQUE
!                   TBLOC < 0 SI NOMBRE DE DDLS NON CONSTANT
!----------------------------------------------------------------
!
#ifdef _HAVE_PETSC
!
!----------------------------------------------------------------
!     VARIABLES LOCALES
    character(len=24) :: precon
    logical(kind=1) :: leliml
    character(len=24), pointer :: slvk(:) => null()
!
!----------------------------------------------------------------
    call jemarq()
!
    call dismoi('NB_DDL_NOEUD', matass, 'MATR_ASSE', repi=tbloc)
!
!     -- PRECONDITIONNEUR UTILISE
    call jeveuo(solveu//'.SLVK', 'L', vk24=slvk)
    precon = slvk(2)
    leliml = slvk(13)(1:3).eq.'OUI'
!
    if ((precon.ne.'ML') .and. (precon.ne.'BOOMER') .and. (precon.ne.'GAMG') ) then
        tbloc = 1
    endif
!
!   -- Si ELIM_LAGR='OUI', comme on va eliminer certains ddls,
!      il n'est pas sur que tbloc soit valide
    if (leliml) tbloc=1
!
    call jedema()
!
#endif
!
end subroutine
