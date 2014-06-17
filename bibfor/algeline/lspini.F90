subroutine lspini(solveu)
    implicit none
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=19) :: solveu
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
! ----------------------------------------------------------
!  BUT : REINITIALISATION DU PRECONDITIONNEUR LDLT_SP POUR
!        PETSC OU GCPC
!
! IN K19 SOLVEU  : NOM DU SOLVEUR DONNE EN ENTREE
! ----------------------------------------------------------
!
!
!
!
    character(len=8) :: precon
    character(len=24), pointer :: slvk(:) => null()
    integer, pointer :: slvi(:) => null()
!
!------------------------------------------------------------------
    call jemarq()
!
! --- LECTURES PARAMETRES DU SOLVEUR
    call jeveuo(solveu//'.SLVK', 'L', vk24=slvk)
    precon = slvk(2)
!
! --- REMISE A ZERO
    if (precon .eq. 'LDLT_SP') then
        call jeveuo(solveu//'.SLVI', 'E', vi=slvi)
        slvi(5)=0
    endif
!
    call jedema()
end subroutine
