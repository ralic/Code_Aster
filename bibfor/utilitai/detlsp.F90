subroutine detlsp(matasz, solvez)
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
    implicit      none
#include "jeveux.h"
#include "asterfort/amumph.h"
#include "asterfort/crsmsp.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=*) :: matasz, solvez
!
! ----------------------------------------------------------------------
!
!  DETRUIRE LES INSTANCES MUMPS DU PRECONDITIONNEUR LDLT_SP
!  ***                                              *    **
!
! ----------------------------------------------------------------------
!
! IN  MATASS : MATRICE ASSEMBLEE
! IN  SOLVEU : SD SOLVEUR
!
!
!
!
    character(len=19) :: solveu, matass
    character(len=24) :: metres, precon, solvbd
    integer :: jslvk, iret
    real(kind=8) :: r8bid
    complex(kind=8) :: c16bid
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    solveu = solvez
    matass = matasz
!
    call jeveuo(solveu//'.SLVK', 'L', jslvk)
    metres = zk24(jslvk-1+1)
    if (metres .eq. 'PETSC' .or. metres .eq. 'GCPC') then
        precon = zk24(jslvk-1+2)
        if (precon .eq. 'LDLT_SP') then
            solvbd = zk24(jslvk-1+3)
            call crsmsp(solvbd, matass, 0)
            call amumph('DETR_MAT', solvbd, matass, r8bid, c16bid,&
                        ' ', 0, iret, .true.)
            call detrsd('SOLVEUR', solvbd)
        endif
    endif
!
    call jedema()
!
end subroutine
