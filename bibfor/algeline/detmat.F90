subroutine detmat()
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
    implicit none
! ----------------------------------------------------------------------
!
! BUT : DETRUIRE TOUTES LES MATR_ASSE PRESENTES SUR LA BASE VOLATILE
!       DETRUIT AUSSI LES EVENTUELLES INSTANCES MUMPS OU PETSC
!
! ----------------------------------------------------------------------
! person_in_charge: jacques.pellet at edf.fr
!
#include "asterfort/assert.h"
#include "asterfort/detlsp.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelstc.h"
    integer :: nbmat, i, ibid, ier
    character(len=19) :: matass, solveu
    character(len=24) :: lirefa(100)
!-----------------------------------------------------------------------
!
    call jelstc('V', '.REFA', 20, 100, lirefa,&
                nbmat)
    ASSERT(nbmat.ge.0)
!
    do i = 1, nbmat
        call jeexin(lirefa(i), ier)
        if (ier .eq. 0) goto 10
        matass = lirefa(i)(1:19)
!
!       -- on detruit l'eventuelle instance mumps associee a ldlt_sp
        call dismoi('C', 'SOLVEUR', matass, 'MATR_ASSE', ibid,&
                    solveu, ier)
        if (ier .eq. 0 .and. solveu(1:4) .ne. 'XXXX') then
            call detlsp(matass, solveu)
        endif
!
!       --  on detruit les matr_asse ainsi que les
!           eventuelles instances mumps et petsc
        call detrsd('MATR_ASSE', matass)
!
 10     continue
    end do
!
end subroutine
