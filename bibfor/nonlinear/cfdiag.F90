subroutine cfdiag(lmat, xmax)
!
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mtdsc2.h"
#include "asterfort/utmess.h"
!
    integer :: lmat
    real(kind=8) :: xmax
!
! VALEUR MAXI SUR LA DIAGONALE DE LA MATR_ASSE DU SYSTEME MECANIQUE
! UTILISE POUR LA DETECTION DE PIVOT NUL
!
! IN  LMAT   : DESCRIPTEUR DE LA MATR_ASSE DU SYSTEME MECANIQUE
! OUT XMAX   : VALEUR DU PIVOT MAX
!
!
!
!
    integer :: jsmdi, jvalm, ieq, iadia, neq
    character(len=19) :: mat
    character(len=4) :: kmpic
! ----------------------------------------------------------------------
!
!
    call jemarq()
    xmax = 0.d0
    mat = zk24(zi(lmat+1))
    call mtdsc2(mat, 'SMDI', 'L', jsmdi)
    call dismoi('MPI_COMPLET', mat, 'MATR_ASSE', repk=kmpic)
    if (kmpic .ne. 'OUI') then
        call utmess('F', 'CALCULEL6_54')
    endif
    call jeveuo(jexnum(mat//'.VALM', 1), 'L', jvalm)
!
    neq=zi(lmat+2)
    do ieq = 1, neq
        iadia = jvalm + zi(jsmdi+ieq-1) - 1
        xmax = max ( xmax , zr(iadia) )
    end do
    call jedema()
end subroutine
