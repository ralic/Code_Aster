subroutine conlag(matasz, cond)
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
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
!
    real(kind=8) :: cond
    character(len=*) :: matasz
!
! ----------------------------------------------------------------------
!
! RECUPERATION DU CONDITIONNEMENT DES LAGRANGES D'UNE MATRICE ASSEMBLEE
!
! ----------------------------------------------------------------------
!
! IN   MATASZ : SD MATRICE ASSEMBLEE
! OUT  COND   : CONDITIONNEMENT DES LAGRANGES
!
!
!
!
!
    integer :: jconl, neq, iret, jcol
    character(len=8) :: k8bid
    character(len=19) :: matass
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    matass=matasz
    cond=1.d0
!
! ---  on sort des que l'on trouve un conditionnement de lagrange
    call jeexin(matass//'.CONL', iret)
    if (iret .ne. 0) then
        call dismoi('F', 'NB_EQUA', matass, 'MATR_ASSE', neq,&
                    k8bid, iret)
        call jeveuo(matass//'.CONL', 'L', jconl)
        do jcol = 1, neq
            cond = 1.d0/zr(jconl-1+jcol)
            if (cond .ne. 1.d0) goto 999
        end do
    endif
!
999 continue
!
    call jedema()
end subroutine
