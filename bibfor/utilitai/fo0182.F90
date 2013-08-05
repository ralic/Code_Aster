subroutine fo0182(obstac, nbval, vale)
    implicit   none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lxlgut.h"
#include "asterfort/wkvect.h"
    integer :: nbval
    real(kind=8) :: vale(*)
    character(len=*) :: obstac
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     OPERATEUR "RECU_FONCTION"  MOT CLE "OBSTACLE"
!     ------------------------------------------------------------------
    integer :: i, lpro, lval
    character(len=19) :: nomfon
!     ------------------------------------------------------------------
!
    call jemarq()
!
    nomfon = obstac
!
!     --- REMPLISSAGE DU .PROL ---
!
    ASSERT(lxlgut(nomfon).le.24)
    call wkvect(nomfon//'.PROL', 'G V K24', 6, lpro)
    zk24(lpro) = 'FONCTION'
    zk24(lpro+1) = 'LIN LIN '
    zk24(lpro+2) = 'THETA   '
    zk24(lpro+3) = 'R       '
    zk24(lpro+4) = 'EE      '
    zk24(lpro+5) = nomfon
!
!     --- REMPLISSAGE DU .VALE ---
!
    call wkvect(nomfon//'.VALE', 'G V R8', 2*nbval, lval)
!
    do 10 i = 1, nbval
        zr(lval+i-1) = vale(2*i-1)
        zr(lval+nbval+i-1) = vale(2*i)
10  end do
!
    call jedema()
end subroutine
