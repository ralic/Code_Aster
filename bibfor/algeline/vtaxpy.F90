subroutine vtaxpy(alpha, chamna, chamnb)
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
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  ENCAPSULATION DAXPY SUR LES .VALE DES CHAM_NO
!                          CHAMN1 ET CHAMN2
!                       CHAMN2.VALE = ALPHA * CHAMN1.VALE + CHAMN2.VALE
!     ------------------------------------------------------------------
!     IN  ALPHA     :  R8  : COEFF. MULTIPLICATEUR
!     IN  CHAMNA    :  K*  : CHAM_NO MAITRE 1
!     IN/OUT CHAMNB :  K*  : CHAM_NO MAITRE 2
!----------------------------------------------------------------------
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "blas/daxpy.h"
    character(len=*) :: chamna, chamnb
    real(kind=8) :: alpha
!
!
    integer :: neq1, neq2, ival1, ival2
    character(len=24) :: kval1, kval2, chamn1, chamn2
!
    call jemarq()
    chamn1=chamna
    chamn2=chamnb
!
    kval1=chamn1(1:19)//'.VALE'
    kval2=chamn2(1:19)//'.VALE'
    call jeveuo(kval1, 'L', ival1)
    call jeveuo(kval2, 'E', ival2)
    call jelira(kval1, 'LONMAX', neq1)
    call jelira(kval2, 'LONMAX', neq2)
    ASSERT(neq1.eq.neq2)
    call daxpy(neq2, alpha, zr(ival1), 1, zr(ival2),&
               1)
!
!
    call jedema()
end subroutine
