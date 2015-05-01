subroutine vlaxpy(alpha, chamna, chamnb)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    character(len=*) :: chamna, chamnb
    real(kind=8) :: alpha
!-----------------------------------------------------------------------
!    - FONCTION REALISEE:  ENCAPSULATION DAXPY SUR LES .VALE DES CHAM_NO
!                    CHAMN1 ET CHAMN2 UNIQUEMENT SUR LES DDL DE LAGRANGE
!                       CHAMN2.VALE = ALPHA * CHAMN1.VALE + CHAMN2.VALE
!     ------------------------------------------------------------------
!     IN  ALPHA     :  R8  : COEFF. MULTIPLICATEUR
!     IN  CHAMNA    :  K*  : CHAM_NO MAITRE 1
!     IN/OUT CHAMNB :  K*  : CHAM_NO MAITRE 2
!----------------------------------------------------------------------
    integer :: neq,   i
    character(len=19) :: prno
    character(len=24) :: chamn1, chamn2
    integer, pointer :: delg(:) => null()
    real(kind=8), pointer :: val1(:) => null()
    real(kind=8), pointer :: val2(:) => null()
!----------------------------------------------------------------------
!
    call jemarq()
    chamn1=chamna
    chamn2=chamnb
!
! --- NUMEROTATION POUR TRIER LES LAGRANGE ET LES DDLS PHYSIQUES
    call dismoi('PROF_CHNO', chamn1, 'CHAM_NO', repk=prno)
    call jeveuo(prno(1:14)// '.NUME.DELG', 'L', vi=delg)
!
!
! --- MISE A JOUR DES VALEURS DES LAGRANGE
    call jeveuo(chamn1(1:19)//'.VALE', 'L', vr=val1)
    call jeveuo(chamn2(1:19)//'.VALE', 'E', vr=val2)
    call jelira(chamn2(1:19)//'.VALE', 'LONMAX', neq)
    do i = 1, neq
        if (delg(i) .ne. 0) val2(i)=alpha*val1(i) + val2(i)
    end do
!
    call jedema()
end subroutine
