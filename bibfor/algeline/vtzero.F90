subroutine vtzero(chamna)
!-----------------------------------------------------------------------
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
!    - fonction realisee:  initialisation a zero du .vale du cham_no chamna
!     ------------------------------------------------------------------
!     IN  CHAMNA  :  K* : CHAM_NO
!----------------------------------------------------------------------
    implicit none
!
! DECLARATION PARAMETRES D'APPELS
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=*) :: chamna
!
!
! DECLARATION VARIABLES LOCALES
    integer :: neq, ival, i, neq1
    character(len=24) :: kval, chamn
!
! CORPS DU PROGRAMME
    call jemarq()
    chamn=chamna
!
!
    kval=chamn(1:19)//'.VALE'
    call jeveuo(kval, 'E', ival)
    call jelira(kval, 'LONMAX', neq)
    neq1=neq-1
    do i = 0, neq1
        zr(ival+i)=0.d0
    end do
!
!
!
    call jedema()
end subroutine
