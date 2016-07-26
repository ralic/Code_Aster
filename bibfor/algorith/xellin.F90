subroutine xellin(elref1, nno1, elref2, nno2)
    implicit none
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/iselli.h"
    character(len=8) :: elref1, elref2
    integer :: nno1, nno2
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!                      RETOURNE LE TYPE DE L'ELEMENT "LINEARISE"
!                      ET LE NOMBRE DE NOEUDS DE CHAQUE ELEMENT
!
!     ENTREE
!       ELREF1  : TYPE DE L'ELEMENT PARENT
!       NNO1    : NOMBRE DE NOEUDS DE L'ELEMENT PARENT
!
!     SORTIE
!       ELREF2  : TYPE DE L'ELEMENT LINEAIRE A L'ELEMENT PARENT

!       NNO2    : NOMBRE DE NOEUDS DE L'ELEMENT LINEAIRE
!......................................................................
!
    call jemarq()
!
    if (iselli(elref1)) goto 999
!
    if (elref1 .eq. 'QU8') then
            ASSERT(nno1.eq.8)
            elref2='QU4'
            nno2= 4
    else if (elref1.eq.'TR6') then
            ASSERT(nno1.eq.6)
            elref2='TR3'
            nno2= 3
    else if (elref1.eq.'SE3') then
            ASSERT(nno1.eq.3)
            elref2='SE2'
            nno2= 2
    else if (elref1.eq.'H20') then
            ASSERT(nno1.eq.20)
            elref2='HE8'
            nno2= 8
    else if (elref1.eq.'P15') then
            ASSERT(nno1.eq.15)
            elref2='PE6'
            nno2= 6
    else if (elref1.eq.'P13') then
            ASSERT(nno1.eq.13)
            elref2='PY5'
            nno2= 5
    else if (elref1.eq.'T10') then
            ASSERT(nno1.eq.10)
            elref2='TE4'
            nno2= 4
    else
        ASSERT(.false.)
    endif
!
999 continue
!
    call jedema()
!
end subroutine
