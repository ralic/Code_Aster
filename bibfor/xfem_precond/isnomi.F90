function isnomi(elrefa, ino)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!-----------------------------------------------------------------------
! BUT : VERIFIER S IL S AGIT D UN NOEUD MILIEU
!-----------------------------------------------------------------------
!
! ARGUMENTS :
!------------
!   - LSN
!-----------------------------------------------------------------------
    implicit none
!-----------------------------------------------------------------------
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/assert.h"
!-----------------------------------------------------------------------
    character(len=8) :: elrefa
    aster_logical :: isnomi
    integer :: ino
!-----------------------------------------------------------------------
    integer :: nnos
!-----------------------------------------------------------------------
!
    isnomi=.false.
!
    if (elrefa.eq.'HE8'.or.elrefa.eq.'H20'.or.elrefa.eq.'H27') then
      nnos=8
      goto 100
    else if (elrefa.eq.'TE4'.or.elrefa.eq.'T10') then
      nnos=4
      goto 100
    else if (elrefa.eq.'PE6'.or.elrefa.eq.'P15'.or.elrefa.eq.'P18') then
      nnos=6
      goto 100
    else if (elrefa.eq.'PY5'.or.elrefa.eq.'P13') then
      nnos=5
      goto 100
    else if (elrefa.eq.'TR3'.or.elrefa.eq.'TR6'.or.elrefa.eq.'TR7') then
      nnos=3
      goto 100
    else if (elrefa.eq.'QU4'.or.elrefa.eq.'QU8'.or.elrefa.eq.'QU9') then
      nnos=4
      goto 100
    else if (elrefa.eq.'SE2'.or.elrefa.eq.'SE3'.or.elrefa.eq.'SE4') then
      nnos=2
      goto 100
    else if (elrefa.eq.'PO1') then
      nnos=1
      goto 100
    else
      ASSERT(.false.)
    endif
!
100 continue
!
    if (ino.gt.nnos) isnomi=.true.
!
end function
