subroutine char_read_vect(keywordfact, iocc, keyword_z, vect_r)
!
    implicit none
!
#include "asterc/getexm.h"
#include "asterfort/assert.h"
#include "asterfort/getvr8.h"
#include "asterfort/u2mess.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=16), intent(in) :: keywordfact
    integer, intent(in) :: iocc
    character(len=*), intent(in) :: keyword_z
    real(kind=8), intent(out) :: vect_r(3)
!
! --------------------------------------------------------------------------------------------------
!
! AFFE_CHAR_MECA
!
! Read keywords values for a vector
!
! --------------------------------------------------------------------------------------------------
!
! In  keywordfact   : factor keyword to read
! In  iocc          : factor keyword index in AFFE_CHAR_MECA
! In  keyword       : keyword to read
! Out vect_r        : vector in keyword
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iarg, val_nb, ibid
    character(len=16) :: keyword
    real(kind=8) :: r8dummy
!
! --------------------------------------------------------------------------------------------------
!
    keyword = keyword_z
    vect_r(1) = 0.d0
    vect_r(2) = 0.d0
    vect_r(3) = 0.d0
!
    if (getexm(keywordfact,keyword) .eq. 0) goto 99
!
    call getvr8(keywordfact, keyword, iocc=iocc, nbval=0, nbret=val_nb)
    val_nb = -val_nb
    if (val_nb .eq. 0) goto 99
    ASSERT(val_nb.le.3)
    call getvr8(keywordfact, keyword, iocc=iocc, nbval=val_nb, vect=vect_r,&
                nbret=ibid)
!
99  continue
!
end subroutine
