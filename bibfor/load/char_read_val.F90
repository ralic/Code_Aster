subroutine char_read_val(keywordfact, iocc, keyword_z, val_type, val_nb,&
                         val_r, val_f, val_c, val_t)
!
    implicit none
!
#include "asterc/getexm.h"
#include "asterfort/assert.h"
#include "asterfort/getvc8.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=16), intent(in) :: keywordfact
    integer, intent(in) :: iocc
    character(len=*), intent(in) :: keyword_z
    character(len=4), intent(in) :: val_type
    integer, intent(out) :: val_nb
    real(kind=8), intent(out) :: val_r
    character(len=8), intent(out) :: val_f
    complex(kind=8), intent(out) :: val_c
    character(len=16), intent(out) :: val_t
!
! --------------------------------------------------------------------------------------------------
!
! AFFE_CHAR_MECA
!
! Read keywords values
!
! --------------------------------------------------------------------------------------------------
!
! In  keywordfact   : factor keyword to read
! In  iocc          : factor keyword index in AFFE_CHAR_MECA
! In  val_type      : type of values (REEL, COMP, FONC or TEXT) in keyword
! In  keyword       : keyword to read
! Out val_nb        : number of values in keyword
! Out val_r         : values (if real) in keyword
! Out val_f         : names of function (if function) in keyword
! Out val_c         : values (if complex) in keyword
! Out val_t         : values (if text) in keyword
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: keyword
!
! --------------------------------------------------------------------------------------------------
!
    keyword = keyword_z
    val_r = 0.d0
    val_c = (0.d0,0.d0)
    val_f = ' '
    val_t = ' '
    val_nb = 0
!
    if (getexm(keywordfact,keyword) .eq. 0) goto 99
!
    if (val_type .eq. 'REEL') then
        call getvr8(keywordfact, keyword, iocc=iocc, scal=val_r, nbret=val_nb)
    else if (val_type .eq. 'FONC') then
        call getvid(keywordfact, keyword, iocc=iocc, scal=val_f, nbret=val_nb)
    else if (val_type .eq. 'COMP') then
        call getvc8(keywordfact, keyword, iocc=iocc, scal=val_c, nbret=val_nb)
    else if (val_type .eq. 'TEXT') then
        call getvtx(keywordfact, keyword, iocc=iocc, scal=val_t, nbret=val_nb)
    else
        ASSERT(.false.)
    endif
!
99  continue
!
end subroutine
