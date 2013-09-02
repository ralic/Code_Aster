subroutine char_read_keyw(keywordfact, iocc , val_type, n_keyexcl, keywordexcl,  &
                          n_max_keyword, n_keyword , keywordlist, val_nb, val_r, &
                          val_f, val_c)
!
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getmjm.h"
#include "asterfort/assert.h"
#include "asterfort/char_read_val.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
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
    integer, intent(in)  :: iocc
    character(len=4), intent(in) :: val_type
    character(len=24), intent(in) :: keywordexcl
    integer, intent(in)  :: n_keyexcl
    integer, intent(in)  :: n_max_keyword
    integer, intent(out) :: n_keyword
    character(len=16), intent(out) :: keywordlist(n_max_keyword)
    integer, intent(out) :: val_nb(n_max_keyword)
    real(kind=8), intent(out) :: val_r(n_max_keyword)
    character(len=8), intent(out) :: val_f(n_max_keyword)
    complex(kind=8), intent(out) :: val_c(n_max_keyword)
!
! --------------------------------------------------------------------------------------------------
!
! AFFE_CHAR_MECA
!
! Read keywords and their values except someones give in keywordexcl object
!
! --------------------------------------------------------------------------------------------------
!
! In  keywordfact   : factor keyword to read
! In  iocc          : factor keyword index in AFFE_CHAR_MECA
! In  val_type      : type of values (REEL, COMP or FONC) in keyword
! In  keywordexcl   : name of JEVEUX object for excluded keywords
! In  n_keyexcl     : number of excluded keywords
! In  n_max_keyword : maximum number of keywords can been read
! Out n_keyword     : number of keywords has been read
! Out keywordlist   : list of keywords has been read
! Out val_nb        : number of values in keyword
! Out val_r         : values (if real) in keyword
! Out val_f         : names of function (if function) in keyword
! Out val_c         : values (if complex) in keyword
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: keywordread(300)
    character(len=16) :: k16_dummy(300), keyword, val_t_dummy
    integer :: n, i_keyword, i_keyexcl
    logical :: l_excl
    integer :: j_kexcl
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    ASSERT(n_max_keyword.eq.300)
!
! - Initial number of keywords
!
    call getmjm(keywordfact, iocc, 0, keywordread, k16_dummy, n)
    n_keyword = -n
    ASSERT(n_keyword .le. n_max_keyword)
!
! - Read all keywords
!
    call getmjm(keywordfact, iocc, n_keyword, keywordread, k16_dummy, n)
!
! - Excluding some keywords
!
    if (n_keyexcl.ne.0) call jeveuo(keywordexcl,'L',j_kexcl)
    n = n_keyword
    n_keyword = 0
    do i_keyword = 1, n
        l_excl = .false.
        keyword = keywordread(i_keyword)
        do i_keyexcl = 1, n_keyexcl
            if (keyword.eq.zk24(j_kexcl-1+i_keyexcl))  then
                l_excl = .true.
            endif
        enddo
        if (.not.l_excl) then
           n_keyword = n_keyword + 1
           keywordlist(n_keyword) = keyword
        endif
    end do
!
! - Values for final keywords
!
    do i_keyword = 1, n_keyword
        keyword = keywordlist(i_keyword)
        call char_read_val(keywordfact, iocc, keyword, val_type, val_nb(i_keyword), &
                           val_r(i_keyword), val_f(i_keyword), val_c(i_keyword), val_t_dummy)
        ASSERT(val_nb(i_keyword).eq.1)
    end do
!
    call jedema()
end subroutine
