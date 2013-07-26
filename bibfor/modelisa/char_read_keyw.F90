subroutine char_read_keyw(keywordfact, iocc , val_type, n_keyexcl, keywordexcl,  &
                          n_max_keyword, n_keyword , keywordlist, val_nb, val_r, val_f)
!
! aslint: disable=W1306
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getmjm.h"
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mesi.h"
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
    integer, intent(in) :: n_keyexcl
    character(len=16), intent(in) :: keywordexcl(n_keyexcl)
    integer, intent(in)  :: n_max_keyword
    integer, intent(out) :: n_keyword
    character(len=16), intent(out) :: keywordlist(n_max_keyword)
    integer, intent(out) :: val_nb(n_max_keyword)
    real(kind=8), intent(out) :: val_r(n_max_keyword)
    character(len=8), intent(out) :: val_f(n_max_keyword)
!
! --------------------------------------------------------------------------------------------------
!
! AFFE_CHAR_MECA
!
! Read keywords and their values except for affectation
!
! --------------------------------------------------------------------------------------------------
!
! In  keywordfact   : factor keyword to read
! In  iocc          : factor keyword index in AFFE_CHAR_MECA
! In  val_type      : type of values (REEL or FONC) in keyword
! In  n_keyexcl     : number of keyword excluded
! In  keywordexcl   : list of keyword excluded
! In  n_max_keyword : maximum number of keywords can been read
! Out n_keyword     : number of keywords has been read
! Out keywordlist   : list of keywords has been read
! Out val_nb        : number of values in keyword
! Out val_r         : values (if real) in keyword
! Out val_f         : names of function (if function) in keyword
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: keywordread(300)
    character(len=16) :: k16_dummy(300), keyword
    integer :: n, iarg, vali(2), i_keyword, i_keyexcl
    logical :: l_excl
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    call assert(n_max_keyword.eq.300)
!
! - Initial number of keywords
!
    call getmjm(keywordfact, iocc, 0, keywordread, k16_dummy, n)
    n_keyword = -n
    if (n_keyword .gt. n_max_keyword) then
        vali(1) = n_max_keyword
        vali(2) = n_keyword
        call u2mesi('F', 'MODELISA8_31',  2,vali)
    endif
!
! - Excluding some keywords
!
    call getmjm(keywordfact, iocc, n_keyword, keywordread, k16_dummy, n)
    n_keyword = 0
    do i_keyword = 1, n
        l_excl = .false.
        keyword = keywordread(i_keyword)
        do i_keyexcl = 1, n_keyexcl
            if (keyword.eq.keywordexcl(i_keyexcl))  then
                l_excl = .true.
            endif
        enddo
        write(6,*) 'keiword: ',keyword,l_excl
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
        if (val_type .eq. 'REEL') then
            call getvr8(keywordfact, keyword, iocc, iarg, 1,&
                        val_r(i_keyword), val_nb(i_keyword))
        elseif (val_type .eq. 'FONC') then
            call getvid(keywordfact, keyword, iocc, iarg, 1,&
                        val_f(i_keyword), val_nb(i_keyword))
        else
            call assert(.false.)
        endif
    end do
!
    call jedema()
end subroutine
