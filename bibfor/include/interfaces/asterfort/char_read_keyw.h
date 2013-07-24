!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
interface
    subroutine char_read_keyw(keywordfact, iocc , val_type, n_keyexcl, keywordexcl,  &
                              n_max_keyword, n_keyword , keywordlist, val_nb, val_r, &
                              val_f, val_c)
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
    end subroutine char_read_keyw
end interface
