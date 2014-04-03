subroutine rccoma(jmat, mater_typez, iarret, mater_keyword, icodre)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer, intent(in) :: jmat
    character(len=*), intent(in) :: mater_typez
    integer, intent(in) :: iarret
    character(len=*), intent(out) :: mater_keyword
    integer, optional, intent(out) :: icodre
!
! --------------------------------------------------------------------------------------------------
!
! Comportment utility
!
! Get material keyword factor from type of material parameter
!
! --------------------------------------------------------------------------------------------------
!
! Example:
!      mater_type/mater_keyword
!      'ELAS' -> / 'ELAS'
!                / 'ELAS_ISTR'
!                / 'ELAS_GONF'
!                / ...
!      'ECRO' -> / 'ECRO_PUIS'
!                / 'ECRO_LINE'
!
! In  jmat          : adress to material parameters
! In  mater_type    : type of material parameter
! In  iarret        : 0 to set return code ICODRE and no error message
!                     1 stop and error message
! Out mater_keyword : keyword factor linked to type of material parameter
! Out icodre        : 0 everything is OK
!                     1 no mater_keyword found
!                     2 several mater_keyword found
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbmat, im, imat, icomp, ind, icodre_in
    character(len=16) :: mater_type
!
! --------------------------------------------------------------------------------------------------
!
    ASSERT(jmat.ne.1)
    ASSERT((iarret.eq.0) .or. (iarret.eq.1))
!
    mater_type = mater_typez
    ind = index(mater_type,'_FO')
    ASSERT(ind.eq.0)
    if (iarret.eq.0) then
        ASSERT((present(icodre)))
    endif
!
    icodre_in = 1
    mater_keyword = ' '
    nbmat    = zi(jmat)
    do im = 1, nbmat
        imat = jmat+zi(jmat+nbmat+im)
        do icomp = 1, zi(imat+1)
            if (mater_type .eq. zk16(zi(imat)+icomp-1)(1:len(mater_typez))) then
                if (mater_keyword .eq. ' ') then
                    mater_keyword=zk16(zi(imat)+icomp-1)
                    icodre_in = 0
                else
                    if (iarret .eq. 1) then
                        call utmess('F', 'COMPOR5_56', sk=mater_type)
                    else
                        icodre_in = 2
                    endif
                endif
            endif
        end do
    end do
!
    if (( icodre_in .eq. 1 ) .and. ( iarret .eq. 1 )) then
        call utmess('F', 'COMPOR5_57', sk=mater_type)
    endif
    if (present(icodre)) then
        icodre = icodre_in
    endif
!
end subroutine
