subroutine conv_int(sens, nb, vi_ast, vi_med)
! ======================================================================
! COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!     utilitaire de conversion de tableaux d'entiers :
!     aster_int <---> ast_med
!
!     sens= / 'ast->med'
!           / 'med->ast'
!     nb : nombre de valeurs dans les tableaux vi_ast et vi_med
!
    implicit none
#include "asterf_types.h"
#include "asterfort/assert.h"
    character(len=*) :: sens
    aster_int :: nb
    aster_int :: vi_ast(nb)
    med_int :: vi_med(nb)
    integer :: i

    if (sens.eq.'ast->med') then
        do  i = 1, nb
            vi_med(i) = to_med_int(vi_ast(i))
        enddo

    elseif (sens.eq.'med->ast') then
        do  i = 1, nb
            vi_ast(i) = to_aster_int(vi_med(i))
        enddo

    else
        ASSERT(.false.)
    endif
end subroutine
