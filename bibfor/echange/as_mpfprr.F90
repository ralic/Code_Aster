subroutine as_mpfprr(fid, pflval, n, pro, cret)
! person_in_charge: nicolas.sellenet at edf.fr
!     L'ARGUMENT N N'EST PAS DANS L'API MED
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
    implicit none
#   include "types/aster_types.h"
#   include "types/med_types.h"
#   include "med/mpfprr.h"
#   include "asterfort/conv_int.h"
    ast_int :: fid, n, cret
    ast_int :: pflval(*)
    character(len=*) :: pro
#ifdef _DISABLE_MED
    call u2mess('F', 'FERMETUR_2')
#else

#if med_int_kind != ast_int_kind
    med_int :: fid4, cret4
    med_int, allocatable :: pflva4(:)
    fid4 = fid
    allocate ( pflva4(n) )
    call mpfprr(fid4, pro, pflva4, cret4)
    call conv_int('med->ast', n, vi_ast=pflval, vi_med=pflva4)
    cret = cret4
    deallocate (pflva4)
#else
    call mpfprr(fid, pro, pflval, cret)
#endif

#endif
end subroutine
