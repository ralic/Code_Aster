subroutine as_mfaofi(fid, maa, ind, fam, num,&
                  attid, attval, attdes, natt, gro,&
                  cret)
! person_in_charge: nicolas.sellenet at edf.fr
!     l'argument natt est en "plus"
!     il sert a dimensionner attid4(*) et attva4(*)
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
#   include "med/mfaofi.h"
#   include "asterfort/conv_int.h"

ast_int :: fid, num, attid(*), attval(*), natt, cret, ind
character(len=*) :: maa, fam, attdes(*) , gro(*)

#ifdef _DISABLE_MED
    call u2mess('F', 'FERMETUR_2')
#else

#if med_int_kind != ast_int_kind
    med_int, allocatable :: attid4(:), attva4(:)
    med_int :: cret4,num4

    allocate ( attid4(natt) )
    allocate ( attva4(natt) )

    call mfaofi(to_med_int(fid), maa, to_med_int(ind), fam, attid4,&
                attva4, attdes, num4, gro, cret4)
    num  = to_ast_int(num4)
    cret = to_ast_int(cret4)
    call conv_int('med->ast', natt, vi_ast=attid, vi_med=attid4)
    call conv_int('med->ast', natt, vi_ast=attval, vi_med=attva4)

    deallocate (attid4)
    deallocate (attva4)
#else
    call mfaofi(fid, maa, ind, fam, attid,&
                attval, attdes, num, gro, cret)
#endif

#endif
end subroutine
