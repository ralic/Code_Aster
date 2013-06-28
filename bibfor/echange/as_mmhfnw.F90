subroutine as_mmhfnw(fid, maa, fam, n, typent,&
                  typgeo, cret)
! person_in_charge: nicolas.sellenet at edf.fr
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
#   include "asterfort/conv_int.h"
#   include "med/mmhfnw.h"
    ast_int :: fid, fam(*), n, typent, typgeo, cret, mdnont, mdnoit
    character(len=*) :: maa
#ifdef _DISABLE_MED
    call u2mess('F', 'FERMETUR_2')
#else

#if med_int_kind != ast_int_kind
    med_int :: fid4, n4, typen4, typge4, cret4
    med_int :: mdnon4, mdnoi4
    med_int, ALLOCATABLE :: fam4(:)
    mdnont = -1
    mdnoit = -1
    fid4 = fid
    allocate ( fam4(n) )
    call conv_int('ast->med', n, vi_ast=fam, vi_med=fam4)
    n4 = n
    typen4 = typent
    typge4 = typgeo
    mdnon4 = mdnont
    mdnoi4 = mdnoit
    call mmhfnw(fid4, maa, mdnon4, mdnoi4, typen4,&
                typge4, n4, fam4, cret4)
    cret = cret4
    deallocate (fam4)
#else
    mdnont = -1
    mdnoit = -1
    call mmhfnw(fid, maa, mdnont, mdnoit, typent,&
                typgeo, n, fam, cret)
#endif

#endif
end subroutine
