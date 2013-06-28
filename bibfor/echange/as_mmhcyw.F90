subroutine as_mmhcyw(fid, maa, conn, csize, switch,&
                  n, typent, typgeo, typcon, cret)
! person_in_charge: nicolas.sellenet at edf.fr
!     L'ARGUMENT CSIZE N'EST PAS DANS L'API MED
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
#   include "med/mmhcyw.h"
#   include "asterfort/conv_int.h"
    character(len=*) :: maa
    ast_int :: fid, conn(*), csize, typent, typgeo, typcon, cret
    ast_int :: n, switch, mdnont, mdnoit
    real(kind=8) :: mdnodt
#ifdef _DISABLE_MED
    call u2mess('F', 'FERMETUR_2')
#else

#if med_int_kind != ast_int_kind
    med_int, ALLOCATABLE :: conn4(:)
    med_int :: fid4, typen4, typge4, typco4, cret4
    med_int :: n4, switc4, mdnon4, mdnoi4
    ast_int :: ic
    mdnont = -1
    mdnoit = -1
    mdnodt = -1.d0
    fid4 = fid
    allocate ( conn4(csize) )
    call conv_int('ast->med', csize, vi_ast=conn, vi_med=conn4)
    typen4 = typent
    typge4 = typgeo
    typco4 = typcon
    n4 = n
    switc4 = switch
    mdnon4 = mdnont
    mdnoi4 = mdnoit
    write(6,*) 'CONN=',(conn(ic),ic=1,min(12,csize))
    write(6,*)'CONN4=',(conn4(ic),ic=1,min(12,csize))
    call mmhcyw(fid4, maa, mdnon4, mdnoi4, mdnodt,&
                typen4, typge4, typco4, switc4, n4,&
                conn4, cret4)
    cret = cret4
    deallocate (conn4)
#else
    mdnont = -1
    mdnoit = -1
    mdnodt = -1.d0
    call mmhcyw(fid, maa, mdnont, mdnoit, mdnodt,&
                typent, typgeo, typcon, switch, n,&
                conn, cret)
#endif

#endif
end subroutine
