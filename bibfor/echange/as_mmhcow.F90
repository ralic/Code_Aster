subroutine as_mmhcow(fid, maa, coo, modcoo, n,&
                  cret)
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
#   include "med/mmhcow.h"
    character(len=*) :: maa
    real(kind=8) :: coo(*)
    ast_int :: fid
    ast_int :: n, cret, modcoo, mdnont, mdnoit
    real(kind=8) :: mdnodt
#ifdef _DISABLE_MED
    call u2mess('F', 'FERMETUR_2')
#else

#if med_int_kind != ast_int_kind
    med_int :: fid4
    med_int :: n4, cret4, modco4, mdnoi4, mdnon4
    mdnont = -1
    mdnoit = -1
    mdnodt = -1.d0
    fid4 = fid
    n4 = n
    modco4 = modcoo
    mdnon4 = mdnont
    mdnoi4 = mdnoit
    call mmhcow(fid4, maa, mdnon4, mdnoi4, mdnodt,&
                modco4, n4, coo, cret4)
    cret = cret4
#else
    mdnont = -1
    mdnoit = -1
    mdnodt = -1.d0
    call mmhcow(fid, maa, mdnont, mdnoit, mdnodt,&
                modcoo, n, coo, cret)
#endif

#endif
end subroutine
