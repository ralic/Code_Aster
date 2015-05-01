subroutine as_mmhenw(fid, maa, num, n, typent,&
                     typgeo, cret)
! person_in_charge: nicolas.sellenet at edf.fr
!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
#include "asterf.h"
#include "asterfort/conv_int.h"
#include "asterfort/utmess.h"
#include "med/mmhenw.h"
    character(len=*) :: maa
    aster_int :: num(*)
    aster_int :: fid, typent, typgeo, cret
    aster_int :: n, mdnont, mdnoit
#ifdef _DISABLE_MED
    call utmess('F', 'FERMETUR_2')
#else
!
#if med_int_kind != aster_int_kind
    med_int, allocatable :: num4(:)
    med_int :: fid4, typen4, typge4, cret4
    med_int :: n4, mdnon4, mdnoi4
    mdnont = -1
    mdnoit = -1
    allocate ( num4(n) )
    call conv_int('ast->med', n, vi_ast=num, vi_med=num4)
    fid4 = fid
    typen4 = typent
    typge4 = typgeo
    n4 = n
    mdnon4 = mdnont
    mdnoi4 = mdnoit
    call mmhenw(fid4, maa, mdnon4, mdnoi4, typen4,&
                typge4, n4, num4, cret4)
    cret = cret4
    deallocate (num4)
#else
    mdnont = -1
    mdnoit = -1
    call mmhenw(fid, maa, mdnont, mdnoit, typent,&
                typgeo, n, num, cret)
#endif
!
#endif
end subroutine
