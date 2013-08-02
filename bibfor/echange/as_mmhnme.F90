subroutine as_mmhnme(fid, maa, quoi, typent, typgeo,&
                  typcon, n, cret)
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
#include "asterf.h"
#include "aster_types.h"
#include "med/mmhnme.h"
    character(len=*) :: maa
    aster_int :: fid, typent, typgeo, cret, typcon, n, quoi, mdnont, mdnoit
    aster_int :: chtseq, chttra
#ifdef _DISABLE_MED
    call u2mess('F', 'FERMETUR_2')
#else

#if med_int_kind != aster_int_kind
    med_int :: fid4, typen4, typge4, cret4, typco4, n4, quoi4
    med_int :: mdnon4, mdnoi4, chtse4, chttr4
    mdnont = -1
    mdnoit = -1
    fid4 = fid
    typen4 = typent
    typge4 = typgeo
    typco4 = typcon
    quoi4 = quoi
    mdnon4 = mdnont
    mdnoi4 = mdnoit
    call mmhnme(fid4, maa, mdnon4, mdnoi4, typen4,&
                typge4, quoi4, typco4, chtse4, chttr4,&
                n4, cret4)
    n = n4
    cret = cret4
#else
    mdnont = -1
    mdnoit = -1
    call mmhnme(fid, maa, mdnont, mdnoit, typent,&
                typgeo, quoi, typcon, chtseq, chttra,&
                n, cret)
#endif

#endif
end subroutine
