subroutine as_mfdonp(fid, cha, numdt, numo, typent,&
                     typgeo, iterma, noma, nompro, nomloc,&
                     n, cret)
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
#include "asterf_types.h"
#include "asterf.h"
#include "asterfort/utmess.h"
#include "med/mfdonp.h"
    aster_int :: fid, typent, typgeo, n, cret, numdt, numo, iterma
    character(len=*) :: nompro, nomloc, cha, noma
#ifdef _DISABLE_MED
    call utmess('F', 'FERMETUR_2')
#else
!
#if med_int_kind != aster_int_kind
    med_int :: fid4, typen4, typge4, n4, cret4, numdt4, numo4, iterm4
    fid4 = fid
    numdt4 = numdt
    numo4 = numo
    typen4 = typent
    typge4 = typgeo
    iterm4 = iterma
    call mfdonp(fid4, cha, numdt4, numo4, typen4,&
                typge4, iterm4, noma, nompro, nomloc,&
                n4, cret4)
    n = n4
    cret = cret4
#else
    call mfdonp(fid, cha, numdt, numo, typent,&
                typgeo, iterma, noma, nompro, nomloc,&
                n, cret)
#endif
!
#endif
end subroutine
