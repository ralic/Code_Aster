subroutine as_mfdonv(fid, cha, typent, typgeo, noma,&
                     numdt, numo, pit, nompro, stm,&
                     npr, nomloc, nip, n, cret)
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
#include "aster_types.h"
#include "asterf.h"
#include "asterfort/utmess.h"
#include "med/mfdonv.h"
    aster_int :: fid, typent, typgeo, stm, npr, nip, n, cret, numdt, numo, pit
    character(len=*) :: cha, nompro, nomloc, noma
#ifdef _DISABLE_MED
    call utmess('F', 'FERMETUR_2')
#else
!
#if med_int_kind != aster_int_kind
    med_int :: fid4, typen4, typge4, stm4, npr4, nip4, n4, cret4
    med_int :: numdt4, numo4, pit4
    fid4 = fid
    typen4 = typent
    typge4 = typgeo
    numdt4 = numdt
    numo4 = numo
    stm4 = stm
    pit4 = pit
    call mfdonv(fid4, cha, numdt4, numo4, typen4,&
                typge4, noma, pit4, stm4, nompro,&
                npr4, nomloc, nip4, n4, cret4)
    npr = npr4
    nip = nip4
    n = n4
    cret = cret4
#else
    call mfdonv(fid, cha, numdt, numo, typent,&
                typgeo, noma, pit, stm, nompro,&
                npr, nomloc, nip, n, cret)
#endif
!
#endif
end subroutine
