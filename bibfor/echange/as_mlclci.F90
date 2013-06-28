subroutine as_mlclci(fid, nordr, k64, ityp, nbn,&
                  ndim, nomasu, cret)
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
#   include "med/mlclci.h"
    ast_int :: fid, nordr, ityp, nbn, cret, ndim, tymasu, nbmasu
    character(len=64) :: k64, giname
    character(len=*) :: nomasu
#ifdef _DISABLE_MED
    call u2mess('F', 'FERMETUR_2')
#else

#if med_int_kind != ast_int_kind
    med_int :: fid4, nordr4, ityp4, nbn4, cret4, ndim4, tymas4, nbmas4
    fid4 = fid
    nordr4 = nordr
    call mlclci(fid4, nordr4, k64, ityp4, ndim4,&
                nbn4, giname, nomasu, nbmas4, tymas4,&
                cret4)
    ityp = ityp4
    nbn = nbn4
    cret = cret4
    ndim = ndim4
    nbmasu = nbmas4
#else
    call mlclci(fid, nordr, k64, ityp, ndim,&
                nbn, giname, nomasu, nbmasu, tymasu,&
                cret)
#endif

#endif
end subroutine
