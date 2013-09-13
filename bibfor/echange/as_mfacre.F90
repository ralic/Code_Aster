subroutine as_mfacre(fid, maa, fam, num, ngro,&
                     gro, cret)
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
#include "med/mfacre.h"
    aster_int :: fid, num, ngro, cret
    character(len=*) :: maa, fam
    character(len=80) :: gro(*)
#ifdef _DISABLE_MED
    call utmess('F', 'FERMETUR_2')
#else
!
#if med_int_kind != aster_int_kind
    med_int :: fid4, num4, ngro4, cret4
    fid4 = fid
    num4 = num
    ngro4 = ngro
    call mfacre(fid4, maa, fam, num4, ngro4,&
                gro, cret4)
    cret = cret4
#else
    call mfacre(fid, maa, fam, num, ngro,&
                gro, cret)
#endif
!
#endif
end subroutine
