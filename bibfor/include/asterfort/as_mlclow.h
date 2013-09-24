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
#include "aster_types.h"
interface
    subroutine as_mlclow(fid, typgeo, refcoo, modeco, ngauss,&
                      gscoo, wg, locname, ndim, nomasu,&
                      cret)
        aster_int :: fid
        aster_int :: typgeo
        real(kind=8) :: refcoo(*)
        aster_int :: modeco
        aster_int :: ngauss
        real(kind=8) :: gscoo(*)
        real(kind=8) :: wg(*)
        character(len=*) :: locname
        aster_int :: ndim
        character(len=*) :: nomasu
        aster_int :: cret
    end subroutine as_mlclow
end interface
