subroutine mfgaue(fid, typgeo, refcoo, modeco, ngauss,&
                  gscoo, wg, locname, ndim, nomasu,&
                  cret)
!           CONFIGURATION MANAGEMENT OF EDF VERSION
! person_in_charge: nicolas.sellenet at edf.fr
! ==================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D              WWW.CODE-ASTER.ORG
!
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR
! MODIFY IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS
! PUBLISHED BY THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE
! LICENSE, OR (AT YOUR OPTION) ANY LATER VERSION.
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,
! BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO : EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ==================================================================
    implicit none
    include 'med/mlclow.h'
    integer :: fid, typgeo, modeco, ngauss, cret, ndim
    real(kind=8) :: refcoo(*), gscoo(*), wg(*)
    character(len=*) :: locname, nomasu
#ifdef _DISABLE_MED
    call u2mess('F', 'FERMETUR_2')
#else
! BEGIN MED
#ifdef _USE_MED_SHORT_INT
    integer(kind=4) :: fid4, typge4, mode_4, ngaus4, cret4, ndim4
#ifdef _DEBUG_MED
    print *,'=== MFGAUE ==='
#endif
    fid4 = fid
    typge4 = typgeo
    mode_4 = modeco
    ngaus4 = ngauss
    ndim4 = ndim
    call mlclow(fid4, locname, typge4, ndim4, refcoo,&
                mode_4, ngaus4, gscoo, wg, '',&
                nomasu, cret4)
    cret = cret4
#else
    call mlclow(fid, locname, typgeo, ndim, refcoo,&
                modeco, ngauss, gscoo, wg, '',&
                nomasu, cret)
#endif
! END MED
#endif
end subroutine
