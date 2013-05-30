subroutine mfnpdt(fid, cha, ma, n, cunit,&
                  cname, cret)
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
    include 'med/mfdfin.h'
    integer :: fid, n, cret, lmesh, typen
    character(len=*) :: cha
    character(len=16) :: cunit(*), cname(*)
    character(len=*) :: ma
    character(len=80) :: dtunit
#ifdef _DISABLE_MED
    call u2mess('F', 'FERMETUR_2')
#else
! BEGIN MED
#ifdef _USE_MED_SHORT_INT
    integer(kind=4) :: fid4, n4, cret4, lmesh4, typen4
#ifdef _DEBUG_MED
    print *,'=== MFNPDT ==='
#endif
    fid4 = fid
    call mfdfin(fid4, cha, ma, lmesh4, typen4,&
                cunit, cname, dtunit, n4, cret4)
    n = n4
    cret = cret4
#else
    call mfdfin(fid, cha, ma, lmesh, typen,&
                cunit, cname, dtunit, n, cret)
#endif
! END MED
#endif
end subroutine
