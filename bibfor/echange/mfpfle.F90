subroutine mfpfle(fid, pflval, nbval, pro, cret)
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
    include 'med/mpfprw.h'
    integer :: fid, nbval, cret
    integer :: pflval(*)
    character(len=*) :: pro
#ifdef _DISABLE_MED
    call u2mess('F', 'FERMETUR_2')
#else
! BEGIN MED
#ifdef _USE_MED_SHORT_INT
    integer(kind=4) :: fid4, nbval4, cret4
    integer(kind=4), ALLOCATABLE :: pflva4(:)
#ifdef _DEBUG_MED
    print *,'=== MFPFLE ==='
#endif
    fid4 = fid
    nbval4 = nbval
    allocate ( pflva4(nbval) )
    call convi4(pflval, pflva4, nbval)
    call mpfprw(fid4, pro, nbval4, pflva4, cret4)
    cret = cret4
    deallocate (pflva4)
#else
    call mpfprw(fid, pro, nbval, pflval, cret)
#endif
! END MED
#endif
end subroutine
