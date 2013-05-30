subroutine mffami(fid, maa, ind, fam, num,&
                  attid, attval, attdes, natt, gro,&
                  cret)
! person_in_charge: nicolas.sellenet at edf.fr
!     L'ARGUMENT NATT N'EST QUE "OUT" DANS L'API MED
!     ICI IL EST "IN" ET "OUT" POUR DIMENSIONNER ATTID4(*) ET ATTVA4(*)
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
    include 'med/mfaofi.h'
    integer :: fid, num, attid(*), attval(*), natt, cret, ind
    character(len=*) :: maa, fam, attdes(*), gro(*)
#ifdef _DISABLE_MED
    call u2mess('F', 'FERMETUR_2')
#else
! BEGIN MED
#ifdef _USE_MED_SHORT_INT
    integer(kind=4) :: fid4, num4
    integer(kind=4), ALLOCATABLE :: attid4(:), attva4(:)
    integer(kind=4) :: cret4, ind4
!      CALL ASSERT(LEN(MAA).EQ.32)
!      CALL ASSERT(LEN(FAM).EQ.32)
!      CALL ASSERT(LEN(ATTDES(1)).EQ.200)
!      CALL ASSERT(LEN(GRO(1)).EQ.80)
    fid4 = fid
    ind4 = ind
#ifdef _DEBUG_MED
    print *,'=== MFFAMI ==='
    print *,'MAA=',maa,' IND=',ind4
#endif
    allocate ( attid4(natt) )
    allocate ( attva4(natt) )
    call mfaofi(fid4, maa, ind4, fam, attid4,&
                attva4, attdes, num4, gro, cret4)
    num = num4
#ifdef _DEBUG_MED
    print *,'NUM=',num,' NATT=',natt
#endif
    call convi4(attid, attid4, -natt)
    call convi4(attval, attva4, -natt)
    cret = cret4
    deallocate (attid4)
    deallocate (attva4)
#else
    call mfaofi(fid, maa, ind, fam, attid,&
                attval, attdes, num, gro, cret)
#endif
! END MED
#endif
end subroutine
