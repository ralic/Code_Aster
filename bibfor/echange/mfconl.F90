subroutine mfconl(fid, maa, conn, csize, switch,&
                  typent, typgeo, typcon, cret)
! person_in_charge: nicolas.sellenet at edf.fr
!     L'ARGUMENT CSIZE N'EST PAS DANS L'API MED
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
    include 'med/mmhcyr.h'
    character(len=*) :: maa
    integer :: fid, typent, typgeo, cret
    integer :: typcon, switch, csize, mdnont, mdnoit
    integer :: conn(*)
#ifdef _DISABLE_MED
    call u2mess('F', 'FERMETUR_2')
#else
! BEGIN MED
#ifdef _USE_MED_SHORT_INT
    integer(kind=4) :: fid4, typen4, typge4, cret4
    integer(kind=4) :: typco4, switc4, mdnon4, mdnoi4
    integer(kind=4), ALLOCATABLE :: conn4(:)
#ifdef _DEBUG_MED
    integer :: ic
#endif
    mdnont = -1
    mdnoit = -1
    fid4 = fid
    typen4 = typent
    typge4 = typgeo
    typco4 = typcon
    switc4 = switch
    mdnon4 = mdnont
    mdnoi4 = mdnoit
#ifdef _DEBUG_MED
    print *,'=== MFCONL ==='
    print *,'CSIZE=',csize
    print *,'SWITCH=',switc4,' TYPENT=',typen4
    print *,'TYPGEO=',typge4,' TYPCON=',typco4
#endif
    allocate ( conn4(csize) )
    call mmhcyr(fid4, maa, mdnon4, mdnoi4, typen4,&
                typge4, typco4, switc4, conn4, cret4)
    call convi4(conn, conn4, -csize)
    cret = cret4
    deallocate (conn4)
#ifdef _DEBUG_MED
    write(6,*) 'CONN(1..12)=',(conn(ic),ic=1,min(12,csize))
    print *,'CRET=',cret
#endif
#else
    mdnont = -1
    mdnoit = -1
    call mmhcyr(fid, maa, mdnont, mdnoit, typent,&
                typgeo, typcon, switch, conn, cret)
#endif
! END MED
#endif
end subroutine
