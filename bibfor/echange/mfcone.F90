subroutine mfcone(fid, maa, conn, csize, switch,&
                  n, typent, typgeo, typcon, cret)
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
!     L'ARGUMENT CSIZE N'EST PAS DANS L'API MED
    implicit none
    include 'med/mmhcyw.h'
    character(len=*) :: maa
    integer :: fid, conn(*), csize, typent, typgeo, typcon, cret
    integer :: n, switch, mdnont, mdnoit
    real(kind=8) :: mdnodt
#ifdef _DISABLE_MED
    call u2mess('F', 'FERMETUR_2')
#else
! BEGIN MED
#ifdef _USE_MED_SHORT_INT
    integer(kind=4), ALLOCATABLE :: conn4(:)
    integer(kind=4) :: fid4, typen4, typge4, typco4, cret4
    integer(kind=4) :: n4, switc4, mdnon4, mdnoi4
#ifdef _DEBUG_MED
    integer :: ic
#endif
    mdnont = -1
    mdnoit = -1
    mdnodt = -1.d0
    fid4 = fid
    allocate ( conn4(csize) )
    call convi4(conn, conn4, csize)
    typen4 = typent
    typge4 = typgeo
    typco4 = typcon
    n4 = n
    switc4 = switch
    mdnon4 = mdnont
    mdnoi4 = mdnoit
#ifdef _DEBUG_MED
    print *,'=== MFCONE ==='
    print *,'FID=',fid4,' MAA=',maa
    write(6,*) 'CONN=',(conn(ic),ic=1,min(12,csize))
    write(6,*)'CONN4=',(conn4(ic),ic=1,min(12,csize))
    print *,'SWITCH=',switc4,' N=',n4,' TYPENT=',typen4
    print *,'TYPGEO=',typge4,' TYPCON=',typco4
#endif
    call mmhcyw(fid4, maa, mdnon4, mdnoi4, mdnodt,&
                typen4, typge4, typco4, switc4, n4,&
                conn4, cret4)
    cret = cret4
    deallocate (conn4)
#else
    mdnont = -1
    mdnoit = -1
    mdnodt = -1.d0
    call mmhcyw(fid, maa, mdnont, mdnoit, mdnodt,&
                typent, typgeo, typcon, switch, n,&
                conn, cret)
#endif
! END MED
#endif
end subroutine
