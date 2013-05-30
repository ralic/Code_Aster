subroutine mfchre(fid, cha, val, intlac, n,&
                  locname, numco, profil, pflmod, typent,&
                  typgeo, numdt, dt, numo, cret)
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
    include 'med/mfdrpw.h'
    character(len=*) :: cha, profil, locname
    integer :: fid, n, pflmod, typent, typgeo, cret
    integer :: intlac, numco, numdt, numo
    real(kind=8) :: dt
    real(kind=8) :: val(*)
#ifdef _DISABLE_MED
    call u2mess('F', 'FERMETUR_2')
#else
! BEGIN MED
#ifdef _USE_MED_SHORT_INT
    integer(kind=4) :: fid4, n4, pflmo4, typen4, typge4, cret4
    integer(kind=4) :: intla4, numco4, numdt4, numo4
#ifdef _DEBUG_MED
    print *,'=== MFCHRE ==='
#endif
    fid4 = fid
    n4 = n
    pflmo4 = pflmod
    typen4 = typent
    typge4 = typgeo
    intla4 = intlac
    numco4 = numco
    numdt4 = numdt
    numo4 = numo
    call mfdrpw(fid4, cha, numdt4, numo4, dt,&
                typen4, typge4, pflmo4, profil, locname,&
                intla4, numco4, n4, val, cret4)
    cret = cret4
#else
    call mfdrpw(fid, cha, numdt, numo, dt,&
                typent, typgeo, pflmod, profil, locname,&
                intlac, numco, n, val, cret)
#endif
! END MED
#endif
end subroutine
