subroutine mfnome(fid, maa, nom, n, typent,&
                  typgeo, cret)
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
    include 'med/mmheaw.h'
    character(len=*) :: maa
    character(len=*) :: nom(*)
    integer :: fid, typent, typgeo, cret, mdnont, mdnoit
    integer :: n
#ifdef _DISABLE_MED
    call u2mess('F', 'FERMETUR_2')
#else
! BEGIN MED
#ifdef _USE_MED_SHORT_INT
    integer(kind=4) :: fid4, typen4, typge4, cret4
    integer(kind=4) :: n4, mdnon4, mdnoi4
#ifdef _DEBUG_MED
    print *,'=== MFNOME ==='
#endif
    mdnont = -1
    mdnoit = -1
    fid4 = fid
    typen4 = typent
    typge4 = typgeo
    n4 = n
    mdnon4 = mdnont
    mdnoi4 = mdnoit
    call mmheaw(fid4, maa, mdnon4, mdnoi4, typen4,&
                typge4, n4, nom, cret4)
    cret = cret4
#else
    mdnont = -1
    mdnoit = -1
    call mmheaw(fid, maa, mdnont, mdnoit, typent,&
                typgeo, n, nom, cret)
#endif
! END MED
#endif
end subroutine
