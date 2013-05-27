subroutine mfmaac(fid, nom, dim, type, desc,&
                  descdt, typrep, nocomp, unit, cret)
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
    include 'med/mmhcre.h'
    character(len=*) :: nom
    character(len=*) :: desc, descdt
    character(len=16) :: nocomp(3), unit(3)
    integer :: fid, dim, type, cret, stunde, typrep
#ifdef _DISABLE_MED
    call u2mess('F', 'FERMETUR_2')
#else
! BEGIN MED
#ifdef _USE_MED_SHORT_INT
    integer(kind=4) :: fid4, dim4, type4, cret4, stund4, typre4
#ifdef _DEBUG_MED
    print *,'=== MFMAAC ==='
#endif
    stunde = 1
    fid4 = fid
    dim4 = dim
    type4 = type
    stund4 = stunde
    typre4 = typrep
    call mmhcre(fid4, nom, dim4, dim4, type4,&
                desc, descdt, stund4, typre4, nocomp,&
                unit, cret4)
    cret = cret4
#else
    stunde = 1
    call mmhcre(fid, nom, dim, dim, type,&
                desc, descdt, stunde, typrep, nocomp,&
                unit, cret)
#endif
! END MED
#endif
end subroutine
