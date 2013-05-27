subroutine mfchac(fid, cha, nomamd, type, comp,&
                  unit, ncomp, cret)
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
    include 'med/mfdcre.h'
    character(len=*) :: cha, nomamd, comp(*), unit(*)
    character(len=80) :: unidt
    integer :: fid, ncomp, cret, type
!
#ifdef _DISABLE_MED
    call u2mess('F', 'FERMETUR_2')
#else
! BEGIN MED
#ifdef _USE_MED_SHORT_INT
    integer(kind=4) :: fid4, ncomp4, cret4, type4
#ifdef _DEBUG_MED
    print *,'=== MFCHAC ==='
#endif
    unidt = ' '
    fid4 = fid
    ncomp4 = ncomp
    type4 = type
    call mfdcre(fid4, cha, type4, ncomp4, comp,&
                unit, unidt, nomamd, cret4)
    cret = cret4
#else
    unidt = ' '
    call mfdcre(fid, cha, type, ncomp, comp,&
                unit, unidt, nomamd, cret)
#endif
! END MED
#endif
end subroutine
