subroutine irmac2(ktype, ityca, gtype, nnoe)
    implicit   none
    include 'asterfort/u2mess.h'
    integer :: ityca, nnoe
    character(len=8) :: ktype, gtype
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!
    if (ktype .eq. 'POI1') then
        ityca = 1
        gtype = 'POI1'
    else if (ktype .eq. 'SEG2') then
        ityca = 2
        gtype = 'SEG2'
    else if (ktype .eq. 'SEG3') then
        ityca = 3
        gtype = 'SEG3'
    else if (ktype .eq. 'SEG4') then
        nnoe = nnoe - 2
        call u2mess('I', 'PREPOST2_64')
        ityca = 2
        gtype = 'SEG2'
    else if (ktype .eq. 'TRIA3') then
        ityca = 4
        gtype = 'TRI3'
    else if (ktype .eq. 'TRIA6') then
        ityca = 6
        gtype = 'TRI6'
    else if (ktype .eq. 'TRIA7') then
        nnoe = nnoe - 1
        call u2mess('I', 'PREPOST2_65')
        ityca = 6
        gtype = 'TRI6'
    else if (ktype .eq. 'QUAD4') then
        ityca = 8
        gtype = 'QUA4'
    else if (ktype .eq. 'QUAD8') then
        ityca = 10
        gtype = 'QUA8'
    else if (ktype .eq. 'QUAD9') then
        nnoe = nnoe - 1
        call u2mess('I', 'PREPOST2_66')
        ityca = 10
        gtype = 'QUA8'
!CC         ITYCA = 11
!CC         GTYPE = 'QUA9'
    else if (ktype .eq. 'HEXA8') then
        ityca = 14
        gtype = 'CUB8'
    else if (ktype .eq. 'HEXA20') then
        ityca = 15
        gtype = 'CU20'
    else if (ktype .eq. 'HEXA27') then
        ityca = 33
        gtype = 'CU27'
    else if (ktype .eq. 'PENTA6') then
        ityca = 16
        gtype = 'PRI6'
    else if (ktype .eq. 'PENTA15') then
        ityca = 17
        gtype = 'PR15'
    else if (ktype .eq. 'PENTA18') then
        ityca = 17
        call u2mess('I', 'PREPOST2_86')
        gtype = 'PR15'
    else if (ktype .eq. 'TETRA4') then
        ityca = 23
        gtype = 'TET4'
    else if (ktype .eq. 'TETRA10') then
        ityca = 24
        gtype = 'TE10'
    else if (ktype .eq. 'PYRAM5') then
        ityca = 25
        gtype = 'PYR5'
    else if (ktype .eq. 'PYRAM13') then
        ityca = 26
        gtype = 'PY13'
    endif
!
end subroutine
