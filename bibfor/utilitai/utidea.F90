subroutine utidea(nom, itype, versio)
    implicit none
    integer :: itype, versio
    character(len=*) :: nom
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CORRESPONDANCE NOM DE MAILLE "ASTER" AVEC NUMERO "IDEAS"
!     ------------------------------------------------------------------
    character(len=8) :: nommai
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    itype = 0
    nommai = nom
    if (nommai .eq. 'POI1') then
        itype = 161
    else if (nommai .eq. 'SEG2') then
        itype = 21
    else if (nommai .eq. 'SEG3') then
        itype = 24
    else if (nommai .eq. 'SEG4') then
        itype = 21
    else if (nommai .eq. 'TRIA3') then
        if (versio .eq. 5) then
            itype= 91
        else
            itype = 74
        endif
    else if (nommai .eq. 'TRIA6') then
        if (versio .eq. 5) then
            itype= 92
        else
            itype = 72
        endif
    else if (nommai .eq. 'TRIA7') then
        if (versio .eq. 5) then
            itype= 92
        else
            itype = 72
        endif
    else if (nommai .eq. 'TRIA9') then
        itype = 73
    else if (nommai .eq. 'QUAD4') then
        if (versio .eq. 5) then
            itype= 94
        else
            itype = 71
        endif
    else if (nommai .eq. 'QUAD8') then
        if (versio .eq. 5) then
            itype= 95
        else
            itype = 75
        endif
    else if (nommai .eq. 'QUAD9') then
        if (versio .eq. 5) then
            itype= 95
        else
            itype = 75
        endif
    else if (nommai .eq. 'QUAD12') then
        itype = 76
    else if (nommai .eq. 'TETRA4') then
        itype = 111
    else if (nommai .eq. 'TETRA10') then
        itype = 118
    else if (nommai .eq. 'PENTA6') then
        itype = 112
    else if (nommai .eq. 'PENTA15') then
        itype = 113
    else if (nommai .eq. 'PENTA18') then
        itype = 113
    else if (nommai .eq. 'HEXA8') then
        itype = 115
    else if (nommai .eq. 'HEXA20') then
        itype = 116
    else if (nommai .eq. 'HEXA27') then
        itype = 116
    else if (nommai .eq. 'PYRAM5') then
        itype = 6000
    else if (nommai .eq. 'PYRAM13') then
        itype = 6001
    endif
!
end subroutine
