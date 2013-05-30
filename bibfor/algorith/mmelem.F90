subroutine mmelem(nomte, ndim, nddl, typmae, nne,&
                  typmam, nnm, nnl, nbcps, nbdm,&
                  laxis, leltf)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/lteatt.h'
    integer :: ndim, nddl, nne, nnm, nnl
    integer :: nbcps, nbdm
    character(len=8) :: typmae, typmam
    character(len=16) :: nomte
    logical :: laxis, leltf
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! RETOURNE DES INFOS SUR LES ELEMENTS DE CONTACT FORMES ENTRE
! DEUX ELEMENTS DE SURFACE
!
! ----------------------------------------------------------------------
!
!
! IN  NOMTE  : NOM DU TE DE L'ELEMENT DE CONTACT EN JEU
! OUT NDIM   : DIMENSION DE LA MAILLE DE CONTACT
! OUT NDDL   : NOMBRE TOTAL DE DEGRES DE LIBERTE DE LA MAILLE DE CONTACT
! OUT TYPMAE : TYPE DE LA MAILLE ESCLAVE
! OUT NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! OUT TYPMAM : TYPE DE LA MAILLE MAITRE
! OUT NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! OUT NNL    : NOMBRE DE NOEUDS PORTANT UN LAGRANGE DE CONTACT/FROTT
! OUT NBCPS  : NOMBRE DE COMPOSANTES/NOEUD DES LAGR_C+LAGR_F
! OUT NBDM   : NOMBRE DE COMPOSANTES/NOEUD DES DEPL+LAGR_C+LAGR_F
! OUT LAXIS  : .TRUE. SI MODELE AXISYMETRIQUE
! OUT LELTF  : .TRUE. SI ELEMENT COMPORTANT DES DDL DE FROTTEMENT
!
! ----------------------------------------------------------------------
!
    integer :: i2d, i3d
!
! ----------------------------------------------------------------------
!
!
!
! --- MODELE AXISYMETRIQUE ?
!
    laxis = lteatt(' ','AXIS','OUI')
!
! --- ELEMENT AVEC DDL DE FROTTEMENT ?
!
    leltf = lteatt(' ','FROTTEMENT','OUI')
!
! --- NOMBRE DE COMPOSANTES LAGRANGIENS (NON DEPLACEMENT)
!
    if (leltf) then
! ----- COMPOSANTES 2D : LAGS_C   LAGS_F1
! ----- COMPOSANTES 3D : LAGS_C   LAGS_F1  LAGS_F2
        i2d = 2
        i3d = 3
    else
! ----- COMPOSANTE : LAGS_C
        i2d = 1
        i3d = 1
    endif
!
! --- 2D
!
! --- 'SE2'
    if (nomte(1:6) .eq. 'CFS2S2' .or. nomte(1:6) .eq. 'COS2S2') then
        ndim = 2
        typmae = 'SE2'
        nne = 2
        typmam = 'SE2'
        nnm = 2
        nddl = nnm*ndim + nne*(ndim+i2d)
    else if (nomte(1:6).eq.'CFS2S3' .or. nomte(1:6).eq.'COS2S3') then
        ndim = 2
        typmae = 'SE2'
        nne = 2
        typmam = 'SE3'
        nnm = 3
        nddl = nnm*ndim + nne*(ndim+i2d)
! --- 'SE3'
    else if (nomte(1:6).eq.'CFS3S2' .or. nomte(1:6).eq.'COS3S2') then
        ndim = 2
        typmae = 'SE3'
        nne = 3
        typmam = 'SE2'
        nnm = 2
        nddl = nnm*ndim + nne*(ndim+i2d)
    else if (nomte(1:6).eq.'CFS3S3' .or. nomte(1:6).eq.'COS3S3') then
        ndim = 2
        typmae = 'SE3'
        nne = 3
        typmam = 'SE3'
        nnm = 3
        nddl = nnm*ndim + nne*(ndim+i2d)
!
! --- 3D
!
! --- 'SE2'
    else if (nomte.eq.'CFP2P2' .or. nomte.eq.'COP2P2') then
        ndim = 3
        typmae = 'SE2'
        nne = 2
        typmam = 'SE2'
        nnm = 2
        nddl = nnm*ndim + nne*(ndim+i3d)
! --- 'TR3'
    else if (nomte.eq.'CFT3T3' .or. nomte.eq.'COT3T3') then
        ndim = 3
        typmae = 'TR3'
        nne = 3
        typmam = 'TR3'
        nnm = 3
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFT3T6' .or. nomte.eq.'COT3T6') then
        ndim = 3
        typmae = 'TR3'
        nne = 3
        typmam = 'TR6'
        nnm = 6
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFT3Q4' .or. nomte.eq.'COT3Q4') then
        ndim = 3
        typmae = 'TR3'
        nne = 3
        typmam = 'QU4'
        nnm = 4
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFT3Q8' .or. nomte.eq.'COT3Q8') then
        ndim = 3
        typmae = 'TR3'
        nne = 3
        typmam = 'QU8'
        nnm = 8
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFT3Q9' .or. nomte.eq.'COT3Q9') then
        ndim = 3
        typmae = 'TR3'
        nne = 3
        typmam = 'QU9'
        nnm = 9
        nddl = nnm*ndim + nne*(ndim+i3d)
! --- 'TR6'
    else if (nomte.eq.'CFT6T3' .or. nomte.eq.'COT6T3') then
        ndim = 3
        typmae = 'TR6'
        nne = 6
        typmam = 'TR3'
        nnm = 3
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFT6T6' .or. nomte.eq.'COT6T6') then
        ndim = 3
        typmae = 'TR6'
        nne = 6
        typmam = 'TR6'
        nnm = 6
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFT6Q4' .or. nomte.eq.'COT6Q4') then
        ndim = 3
        typmae = 'TR6'
        nne = 6
        typmam = 'QU4'
        nnm = 4
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFT6Q8' .or. nomte.eq.'COT6Q8') then
        ndim = 3
        typmae = 'TR6'
        nne = 6
        typmam = 'QU8'
        nnm = 8
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFT6Q9' .or. nomte.eq.'COT6Q9') then
        ndim = 3
        typmae = 'TR6'
        nne = 6
        typmam = 'QU9'
        nnm = 9
        nddl = nnm*ndim + nne*(ndim+i3d)
! --- 'QU4'
    else if (nomte.eq.'CFQ4T3' .or. nomte.eq.'COQ4T3') then
        ndim = 3
        typmae = 'QU4'
        nne = 4
        typmam = 'TR3'
        nnm = 3
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFQ4T6' .or. nomte.eq.'COQ4T6') then
        ndim = 3
        typmae = 'QU4'
        nne = 4
        typmam = 'TR6'
        nnm = 6
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFQ4Q4' .or. nomte.eq.'COQ4Q4') then
        ndim = 3
        typmae = 'QU4'
        nne = 4
        typmam = 'QU4'
        nnm = 4
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFQ4Q8' .or. nomte.eq.'COQ4Q8') then
        ndim = 3
        typmae = 'QU4'
        nne = 4
        typmam = 'QU8'
        nnm = 8
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFQ4Q9' .or. nomte.eq.'COQ4Q9') then
        ndim = 3
        typmae = 'QU4'
        nne = 4
        typmam = 'QU9'
        nnm = 9
        nddl = nnm*ndim + nne*(ndim+i3d)
! --- 'QU8'
    else if (nomte.eq.'CFQ8T3' .or. nomte.eq.'COQ8T3') then
        ndim = 3
        typmae = 'QU8'
        nne = 8
        typmam = 'TR3'
        nnm = 3
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFQ8T6' .or. nomte.eq.'COQ8T6') then
        ndim = 3
        typmae = 'QU8'
        nne = 8
        typmam = 'TR6'
        nnm = 6
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFQ8Q4' .or. nomte.eq.'COQ8Q4') then
        ndim = 3
        typmae = 'QU8'
        nne = 8
        typmam = 'QU4'
        nnm = 4
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFQ8Q8' .or. nomte.eq.'COQ8Q8') then
        ndim = 3
        typmae = 'QU8'
        nne = 8
        typmam = 'QU8'
        nnm = 8
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFQ8Q9' .or. nomte.eq.'COQ8Q9') then
        ndim = 3
        typmae = 'QU8'
        nne = 8
        typmam = 'QU9'
        nnm = 9
        nddl = nnm*ndim + nne*(ndim+i3d)
! --- 'QU9'
    else if (nomte.eq.'CFQ9T3' .or. nomte.eq.'COQ9T3') then
        ndim = 3
        typmae = 'QU9'
        nne = 9
        typmam = 'TR3'
        nnm = 3
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFQ9T6' .or. nomte.eq.'COQ9T6') then
        ndim = 3
        typmae = 'QU9'
        nne = 9
        typmam = 'TR6'
        nnm = 6
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFQ9Q4' .or. nomte.eq.'COQ9Q4') then
        ndim = 3
        typmae = 'QU9'
        nne = 9
        typmam = 'QU4'
        nnm = 4
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFQ9Q8' .or. nomte.eq.'COQ9Q8') then
        ndim = 3
        typmae = 'QU9'
        nne = 9
        typmam = 'QU8'
        nnm = 8
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFQ9Q9' .or. nomte.eq.'COQ9Q9') then
        ndim = 3
        typmae = 'QU9'
        nne = 9
        typmam = 'QU9'
        nnm = 9
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFS2T3' .or. nomte.eq.'COS2T3') then
        ndim = 3
        typmae = 'SE2'
        nne = 2
        typmam = 'TR3'
        nnm = 3
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFS2T6' .or. nomte.eq.'COS2T6') then
        ndim = 3
        typmae = 'SE2'
        nne = 2
        typmam = 'TR6'
        nnm = 6
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFS2Q4' .or. nomte.eq.'COS2Q4') then
        ndim = 3
        typmae = 'SE2'
        nne = 2
        typmam = 'QU4'
        nnm = 4
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFS2Q8' .or. nomte.eq.'COS2Q8') then
        ndim = 3
        typmae = 'SE2'
        nne = 2
        typmam = 'QU8'
        nnm = 8
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFS2Q9' .or. nomte.eq.'COS2Q9') then
        ndim = 3
        typmae = 'SE2'
        nne = 2
        typmam = 'QU9'
        nnm = 9
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFS3T3' .or. nomte.eq.'COS3T3') then
        ndim = 3
        typmae = 'SE3'
        nne = 3
        typmam = 'TR3'
        nnm = 3
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFS3T6' .or. nomte.eq.'COS3T6') then
        ndim = 3
        typmae = 'SE3'
        nne = 3
        typmam = 'TR6'
        nnm = 6
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFS3Q4' .or. nomte.eq.'COS3Q4') then
        ndim = 3
        typmae = 'SE3'
        nne = 3
        typmam = 'QU4'
        nnm = 4
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFS3Q8' .or. nomte.eq.'COS3Q8') then
        ndim = 3
        typmae = 'SE3'
        nne = 3
        typmam = 'QU8'
        nnm = 8
        nddl = nnm*ndim + nne*(ndim+i3d)
    else if (nomte.eq.'CFS3Q9' .or. nomte.eq.'COS3Q9') then
        ndim = 3
        typmae = 'SE3'
        nne = 3
        typmam = 'QU9'
        nnm = 9
        nddl = nnm*ndim + nne*(ndim+i3d)
    else
        call assert(.false.)
    endif
!
! --- NOMBRE DE NOEUDS PORTANT DES LAGRANGES
!
    nnl = nne
!
! --- NOMBRE DE COMPOSANTES LAGR_C + LAGR_F
!
    if (leltf) then
        nbcps = ndim
    else
        nbcps = 1
    endif
!
! --- NOMBRE DE COMPOSANTES TOTAL DEPL + LAGR_C + LAGR_F
!
    nbdm = ndim + nbcps
!
    call assert(nddl.le.81)
    call assert((ndim.eq.2).or.(ndim.eq.3))
!
end subroutine
