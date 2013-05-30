subroutine elref7(elrefv, tymvol, ndegre, nbf, elref1,&
                  elref2)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: gerald.nicolas at edf.fr
!
    implicit none
!
    include 'asterfort/u2mesk.h'
    character(len=8) :: elrefv
    integer :: tymvol, ndegre
    integer :: nbf
    character(len=8) :: elref1, elref2
!
!
!     BUT:
!         DETERMINATION DES CARACTERISTIQUES DES FACES DES VOLUMES
!
!     ARGUMENTS:
!     ----------
!      ENTREE :
!-------------
! IN   ELREFV : DENOMINATION DE LA MAILLE VOLUMIQUE
!               'QU4', 'QU8', 'QU9'
!               'TR3', 'TR6', 'TR7'
!               'HE8', 'H20', 'H27'
!               'PE6', 'P15', 'P18'
!               'TE4', 'T10'
!               'PY5', 'P13'
!
!      SORTIE :
!-------------
! OUT  TYMVOL : TYPE DE LA MAILLE VOLUMIQUE SELON LE CODE SUIVANT
!              -2 : QUADRANGLE
!              -1 : TRIANGLE
!               1 : HEXAEDRE
!               2 : PENTAEDRE
!               3 : TETRAEDRE
!               4 : PYRAMIDE
! OUT  NDEGRE : DEGRE DE L'ELEMENT
! OUT  NBF    : NOMBRE DE FACES DE LA MAILLE VOLUMIQUE
! OUT  ELREF1 : DENOMINATION DE LA MAILLE FACE DE ELREFV - FAMILLE 1
! OUT  ELREF2 : DENOMINATION DE LA MAILLE FACE DE ELREFV - FAMILLE 2
! ......................................................................
!
    character(len=6) :: valk(2)
!
    elref1 = '        '
    elref2 = '        '
!               12345678
!
!====
! 1. LA DESCRIPTION DES FACES DES VOLUMES 2D
!====
! 1.1. ==> QUADRANGLES
!
    if (elrefv(1:3) .eq. 'QU4') then
        tymvol = -2
        ndegre = 1
        nbf = 4
        elref1 = 'SE2'
!
    else if (elrefv(1:3).eq.'QU8') then
        tymvol = -2
        ndegre = 2
        nbf = 4
        elref1 = 'SE3'
!
    else if (elrefv(1:3).eq.'QU9') then
        tymvol = -2
        ndegre = 2
        nbf = 4
        elref1 = 'SE3'
!
! 1.2. ==> TRIANGLES
!
    else if (elrefv(1:3).eq.'TR3') then
        tymvol = -1
        ndegre = 1
        nbf = 3
        elref1 = 'SE2'
!
    else if (elrefv(1:3).eq.'TR6') then
        tymvol = -1
        ndegre = 2
        nbf = 3
        elref1 = 'SE3'
!
    else if (elrefv(1:3).eq.'TR7') then
        tymvol = -1
        ndegre = 2
        nbf = 3
        elref1 = 'SE3'
!
!====
! 2. LA DESCRIPTION DES FACES DES VOLUMES 3D
!====
! 2.1. ==> HEXAEDRES
!
    else if (elrefv(1:3).eq.'HE8') then
        tymvol = 1
        ndegre = 1
        nbf = 6
        elref1 = 'QU4'
!
    else if (elrefv(1:3).eq.'H20') then
        tymvol = 1
        ndegre = 2
        nbf = 6
        elref1 = 'QU8'
!
    else if (elrefv(1:3).eq.'H27') then
        tymvol = 1
        ndegre = 2
        nbf = 6
        elref1 = 'QU9'
!
! 2.2. ==> PENTAEDRES
!
    else if (elrefv(1:3).eq.'PE6') then
        tymvol = 2
        ndegre = 1
        nbf = 5
        elref1 = 'TR3'
        elref2 = 'QU4'
!
    else if (elrefv(1:3).eq.'P15') then
        tymvol = 2
        ndegre = 2
        nbf = 5
        elref1 = 'TR6'
        elref2 = 'QU8'
!
    else if (elrefv(1:3).eq.'P18') then
        tymvol = 2
        ndegre = 2
        nbf = 5
        elref1 = 'TR6'
        elref2 = 'QU9'
!
! 2.3. ==> TETRAEDRES
!
    else if (elrefv(1:3).eq.'TE4') then
        tymvol = 3
        ndegre = 1
        nbf = 4
        elref1 = 'TR3'
!
    else if (elrefv(1:3).eq.'T10') then
        tymvol = 3
        ndegre = 2
        nbf = 4
        elref1 = 'TR6'
!
! 2.4. ==> PYRAMIDES
!
    else if (elrefv(1:3).eq.'PY5') then
        tymvol = 4
        ndegre = 1
        nbf = 5
        elref1 = 'TR3'
        elref2 = 'QU4'
!
    else if (elrefv(1:3).eq.'P13') then
        tymvol = 4
        ndegre = 2
        nbf = 5
        elref1 = 'TR6'
        elref2 = 'QU8'
!
!====
! 3. INCONNU
!====
!
    else
!
        valk(1) = elrefv(1:3)
        call u2mesk('F', 'INDICATEUR_10', 1, valk)
!
    endif
!
!GN          WRITE(6,*) 'TYMVOL :',TYMVOL, ', NBF :', NBF
!GN          WRITE(6,*) 'ELREF1 : ',ELREF1
!GN          WRITE(6,*) 'ELREF2 : ',ELREF2
!
end subroutine
