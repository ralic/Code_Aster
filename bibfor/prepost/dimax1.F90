subroutine dimax1(jvec1, jvec2, nbp1, nbp2, dismax,&
                  cu1max, cv1max, cu2max, cv2max)
    implicit   none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    integer :: jvec1, jvec2, nbp1, nbp2
    real(kind=8) :: dismax, cu1max, cv1max, cu2max, cv2max
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean.angles at edf.fr
! ---------------------------------------------------------------------
! BUT: ENTRE DEUX LISTES DE POINTS, DETERMINER LES DEUX POINTS QUI SONT
!     LE PLUS ELOIGNES.
! ---------------------------------------------------------------------
! ARGUMENTS :
!     JVEC1   : IN  : ADRESSE DU VECTEUR CONTENANT LES POINTS DU
!                     PREMIER GROUPE DE POINTS.
!     JVEC2   : IN  : ADRESSE DU VECTEUR CONTENANT LES POINTS DU
!                     SECOND GROUPE DE POINTS.
!     NBP1    : IN  : NOMBRE DE POINTS DU PREMIER GROUPE DE POINTS.
!     NBP2    : IN  : NOMBRE DE POINTS DU SECOND GROUPE DE POINTS.
!     DISMAX  : OUT : DISTANCE ENTRE LES DEUX POINTS LES PLUS ELOIGNES.
!     CU1MAX  : OUT : COMPOSANTE U DU POINT LE PLUS ELOIGNE APPARTENANT
!                     AU PREMIER GROUPE.
!     CV1MAX  : OUT : COMPOSANTE V DU POINT LE PLUS ELOIGNE APPARTENANT
!                     AU PREMIER GROUPE.
!     CU2MAX  : OUT : COMPOSANTE U DU POINT LE PLUS ELOIGNE APPARTENANT
!                     AU SECOND GROUPE.
!     CV2MAX  : OUT : COMPOSANTE V DU POINT LE PLUS ELOIGNE APPARTENANT
!                     AU SECOND GROUPE.
!     -----------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: i, j
    real(kind=8) :: cu1, cv1, cu2, cv2, dist
!     ------------------------------------------------------------------
!
!234567                                                              012
    call jemarq()
!
    dismax = 0.0d0
!
    do 10 i = 1, nbp1
        cu1 = zr(jvec1 + (i-1)*2)
        cv1 = zr(jvec1 + (i-1)*2 + 1)
!
        do 20 j = 1, nbp2
            cu2 = zr(jvec2 + (j-1)*2)
            cv2 = zr(jvec2 + (j-1)*2 + 1)
            dist = sqrt((cu1 - cu2)**2 + (cv1 - cv2)**2)
!
            if (dist .gt. dismax) then
                dismax = dist
                cu1max = cu1
                cv1max = cv1
                cu2max = cu2
                cv2max = cv2
            endif
!
20      continue
10  end do
!
    call jedema()
end subroutine
