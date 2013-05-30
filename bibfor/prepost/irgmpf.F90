subroutine irgmpf(ifi, versio)
    implicit none
    integer :: ifi, versio
!     ------------------------------------------------------------------
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
!
!     BUT :   ECRITURE D'UN RESULTAT AU FORMAT GMSH
!
!     ENTREE:
!       IFI    : NUMERO D'UNITE LOGIQUE DU FICHIER GMSH
!     ------------------------------------------------------------------
!
    write(ifi,101) '$PostFormat'
!
    if (versio .eq. 1) then
        write(ifi,102) 1.0d0 , 0, 8
    else if (versio.eq.2) then
        write(ifi,102) 1.2d0 , 0, 8
    endif
!
    write(ifi,103) '$EndPostFormat'
!
    101 format(a11)
    102 format(f4.1,1x,i1,1x,i1)
    103 format(a14)
!
end subroutine
