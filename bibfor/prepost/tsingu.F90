subroutine tsingu(nelem, nbr, re, taille, he)
!
    implicit none
    integer :: nelem, nbr(nelem)
    real(kind=8) :: re(nelem), taille(nelem)
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!     BUT:
!         CALCUL LA NOUVELLE TAILLE
!         OPTION : 'SING_ELEM'
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   NELEM         : NOMBRE D ELEMENTS FINIS
! IN   NBR(NELEM)    : NOMBRE DE COMPOSANTES A STOCKER PAR EF
!      3 SI EF SURFACIQUES EN 2D OU VOLUMIQUES EN 3D
!      0 SINON
! IN   RE(NELEM)     : RAPPORT ENTRE ANCIENNE ET NOUVELLE TAILLE
! IN   TAILLE(NELEM) : ANCIENNE TAILLE
!
!      SORTIE :
!-------------
! OUT  HE(NELEM)     : NOUVELLE TAILLE
!
! ......................................................................
!
    integer :: inel
    real(kind=8) :: he(nelem)
!
    do 10 inel = 1, nelem
        if (nbr(inel) .eq. 3) then
            he(inel)=re(inel)*taille(inel)
        endif
10  end do
!
end subroutine
