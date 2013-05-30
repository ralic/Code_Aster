subroutine inical(nbin, lpain, lchin, nbout, lpaout,&
                  lchout)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: nbin, nbout
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchin(nbin), lchout(nbout)
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE POUR CALCUL
!
! INITIALISATIONS DES CHAMPS IN/OUT POUR CALCUL
!
! ----------------------------------------------------------------------
!
!
! IN  NBIN   : NOMBRE MAXI DE CHAMPS IN POUR CALCUL
! IN  LPAIN  : NOM DES TYPES DE CHAMP D'ENTREE
! IN  LCHIN  : NOM DES CHAMPS D'ENTREE
! IN  NBOUT  : NOMBRE MAXI DE CHAMPS OUT POUR CALCUL
! IN  LPAOUT : NOM DES TYPES DE CHAMP DE SORTIE
! IN  LCHOUT : NOM DES CHAMPS DE SORTIE
!
! ----------------------------------------------------------------------
!
    integer :: ich
    character(len=8) :: k8bla
    character(len=19) :: k19bla
!
! ---------------------------------------------------------------------
!
    k8bla = ' '
    k19bla = ' '
!
    do 100 ich = 1, nbin
        lchin(ich) = k19bla
        lpain(ich) = k8bla
100  end do
!
    do 200 ich = 1, nbout
        lchout(ich) = k19bla
        lpaout(ich) = k8bla
200  end do
end subroutine
