subroutine ordon1(vale, nb)
    implicit none
    include 'asterfort/ordr8.h'
    include 'blas/dcopy.h'
    integer :: nb
    real(kind=8) :: vale(*)
! ----------------------------------------------------------------------
! TOLE CRS_1404
! ======================================================================
! COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
! person_in_charge: mathieu.courtois at edf.fr
! ----------------------------------------------------------------------
!     APPELEE PAR ORDONN : ON SAIT DEJA QU'IL FAUT INVERSER L'ORDRE
!     ORDON1 POUR LES FONCTIONS A VALEURS REELLES
! IN/OUT : VALE : ABSCISSES SUIVIES DES ORDONNEES
! IN     : NB   : NBRE DE POINTS
! ----------------------------------------------------------------------
    integer :: i, iord(nb)
    real(kind=8) :: xbid(nb), yrbid(nb)
!     ------------------------------------------------------------------
!
!
    call dcopy(nb, vale, 1, xbid, 1)
    call dcopy(nb, vale(nb+1), 1, yrbid, 1)
    call ordr8(xbid, nb, iord)
    do 101 i = 1, nb
        vale(i)=xbid(iord(i))
        vale(nb+i)=yrbid(iord(i))
101  end do
!
end subroutine
