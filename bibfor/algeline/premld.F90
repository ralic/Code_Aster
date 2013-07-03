subroutine premld(n1, diag, col, xadj1, adjnc1,&
                  nnz, deb, voisin, suiv, ladjn,&
                  nrl)
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: olivier.boiteau at edf.fr
    implicit none
#include "asterfort/caladj.h"
    integer :: n1, diag(0:*), col(*)
    integer :: xadj1(n1+1), adjnc1(*)
    integer :: voisin(*), suiv(*)
!     VARIABLES LOCALES
    integer :: lmat
    integer :: nnz(1:n1), deb(1:n1), ladjn
    integer :: nrl
!-------------------------------------------------------
!      1) A PARTIR DE DIAG ET COL -> ADJNC1
!       AVEC TOUS LES DDL ASTER   (1:N1)
!       CETTE ROUTINE NECESSITE 3*LMAT DE LONGUEUR DE TRAVAIL
!       COL (LMAT)
!       ADJNC1(2*(LMAT-N1))
!       DEB(1:N1)
!       SUIV(LT)
!       VOISIN(LT)
!     CES 3 TABLEAUX NE SONT UTILISES QU'AVEC LES RELATIONS LINEAIRES
!     ENTRE DDL (NRL >0)
!-------------------------------------------------------
    lmat = diag(n1)
    call caladj(col, diag, xadj1, adjnc1, n1,&
                nnz, deb, voisin, suiv, lmat,&
                ladjn, nrl)
end subroutine
