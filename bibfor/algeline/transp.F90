subroutine transp(a, nlamax, dimal, dimac, b,&
                  nlbmax)
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
!    BUT : TRANSPOSEE DE LA MATRICE A DANS LA MATRICE B
!
!    IN  : NLAMAX    : NB DE LIGNES DE MATRICE A DIMENSIONNEES
!          DIMAL     : NB DE LIGNES DE MATRICE A UTILISEES
!          DIMAC     : NB DE COLONNES DE MATRICE A
!          A(DIMAL,DIMAC): MATRICE A
!          NLBMAX    : NB DE LIGNES DE MATRICE B DIMENSIONNEES
!
!    OUT : B(DIMAC,DIMAL): MATRICE TRANSPOSEE DE A
! ------------------------------------------------------------------
    implicit none
#include "asterfort/u2mess.h"
    integer :: dimal, dimac
    real(kind=8) :: a(nlamax, *), b(nlbmax, *)
!-----------------------------------------------------------------------
    integer :: icol, ilig, nlamax, nlbmax
!-----------------------------------------------------------------------
    if (dimac .gt. nlbmax) then
        call u2mess('F', 'ALGELINE3_51')
    endif
    do 10 ilig = 1, dimal
        do 5 icol = 1, dimac
            b(icol,ilig) = a(ilig,icol)
 5      continue
10  continue
end subroutine
