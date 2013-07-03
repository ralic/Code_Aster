subroutine rvinvt(tensor, vonm, tres, trac, detr)
    implicit none
#include "asterc/r8vide.h"
#include "asterfort/fgequi.h"
#include "asterfort/rsvnmi.h"
#include "asterfort/rvdet3.h"
    real(kind=8) :: tensor(*), vonm, tres, trac, detr
!
!*********************************************************************
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
!
!   OPERATION REALISEE
!   ------------------
!
!     CALCUL DES QUANTITES DE VON MISES ET TRESCA POUR UN TENSEUR
!     3X3 A 2 INDICE
!
!   ARGUMENT EN ENTREE
!   ------------------
!
!     TENSOR : TABLEAU REPRESENTANT LE TENSEUR SYMETRIQUE, ORGANISE
!              COMME SUIT :   XX, YY, ZZ, XY, XZ, YZ
!
!   ARGUMENT EN SORTIE
!   ------------------
!
!     VONM : QUANTITE DE VON MISES
!            VONM = (3*T(I,J)*T(I,J)/2)**0.5)
!
!     TRES : QUANTITE DE TRESCA
!            SOIT L(I) LA IEME VALEUR PROPRE DU TENSEUR
!            TRES = MAX( L(I) - L(J) )
!
!     TRAC : TRACE DU TENSEUR
!
!     DETR : DETERMINANT DU TENSEUR
!
!*********************************************************************
!
    integer :: i, nbvp
    real(kind=8) :: t(6), equi(6), unsur3
!
    vonm = 0.0d0
    tres = 0.0d0
    detr = 0.0d0
    trac = 0.0d0
!
    do 10, i = 1, 6, 1
    if (tensor(i) .eq. r8vide()) then
        t(i) = 0.d0
    else
        t(i) = tensor(i)
    endif
    10 end do
!
    unsur3 = 1.0d0/3.0d0
    nbvp = 3
!
    call fgequi(t, 'SIGM', nbvp, equi)
    tres = equi(2)
!
    call rvdet3(t, detr)
!
    do 20, i = 1, 3 ,1
    trac = trac + t(i)
    20 end do
!
    do 30, i = 1, 3 ,1
    t(i) = t(i) - trac*unsur3
    30 end do
!
    call rsvnmi(t, vonm)
!
end subroutine
