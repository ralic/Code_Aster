subroutine nmgvdn(ndim, nno1, nno2, iu, ia)
!
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
!
    implicit none
!
    integer :: ndim, nno1, nno2, iu(ndim*nno1), ia(nno2)
! ---------------------------------------------------------------------
!
!     POSITION DES INDICES POUR LES DEGRES DE LIBERTE
!
! IN  NDIM    : DIMENSION DES ELEMENTS
! IN  NNO1    : NOMBRE DE NOEUDS (FAMILLE U)
! IN  NNO2    : NOMBRE DE NOEUDS (FAMILLE A)
! ---------------------------------------------------------------------
!
    integer :: n, i, os
!
    character(len=16) :: nomelt
    common /ffauto/ nomelt
! ---------------------------------------------------------------------
!
!
!      ELEMENT P1 - CONTINU
!
    do 110 n = 1, nno2
        do 120 i = 1, ndim
            iu(nno1*(i-1)+n) = i + (n-1)*(ndim+1)
120      continue
        ia(n) = 1 + ndim + (n-1)*(ndim+1)
110  continue
    os = (1+ndim)*nno2
    do 140 n = 1, nno1-nno2
        do 150 i = 1, ndim
            iu(nno1*(i-1)+n+nno2) = i + (n-1)*ndim + os
150      continue
140  continue
!
!
!
end subroutine
