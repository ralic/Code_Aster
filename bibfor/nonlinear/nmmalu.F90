subroutine nmmalu(nno, axi, r, vff, dfdi,&
                  lij)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    include 'asterfort/r8inir.h'
    include 'blas/dcopy.h'
    include 'blas/dscal.h'
    logical :: axi
    integer :: nno, lij(3, 3)
    real(kind=8) :: dfdi(nno, 4), vff(nno), r
!
! ----------------------------------------------------------------------
!                     CALCUL DE LA MATRICE L :  GRAD(U) = L.U
!
! IN  NNO     : NOMBRE DE NOEUDS
! IN  AXI     : .TRUE. SI AXISYMETRIQUE
! IN  R       : RAYON AU POINT DE GAUSS CONSIDERE (UTILE EN AXI)
! IN  VFF     : FONCTIONS DE FORME AU POINT DE GAUSS CONSIDERE
! VAR DFDI    : IN  : DERIVEE DES FONCTIONS DE FORME
!               OUT : MATRICE L (AVEC LIJ)
!                --> DDL (N,I) ET VAR J  -> DFDI(N,LIJ(I,J))
! OUT LIJ     : INDIRECTION POUR ACCEDER A L
! ----------------------------------------------------------------------
!
!
!    CAS 2D OU 3D STANDARD
    if (.not. axi) then
        lij(1,1) = 1
        lij(1,2) = 2
        lij(1,3) = 3
        lij(2,1) = 1
        lij(2,2) = 2
        lij(2,3) = 3
        lij(3,1) = 1
        lij(3,2) = 2
        lij(3,3) = 3
        goto 9999
    endif
!
! CAS AXISYMETRIQUE
    lij(1,1) = 1
    lij(1,2) = 2
    lij(1,3) = 4
    lij(2,1) = 1
    lij(2,2) = 2
    lij(2,3) = 4
    lij(3,1) = 4
    lij(3,2) = 4
    lij(3,3) = 3
!
!    TERME EN N/R : DERIVATION 3,3
    call dcopy(nno, vff, 1, dfdi(1, 3), 1)
    call dscal(nno, 1/r, dfdi(1, 3), 1)
!
!    TERME NUL : DERIVATION 1,3  2,3  3,1  3,2
    call r8inir(nno, 0.d0, dfdi(1, 4), 1)
!
9999  continue
end subroutine
