subroutine vff2dn(ndim, nno, ipg, ipoids, idfde,&
                  coor, nx, ny, jac)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    integer :: ndim, nno, ipoids, idfde, ipg, i, k
    real(kind=8) :: dx, coor(1), nx, ny, jac, dxds, dyds
! ......................................................................
!    - BUT:  CALCULER LA VALEUR DU POIDS D'INTEGRATION EN 1 POINT DE
!            GAUSS POUR UN SEGMENT PLAN  A 2 OU 3 NOEUDS.
!      CALCULE AUSSI LA NORMALE AU SEGMENT AU POINT DE GAUSS.
!
!    - ARGUMENTS:
!        DONNEES:
!        IPG      -->  NUMERO DU POINT DE GAUSS
!        NDIM     -->  1 (SEGMENT)
!        IPOIDS   -->  ADRESSE DES POIDS DE GAUSS
!        NNO      -->  NOMBRE DE NOEUDS
!        DFDE     -->  ADRESSES DES DERIVEES DES FONCTIONS DE FORME
!        COOR     -->  COORDONNEES DES NOEUDS
!
!        RESULTATS:    NX,NY    <--  COMPOSANTES DE LA NORMALE
!                      JAC      <--  PRODUIT DU JACOBIEN ET DU POIDS
!
!  REMARQUE :
!    - LES SEGMENTS DOIVENT ETRE "PLANS" (DANS OXY)
! ......................................................................
!
!
    call assert(ndim.eq.1)
    dxds = 0.d0
    dyds = 0.d0
    do 1 i = 1, nno
        k = nno*(ipg-1)
        dx = zr(idfde-1+k+i)
        dxds = dxds + dx * coor(2*i-1)
        dyds = dyds + dx * coor(2*i)
 1  end do
    jac = sqrt(dxds**2 + dyds**2)
    nx = dyds/jac
    ny = -dxds/jac
    jac = zr(ipoids + ipg-1) * jac
end subroutine
