subroutine ermev3(nno, ipg, ivf, isig, nbcmp,&
                  dfdx, dfdy, dfdz, dsx, dsy,&
                  dsz, norme)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! =====================================================================
!  ERREUR EN MECANIQUE - TERME VOLUMIQUE - DIMENSION 3
!  **        **                *                     *
! =====================================================================
!     BUT:
!         PREMIER TERME DE L'ESTIMATEUR D'ERREUR EN RESIDU EXPLICITE :
!         CALCUL DE LA DIVERGENCE ET DE LA NORME DE SIGMA EN UN POINT
!         DE GAUSS EN 3D.
!
!
!     ARGUMENTS:
!     ----------
!
!      ENTREE :
!-------------
! IN   NNO    : NOMBRE DE NOEUDS DE LA_MAILLE VOLUMIQUE
! IN   IPG    : NUMERO DU POINT DE GAUSS
! IN   IVF    : ADRESSE DANS ZR DU TABLEAU DES FONCTIONS DE FORME
! IN   ISIG   : ADRESSE DANS ZR DU TABLEAU DES CONTRAINTES AUX NOEUDS
! IN   NBCMP  : NOMBRE DE COMPOSANTES
! IN   DFDX   : DERIVEES DES FONCTIONS DE FORME / X
! IN   DFDY   : DERIVEES DES FONCTIONS DE FORME / Y
! IN   DFDZ   : DERIVEES DES FONCTIONS DE FORME / Z
!
!      SORTIE :
!-------------
! OUT  DSX    : PREMIERE COMPOSANTE DE DIVERGENCE SIGMA
! OUT  DSY    : SECONDE COMPOSANTE DE DIVERGENCE SIGMA
! OUT  DSZ    : TROISEME COMPOSANTE DE DIVERGENCE SIGMA
! OUT  NORME  : NORME DE SIGMA AU POINT DE GAUSS
!
! ......................................................................
!
    implicit none
    include 'jeveux.h'
    integer :: nno, ipg, ivf, isig, nbcmp
    real(kind=8) :: dfdx(27), dfdy(27), dfdz(27), dsx, dsy, dsz, norme
!
!
!
!
    integer :: i, k
!
    real(kind=8) :: dsig11, dsig12, dsig13
    real(kind=8) :: dsig21, dsig22, dsig23
    real(kind=8) :: dsig31, dsig32, dsig33
    real(kind=8) :: spg11, spg22, spg33, spg12, spg13, spg23
    real(kind=8) :: sig11, sig22, sig33, sig12, sig13, sig23
!
! ----------------------------------------------------------------------
!
    dsig11=0.d0
    dsig12=0.d0
    dsig13=0.d0
    dsig21=0.d0
    dsig22=0.d0
    dsig23=0.d0
    dsig31=0.d0
    dsig32=0.d0
    dsig33=0.d0
!
    spg11=0.d0
    spg22=0.d0
    spg33=0.d0
    spg12=0.d0
    spg13=0.d0
    spg23=0.d0
!
    do 10 i = 1, nno
        k=(ipg-1)*nno
        sig11=zr(isig-1+nbcmp*(i-1)+1)
        sig22=zr(isig-1+nbcmp*(i-1)+2)
        sig33=zr(isig-1+nbcmp*(i-1)+3)
        sig12=zr(isig-1+nbcmp*(i-1)+4)
        sig13=zr(isig-1+nbcmp*(i-1)+5)
        sig23=zr(isig-1+nbcmp*(i-1)+6)
!
        dsig11=dsig11+sig11*dfdx(i)
        dsig12=dsig12+sig12*dfdy(i)
        dsig13=dsig13+sig13*dfdz(i)
        dsig21=dsig21+sig12*dfdx(i)
        dsig22=dsig22+sig22*dfdy(i)
        dsig23=dsig23+sig23*dfdz(i)
        dsig31=dsig31+sig13*dfdx(i)
        dsig32=dsig32+sig23*dfdy(i)
        dsig33=dsig33+sig33*dfdz(i)
!
        spg11=spg11+sig11*zr(ivf+k+i-1)
        spg22=spg22+sig22*zr(ivf+k+i-1)
        spg33=spg33+sig33*zr(ivf+k+i-1)
        spg12=spg12+sig12*zr(ivf+k+i-1)
        spg13=spg13+sig13*zr(ivf+k+i-1)
        spg23=spg23+sig23*zr(ivf+k+i-1)
!
10  end do
!
    dsx=dsig11+dsig12+dsig13
    dsy=dsig21+dsig22+dsig23
    dsz=dsig31+dsig32+dsig33
!
    norme=spg11**2+spg22**2+spg33**2+&
     &      2.d0*(spg12**2+spg13**2+spg23**2)
!
end subroutine
