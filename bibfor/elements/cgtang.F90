subroutine cgtang(ndim, nno, npg, geom, dffr,&
                  t)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
    integer :: nno, ndim, npg
    real(kind=8) :: geom(ndim, nno), dffr(nno, npg), t(3, 3)
!
!--------------------------------------------------
!  DEFINITION DES TANGENTES POUR UNE VARIETE 1D PLONGEE DANS DU 3D
!
!  IN  : NDIM : DIMENSION DE L'ESPACE
!        NNO  : NOMBRE DE NOEUDS
!        NPG  : NOMBRE DE POINTS DE GAUSS
!        GEOM : COORDONNEES DES NOEUDS
!        DFFR : DERIVEES DES FONCTIONS DE FORME
!  OUT : T    : TANGENTE AUX NOEUDS
!--------------------------------------------------
    integer :: ig, i, n
    real(kind=8) :: nor
!
!
    do ig = 1, npg
        do 20 i = 1, ndim
            t(i,ig)=0.d0
            do 30 n = 1, nno
                t(i,ig)=t(i,ig)+geom(i,n)*dffr(n,ig)
30          continue
20      continue
        nor=0.d0
        do 25 i = 1, ndim
            nor=nor+t(i,ig)**2
25      continue
        nor=sqrt(nor)
        do 27 i = 1, ndim
            t(i,ig)=t(i,ig)/nor
27      continue
    end do
!
!
!
end subroutine
