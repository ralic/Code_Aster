subroutine resr3d(rota, coor, ff, rho, nno,&
                  npg, frx, fry, frz)
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
!
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DE LA FORCE 3D DUE A UN TERME DE
!                          ROTATION
!                          POUR L'OPTION : 'ERME_ELEM'
!                             (ESTIMATEUR EN RESIDU)
!
!    - ARGUMENTS:
!        DONNEES:
!                  ROTA      -->  TABLEAU OME , AR , BR , CR
!                  COOR      -->  COORDONNEES DES NOEUDS
!                  FF        -->  FONCTIONS DE FORME AUX POINTS DE GAUSS
!                  RHO       -->  DENSITE
!                  NNO       -->  NOMBRE DE NOEUDS
!                  NPG       -->  NOMBRE DE POINTS DE GAUSS
!
!        SORTIE :  FRX       -->  FORCE AU POINT DE GAUSS EN X
!                  FRY       -->  FORCE AU POINT DE GAUSS EN Y
!                  FRZ       -->  FORCE AU POINT DE GAUSS EN Z
! ......................................................................
!
    real(kind=8) :: rota(*), coor(1), ff(1)
    real(kind=8) :: fx(27), fy(27), fz(27)
    real(kind=8) :: frx(27), fry(27), frz(27)
    real(kind=8) :: omo, omm, om1, om2, om3
    integer :: npg, nno, i, k, kp
!
!-----------------------------------------------------------------------
    real(kind=8) :: rho
!-----------------------------------------------------------------------
    omm = rota(1) * rota(1)
    om1 = rota(1) * rota(2)
    om2 = rota(1) * rota(3)
    om3 = rota(1) * rota(4)
    do 100 i = 1, nno
        omo = om1 * coor(3*i-2) + om2 * coor(3*i-1)+ om3 * coor(3*i)
        fx(i) = omm * coor(3*i-2) - omo * om1
        fy(i) = omm * coor(3*i-1) - omo * om2
        fz(i) = omm * coor(3*i) - omo * om3
100  end do
!
    do 200 kp = 1, npg
        k=(kp-1)*nno
        frx(kp) = 0.d0
        fry(kp) = 0.d0
        frz(kp) = 0.d0
        do 150 i = 1, nno
            frx(kp) = frx(kp) + fx(i) * ff(k+i)
            fry(kp) = fry(kp) + fy(i) * ff(k+i)
            frz(kp) = frz(kp) + fz(i) * ff(k+i)
150      continue
        frx(kp) = rho * frx(kp)
        fry(kp) = rho * fry(kp)
        frz(kp) = rho * frz(kp)
200  end do
end subroutine
