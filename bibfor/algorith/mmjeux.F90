subroutine mmjeux(alias, nno, ndim, coorma, ksi1,&
                  ksi2, coorpt, jeupm, dist)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterfort/mmcoor.h"
    character(len=8) :: alias
    integer :: nno, ndim
    real(kind=8) :: coorma(27)
    real(kind=8) :: coorpt(3), dist(3)
    real(kind=8) :: jeupm
    real(kind=8) :: ksi1, ksi2
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - APPARIEMENT)
!
! CALCUL DE LA DISTANCE ENTRE POINT ET SA PROJECTION SUR LA
! MAILLE
!
! ----------------------------------------------------------------------
!
!
! IN  ALIAS  : TYPE DE MAILLE
! IN  NNO    : NOMBRE DE NOEUD SUR LA MAILLE
! IN  NDIM   : DIMENSION DE LA MAILLE (2 OU 3)
! IN  COORMA : COORDONNEES DES NOEUDS DE LA MAILLE
! IN  COORPT : COORDONNEES DU POINT
! IN  KSI1   : PREMIERE COORDONNEE PARAMETRIQUE DE LA PROJECTION DU
!              POINT SUR LA MAILLE
! IN  KSI2   : SECONDE COORDONNEE PARAMETRIQUE DE LA PROJECTION DU
!              POINT SUR LA MAILLE
! OUT JEUPM  : DISTANCE ENTRE POINT ET SA PROJECTION SUR LA MAILLE
! OUT DIST   : VECTEUR RELIANT LE POINT AVEC SON PROJETE
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: coorpr(3)
    integer :: idim, ndimg
    real(kind=8) :: zero
    parameter    (zero=0.d0)
!
! ----------------------------------------------------------------------
!
    ndimg = 3
!
    do 10 idim = 1, ndimg
        dist(idim) = zero
10  end do
!
! --- COORDONNEES DU PROJETE
!
    call mmcoor(alias, nno, ndim, coorma, ksi1,&
                ksi2, coorpr)
!
! --- DISTANCE POINT DE CONTACT/PROJECTION
!
    do 140 idim = 1, ndimg
        dist(idim) = coorpr(idim) - coorpt(idim)
140  end do
!
! --- JEUPM
!
    jeupm = sqrt(dist(1)**2+dist(2)**2+dist(3)**2)
!
end subroutine
