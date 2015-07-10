subroutine mmtole(alias, nno, ndim, coorma, toleou,&
                  ksi1, ksi2, tau1, tau2, iproj)
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
#include "asterfort/cfadju.h"
#include "asterfort/mmdonf.h"
#include "asterfort/mmtang.h"
    character(len=8) :: alias
    integer :: nno
    integer :: ndim
    real(kind=8) :: coorma(27)
    real(kind=8) :: ksi1, ksi2
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: toleou
    integer :: iproj
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - APPARIEMENT)
!
! RAMENE POINT PROJETE HORS DE LA MAILLE
!
! ----------------------------------------------------------------------
!
! IN  ALIAS  : TYPE DE MAILLE
! IN  NNO    : NOMBRE DE NOEUD SUR LA MAILLE
! IN  NDIM   : DIMENSION DE LA MAILLE (2 OU 3)
! IN  COORMA : COORDONNEES DES NOEUDS DE LA MAILLE
! IN  TOLEOU : TOLERANCE POUR LE PROJETE HORS SURFACE MAITRE
! I/O KSI1   : PREMIERE COORDONNEE PARAMETRIQUE DU POINT PROJETE
! I/O KSI2   : SECONDE COORDONNEE PARAMETRIQUE DU POINT PROJETE
! OUT TAU1   : PREMIER VECTEUR TANGENT EN (KSI1,KSI2)
! OUT TAU2   : SECOND VECTEUR TANGENT EN (KSI1,KSI2)
! OUT IPROJ  : VAUT 0 SI POINT PROJETE DANS L'ELEMENT
!                   1 SI POINT PROJETE DANS LA ZONE DEFINIE PAR TOLEOU
!                   2 SI POINT PROJETE EN DEHORS (EXCLUS)
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: dff(2, 9)
!
! ----------------------------------------------------------------------
!
!
!
! --- AJUSTEMENT PROJECTION HORS ZONE
!
    call cfadju(alias, ksi1, ksi2, toleou, iproj)
!
! --- CALCUL DES DERIVEES DES FONCTIONS DE FORME
!
    call mmdonf(ndim, nno, alias, ksi1, ksi2,&
                dff)
!
! --- RE-CALCUL DES TANGENTES APRES AJUSTEMENT
!
    call mmtang(ndim, nno, coorma, dff, tau1,&
                tau2)
!
end subroutine
