subroutine gdjrg0(kp, nno, enprim, x00, y0,&
                  ajacob, rot0)
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
!
! FONCTION: POUR UN ELEMENT DE POUTRE EN GRAND DEPLACEMENT, CALCULE, AUX
!           POINTS DE GAUSS, LE JACOBIEN ET LA MATRICE DE ROTATION DES
!           AXES PRINCIPAUX D'INERTIE EN POSITION DE REFERENCE, PAR RAP-
!           PORT AUX AXES DE COORDONNEES GENERAUX.
!
!     IN  : KP        : NUMERO DU POINT DE GAUSS
!           NNO       : NOMBRE DE NOEUDS
!           ENPRIM    : DERIVEES DES FONCTIONS DE FORME
!           X00       : COORDONNEES DES NOEUDS EN POSITION DE REFERENCE
!           Y0        : VECTEUR DE COMPOSANTES ALPHA, BETA ET GAMMA:
!                       ANGLES NAUTIQUES DES AXES PRINCIPAUX PAR RAPPORT
!                       AUX AXES GENERAUX.
!
!     OUT : AJACOB : JACOBIEN
!           ROT0   : MATRICE DE ROTATION
! ------------------------------------------------------------------
    implicit none
    include 'asterfort/matrot.h'
    include 'asterfort/transp.h'
    include 'blas/ddot.h'
    real(kind=8) :: enprim(3, 2), x00(3, 3), y0(3), rot(3, 3), rot0(3, 3), e1(3)
!
!-----------------------------------------------------------------------
    integer :: ic, kp, ne, nno
    real(kind=8) :: ajacob, zero
!-----------------------------------------------------------------------
    zero = 0.d0
    do 2 ic = 1, 3
        e1(ic) = zero
        do 1 ne = 1, nno
            e1(ic) = e1(ic) + enprim(ne,kp)*x00(ic,ne)
 1      end do
 2  end do
!
    ajacob=ddot(3,e1,1,e1,1)
    ajacob = sqrt(ajacob)
!
    call matrot(y0, rot)
    call transp(rot, 3, 3, 3, rot0,&
                3)
!
end subroutine
