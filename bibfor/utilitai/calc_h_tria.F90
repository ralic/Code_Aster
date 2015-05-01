subroutine calc_h_tria(ino, x3d1, x3d2, x3d3, h)
    implicit none
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!  DESCRIPTION : 
!  -----------   
!       CALCUL DE LA HAUTEUR H RELATIVE AU NOEUD INO DU TRIANGLE DEFINI
!       PAR LES POINTS X3D1, X3D2, X3D3
!
!
!-------------------   DECLARATION DES VARIABLES   ---------------------
!
!
#include "asterfort/normev.h"
! ARGUMENTS
! ---------
    integer :: ino
    real(kind=8) :: x3d1(3), x3d2(3), x3d3(3),h 
!
! VARIABLES LOCALES
! -----------------
    real(kind=8) :: x3d(3,3), vect_jk(3), norme_jk, proj, vect_ji(3)
    integer :: jno, kno
!
!
!-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
!
!
    x3d(1,1) = x3d1(1)
    x3d(2,1) = x3d1(2)
    x3d(3,1) = x3d1(3)
!
    x3d(1,2) = x3d2(1)
    x3d(2,2) = x3d2(2)
    x3d(3,2) = x3d2(3)
!
    x3d(1,3) = x3d3(1)
    x3d(2,3) = x3d3(2)
    x3d(3,3) = x3d3(3)
!   
    jno = mod(ino+1,3)
    if (jno.eq.0) jno = 3
    kno = mod(ino+2,3)
    if (kno.eq.0) kno = 3
!
!   vecteur directeur du coté opposé au noeud
    vect_jk(1) = x3d(1,kno) - x3d(1,jno)
    vect_jk(2) = x3d(2,kno) - x3d(2,jno)
    vect_jk(3) = x3d(3,kno) - x3d(3,jno)
!
    call normev(vect_jk, norme_jk)
!   
    vect_ji(1) = x3d(1,ino) - x3d(1,jno)
    vect_ji(2) = x3d(2,ino) - x3d(2,jno)
    vect_ji(3) = x3d(3,ino) - x3d(3,jno)
!
    proj = (vect_jk(1)*vect_ji(1)+vect_jk(2)*vect_ji(2)+vect_jk(3)*vect_ji(3))
!    
    vect_ji(1) = vect_ji(1) - proj*vect_jk(1)
    vect_ji(2) = vect_ji(2) - proj*vect_jk(2)
    vect_ji(3) = vect_ji(3) - proj*vect_jk(3)
!
    call normev(vect_ji, h)
    
end subroutine
