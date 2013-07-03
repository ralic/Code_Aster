subroutine i2trgi(t1, t2, n2, pt)
    implicit none
!
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
#include "asterfort/i2rdli.h"
    integer :: t1(*), t2(*), n2, pt
!
!**********************************************************************
!
!  OPERATION REALISEE
!  ------------------
!
!     RANGEMENT DES ELEMENTS DU TABLEAU T2 DE DIMENSION N2 DANS LE
!     TABLEAU T1.
!
!  INVARIANT DE ROUTINE
!  --------------------
!
!     TOUS LES ELEMENTS DE T1 SONT DISTINCTS ET TRIES DANS L' ORDRE
!     CROISSANT ; PT EST L' ADRESSE DE LA PREMIERE CASE LIBRE
!
!**********************************************************************
!
    integer :: i, val
!
    do 10, i = 1, n2, 1
!
    val = t2(i)
!
    call i2rdli(val, t1, pt)
!
    10 end do
!
end subroutine
