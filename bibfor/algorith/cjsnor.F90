subroutine cjsnor(mater, sig, x, nor, devnul,&
                  trac)
!
!       ================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   CALCUL  UNE ESTIMATION UN VECTEUR PARALLELE A DF/DQ
!           OU DF EST LE SEUIL DEVIATOIRE ET Q LE TENSEUR Q
!                     Q = S-X*I1
!   IN :
!       MATER : MATERIAU
!       SIG   : CONTRAINTES
!       X     : VARIABLES INTERNES CINEMATIQUES
!   OUT :
!       NOR   : ESTIMATION DE LA DITRECTION DE LA NORMALE
!               A LA SURFACE DEVIATOIRE DANS LE PLAN DEVIATOIRE
!               PERPENDICULAIRE A LA TRISECTRICE
!               LE VECTEUR NOR(1:NDT) N EST PAS NORME
!               SA NORME EST NOR(NDT+1)
!    DEVNUL   : VRAI SI DEVIATEUR DE Q NUL
!    TRAC     : VRAI SI I1  NUL
!
    implicit none
!
!
#include "asterf_types.h"
#include "asterfort/cjsc3q.h"
#include "asterfort/cjst.h"
    real(kind=8) :: mater(14, 2), sig(6), x(6), nor(7)
    aster_logical :: devnul, trac
    real(kind=8) :: zero, deux, six
    parameter     ( zero   = 0.d0   )
    parameter     ( deux   = 2.d0   )
    parameter     ( six    = 6.d0   )
!
    real(kind=8) :: g, pa, qinit, q(6), tq(6), coef, qii, cos3tq, trav, trav2
    integer :: i
    integer :: ndt, ndi
    common /tdim/   ndt, ndi
!
!
!
!
!-----------------------------------------------------------------------
!->     PROPRIETES CJS MATERIAU
!------------------------------
    g = mater(9,2)
    pa = mater(12,2)
    qinit = mater(13,2)
!-----------------------------------------------------------------------
!->    Q QII ET COS3TQ
!-----------------------------------------------------------------------
!
    call cjsc3q(sig, x, pa, qinit, q,&
                qii, cos3tq, devnul, trac)
!-----------------------------------------------------------------------
!->    TQ = DET(Q)*INV(Q)
!-----------------------------------------------------------------------
    call cjst(q, tq)
!
!
    coef = sqrt(six)*g/qii
    trav2 = zero
    do 10 i = 1, ndt
        trav = (deux+g*cos3tq)*q(i)+coef*tq(i)
        trav2 = trav2+trav*trav
        nor(i) = trav
 10 continue
    nor(ndt+1) = sqrt(trav2)
end subroutine
