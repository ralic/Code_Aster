subroutine dxqfor(global, xyzl, pgl, for, vecl)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "jeveux.h"
!
#include "asterc/r8dgrd.h"
#include "asterfort/coqrep.h"
#include "asterfort/gquad4.h"
#include "asterfort/jevech.h"
    logical :: global
    real(kind=8) :: xyzl(3, *), pgl(3, *)
    real(kind=8) :: for(6, *)
    real(kind=8) :: vecl(*)
!     ------------------------------------------------------------------
!     CHARGEMENT FORCE_FACE DES ELEMENTS DE PLAQUE DKQ ET DSQ
!     ------------------------------------------------------------------
!     IN  GLOBAL : VARIABLE LOGIQUE DE REPERE GLOBAL OU LOCAL
!     IN  XYZL   : COORDONNEES LOCALES DES QUATRE NOEUDS
!     IN  PGL    : MATRICE DE PASSAGE GLOBAL - LOCAL
!     IN  FOR    : FORCE APPLIQUE SUR LA FACE
!     OUT VECL   : CHARGEMENT NODAL RESULTANT
!     ------------------------------------------------------------------
    real(kind=8) :: airetr(4), c1, c2, fno(6, 4, 4)
    real(kind=8) :: fx, fy, alpha, beta
    real(kind=8) :: t2iu(4), t2ui(4), caraq4(25), c, s
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ino, it, j, k, nno, jcara
!-----------------------------------------------------------------------
    nno = 4
!
!     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE QUADRANGLE --------
    call gquad4(xyzl, caraq4)
!
    call jevech('PCACOQU', 'L', jcara)
    alpha = zr(jcara+1) * r8dgrd()
    beta = zr(jcara+2) * r8dgrd()
    call coqrep(pgl, alpha, beta, t2iu, t2ui,&
                c, s)
!
    if (.not. global) then
        do 50 i = 1, nno
            fx = for(1,i)
            fy = for(2,i)
            for(1,i) = t2iu(1)*fx + t2iu(3)*fy
            for(2,i) = t2iu(2)*fx + t2iu(4)*fy
            fx = for(4,i)
            fy = for(5,i)
            for(4,i) = t2iu(1)*fx + t2iu(3)*fy
            for(5,i) = t2iu(2)*fx + t2iu(4)*fy
50      continue
    endif
!
    do 100 ino = 1, nno
        airetr(ino) = caraq4(21+ino)
100  end do
!
    do 110 i = 1, 6
        do 111 j = 1, nno
            do 112 k = 1, nno
                fno(i,j,k) = 0.d0
112          continue
111      continue
110  end do
!
    do 120 i = 1, 6*nno
        vecl(i) = 0.d0
120  end do
!
    c1 = 1.d0 / 6.d0
    c2 = 1.d0 / 12.d0
!
    do 200 i = 1, 6
        fno(i,1,1) = (c1*for(i,1)+c2*for(i,2)+c2*for(i,4)) * airetr(1)
        fno(i,1,2) = (c2*for(i,1)+c1*for(i,2)+c2*for(i,4)) * airetr(1)
        fno(i,1,4) = (c2*for(i,1)+c2*for(i,2)+c1*for(i,4)) * airetr(1)
        fno(i,2,2) = (c1*for(i,2)+c2*for(i,3)+c2*for(i,1)) * airetr(2)
        fno(i,2,3) = (c2*for(i,2)+c1*for(i,3)+c2*for(i,1)) * airetr(2)
        fno(i,2,1) = (c2*for(i,2)+c2*for(i,3)+c1*for(i,1)) * airetr(2)
        fno(i,3,3) = (c1*for(i,3)+c2*for(i,4)+c2*for(i,2)) * airetr(3)
        fno(i,3,4) = (c2*for(i,3)+c1*for(i,4)+c2*for(i,2)) * airetr(3)
        fno(i,3,2) = (c2*for(i,3)+c2*for(i,4)+c1*for(i,2)) * airetr(3)
        fno(i,4,4) = (c1*for(i,4)+c2*for(i,1)+c2*for(i,3)) * airetr(4)
        fno(i,4,1) = (c2*for(i,4)+c1*for(i,1)+c2*for(i,3)) * airetr(4)
        fno(i,4,3) = (c2*for(i,4)+c2*for(i,1)+c1*for(i,3)) * airetr(4)
        do 160 ino = 1, nno
            do 150 it = 1, nno
                vecl(i+6*(ino-1)) = vecl(i+6*(ino-1)) + fno(i,it,ino)
150          continue
            vecl(i+6*(ino-1)) = vecl(i+6*(ino-1)) / 2.d0
160      continue
200  end do
!
end subroutine
