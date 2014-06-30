subroutine dxtfor(global, xyzl, pgl, for, vecl)
    implicit  none
#include "asterc/r8dgrd.h"
#include "asterfort/coqrep.h"
#include "asterfort/gtria3.h"
#include "asterfort/jevech.h"
    real(kind=8) :: xyzl(3, *), pgl(3, *), for(6, *), vecl(*)
    logical(kind=1) :: global
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
!     ------------------------------------------------------------------
!     CHARGEMENT FORCE_FACE DES ELEMENTS DE PLAQUE DKT ET DST
!     ------------------------------------------------------------------
!     IN  GLOBAL : VARIABLE LOGIQUE DE REPERE GLOBAL OU LOCAL
!     IN  XYZL   : COORDONNEES LOCALES DES TROIS NOEUDS
!     IN  PGL    : MATRICE DE PASSAGE GLOBAL - LOCAL
!     IN  FOR    : FORCE APPLIQUEE SUR LA FACE
!     OUT VECL   : CHARGEMENT NODAL RESULTANT
!     ------------------------------------------------------------------
#include "jeveux.h"
!
    integer :: i, nno, jcara
    real(kind=8) :: aire, alpha, beta
    real(kind=8) :: fx, fy, carat3(21), t2iu(4), t2ui(4), c, s
!     ------------------------------------------------------------------
    nno = 3
!     ----- CALCUL DES GRANDEURS GEOMETRIQUES SUR LE TRIANGLE ----------
    call gtria3(xyzl, carat3)
!
    call jevech('PCACOQU', 'L', jcara)
    alpha = zr(jcara+1) * r8dgrd()
    beta = zr(jcara+2) * r8dgrd()
    call coqrep(pgl, alpha, beta, t2iu, t2ui,&
                c, s)
!
    if (.not.global) then
        do 10 i = 1, nno
            fx = for(1,i)
            fy = for(2,i)
            for(1,i) = t2iu(1)*fx + t2iu(3)*fy
            for(2,i) = t2iu(2)*fx + t2iu(4)*fy
            fx = for(4,i)
            fy = for(5,i)
            for(4,i) = t2iu(1)*fx + t2iu(3)*fy
            for(5,i) = t2iu(2)*fx + t2iu(4)*fy
10      continue
    endif
!
    aire = carat3(8)
!
    do 20 i = 1, 6*nno
        vecl(i) = 0.d0
20  end do
!
    do 30 i = 1, 6
        vecl(i ) = for(i,1)*aire/3.d0
        vecl(i+6 ) = for(i,2)*aire/3.d0
        vecl(i+12) = for(i,3)*aire/3.d0
30  end do
!
end subroutine
