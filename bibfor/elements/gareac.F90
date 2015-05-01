subroutine gareac(xdm, xdp, dgamma)
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
#include "jeveux.h"
#include "asterc/r8miem.h"
#include "asterc/r8prem.h"
#include "asterfort/angvx.h"
#include "asterfort/matrot.h"
#include "asterfort/normev.h"
#include "asterfort/promat.h"
#include "asterfort/provec.h"
#include "asterfort/trigom.h"
#include "asterfort/vdiff.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/ddot.h"
    real(kind=8) :: xdm(3), xdp(3), dgamma
!
    real(kind=8) :: xdmnor(3), xdpnor(3), dd(3)
    real(kind=8) :: normm, normp
    real(kind=8) :: ytr(3), ztr(3)
    real(kind=8) :: ptr2gl(3, 3), pstrx, pstry, ptrbtr(3, 3)
    real(kind=8) :: anglm(3), pglm(3, 3), vtemp(3), pslty, psltz, plo2tr(3, 3)
    real(kind=8) :: ytemp(3), ylocp(3)
    real(kind=8) :: anglp(3), pglp(3, 3)
    real(kind=8) :: pscal, norm
!
! ----------------------------------------------------------------------
!
! --- SI L'ORIENTATION N'A PAS CHANGE ==> DGAMMA = 0
    call dcopy(3, xdm, 1, xdmnor, 1)
    call normev(xdmnor, normm)
    call dcopy(3, xdp, 1, xdpnor, 1)
    call normev(xdpnor, normp)
    call vdiff(3, xdpnor, xdmnor, dd)
    if (abs(ddot(3,dd,1,dd,1)) .lt. r8prem()) then
        dgamma = 0.d0
        goto 9999
    endif
!
! --- CONSTRUCTION DE LA MATRICE DE PASSAGE DU REPERE GLOBAL
!     VERS (XDMNOR,YTR,ZTR)
!     YTR EST NORMAL A XDMNOR DANS LE PLAN FORME PAR (XDMNOR,XDPNOR)
!     ZTR EST NORMAL AU PLAN FORME PAR (XDMNOR,XDPNOR)
!
    pscal = ddot(3,xdpnor,1,xdmnor,1)
    call dcopy(3, xdpnor, 1, ytr, 1)
    call daxpy(3, -pscal, xdmnor, 1, ytr,&
               1)
    call normev(ytr, norm)
    if (norm .le. r8miem()) then
        dgamma = 0.d0
        goto 9999
    endif
    call provec(xdmnor, ytr, ztr)
!
    ptr2gl(1,1) = xdmnor(1)
    ptr2gl(2,1) = xdmnor(2)
    ptr2gl(3,1) = xdmnor(3)
    ptr2gl(1,2) = ytr(1)
    ptr2gl(2,2) = ytr(2)
    ptr2gl(3,2) = ytr(3)
    ptr2gl(1,3) = ztr(1)
    ptr2gl(2,3) = ztr(2)
    ptr2gl(3,3) = ztr(3)
!
! --- CONSTRUCTION DE LA MATRICE DE PASSAGE DE (XDMNOR,YTR,ZTR)
!     VERS (XDPNOR,YDPNOR,ZTR)
!     IL S'AGIT DE LA ROTATION AUTOUR DE ZTR
!     YDPNOR EST NORMAL A XDPNOR DANS LE PLAN NORMAL A ZTR
!
    pstrx = ddot(3,xdmnor,1,xdpnor,1)
    pstry = ddot(3,ytr ,1,xdpnor,1)
!
    ptrbtr(1,1) = pstrx
    ptrbtr(1,2) = -pstry
    ptrbtr(1,3) = 0.d0
    ptrbtr(2,1) = pstry
    ptrbtr(2,2) = pstrx
    ptrbtr(2,3) = 0.d0
    ptrbtr(3,1) = 0.d0
    ptrbtr(3,2) = 0.d0
    ptrbtr(3,3) = 1.d0
!
! --- CONSTRUCTION DE LA MATRICE DE PASSAGE DE (XDMNOR,YTR,ZTR)
!     VERS (XDMNOR,YLOCM,ZLOCM)
!     IL S'AGIT DE LA ROTATION AUTOUR DE XDMNOR
!     (YLOCM,ZLOCM) CONSTITUE LE REPERE LOCAL A T- POUR GAMMA=0
!
    call angvx(xdm, anglm(1), anglm(2))
    anglm(3) = 0.d0
    call matrot(anglm, pglm)
    vtemp(1) = pglm(2,1)
    vtemp(2) = pglm(2,2)
    vtemp(3) = pglm(2,3)
    pslty = ddot(3,ytr,1,vtemp,1)
    vtemp(1) = pglm(3,1)
    vtemp(2) = pglm(3,2)
    vtemp(3) = pglm(3,3)
    psltz = ddot(3,ytr,1,vtemp,1)
!
    plo2tr(1,1) = 1.d0
    plo2tr(1,2) = 0.d0
    plo2tr(1,3) = 0.d0
    plo2tr(2,1) = 0.d0
    plo2tr(2,2) = pslty
    plo2tr(2,3) = psltz
    plo2tr(3,1) = 0.d0
    plo2tr(3,2) = -psltz
    plo2tr(3,3) = pslty
!
! --- CALCUL DE YLOC A T+
!
!     PTR2GL * PTRBTR * PLO2TR *  (0 1 0)T
    vtemp(1) = plo2tr(1,2)
    vtemp(2) = plo2tr(2,2)
    vtemp(3) = plo2tr(3,2)
    call promat(ptrbtr, 3, 3, 3, vtemp,&
                3, 3, 1, ytemp)
    call promat(ptr2gl, 3, 3, 3, ytemp,&
                3, 3, 1, ylocp)
!
! --- CALCUL DE LA BASE LOCALE A T+ EN CONSIDERANT GAMMA NUL
    call angvx(xdp, anglp(1), anglp(2))
    anglp(3) = 0.d0
    call matrot(anglp, pglp)
!     LES LIGNES DE PGLP SONT LES 3 VECTEURS DE LA BASE LOCAL
!
    vtemp(1) = pglp(2,1)
    vtemp(2) = pglp(2,2)
    vtemp(3) = pglp(2,3)
    pscal = ddot(3,ylocp,1,vtemp,1)
!
    dgamma = trigom('ACOS',pscal)
    vtemp(1) = pglp(3,1)
    vtemp(2) = pglp(3,2)
    vtemp(3) = pglp(3,3)
    if (ddot(3,ylocp,1,vtemp,1) .lt. 0.d0) then
        dgamma = -dgamma
    endif
!
9999  continue
!
end subroutine
