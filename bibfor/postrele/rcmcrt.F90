subroutine rcmcrt(symax, sigm, stlin, stpar)
    implicit   none
    include 'asterc/r8vide.h'
    include 'asterfort/u2mesr.h'
    real(kind=8) :: symax, sigm, stlin, stpar
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ------------------------------------------------------------------
!
!     OPERATEUR POST_RCCM
!     CALCUL DU CRITERE DU ROCHET THERMIQUE
!
! IN  : SYMAX : LIMITE ELASTIQUE
! IN  : SIGM  : MAXIMUM DE LA CONTRAINTE DE MEMBRANE DUE A LA PRESSION
! OUT : STLIN : VALEUR MAXIMALE ADMISSIBLE DE L'AMPLITUDE DE VARIATION
!               D'ORIGINE THERMIQUE (VARIATION DE TEMPERATURE LINEAIRE)
! OUT : STPAR : VALEUR MAXIMALE ADMISSIBLE DE L'AMPLITUDE DE VARIATION
!             D'ORIGINE THERMIQUE (VARIATION DE TEMPERATURE PARABOLIQUE)
!     ------------------------------------------------------------------
!
    real(kind=8) :: linlin, x, x1, y1, x2, y2, yprim, valer(3)
!
    linlin(x,x1,y1,x2,y2)= y1+(x-x1)*(y2-y1)/(x2-x1)
! DEB ------------------------------------------------------------------
!
    x = sigm / symax
!
! --- VARIATION DE TEMPERATURE LINEAIRE DANS LA PAROI
!
    if (abs(x) .le. 1.0d-10) then
        yprim = 0.0d0
    else if (abs(x-1.0d0).le.1.0d-10) then
        yprim = 0.0d0
    else if (x.gt.0.0d0 .and. x.le.0.50d0) then
        yprim = 1.0d0 / x
!
    else if (x.gt.0.50d0 .and. x.le.1.0d0) then
        yprim = 4.0d0 * ( 1.0d0 - x )
!
    else
        stlin = r8vide()
        valer(1) = x
        valer(2) = sigm
        valer(3) = symax
        call u2mesr('I', 'POSTRCCM_5', 3, valer)
        goto 9998
    endif
    stlin = yprim * symax
!
9998  continue
!
! --- VARIATION DE TEMPERATURE PARABOLIQUE DANS LA PAROI
!
    if (abs(x) .le. 1.0d-10) then
        yprim = 0.0d0
    else if (abs(x-1.0d0).le.1.0d-10) then
        yprim = 0.0d0
    else if (x.ge.0.615d0 .and. x.lt.1.0d0) then
        yprim = 5.2d0 * ( 1.0d0 - x )
!
    else if (x.ge.0.5d0 .and. x.lt.0.615d0) then
        yprim = linlin( x, 0.5d0,2.7d0, 0.615d0,2.002d0 )
!
    else if (x.ge.0.4d0 .and. x.lt.0.5d0) then
        yprim = linlin( x, 0.4d0,3.55d0, 0.5d0,2.7d0 )
!
    else if (x.ge.0.3d0 .and. x.lt.0.4d0) then
        yprim = linlin( x, 0.3d0,4.65d0, 0.4d0,3.55d0 )
!
    else
        stpar = r8vide()
        valer(1) = x
        valer(2) = sigm
        valer(3) = symax
        call u2mesr('I', 'POSTRCCM_6', 3, valer)
        goto 9999
    endif
    stpar = yprim * symax
!
9999  continue
!
end subroutine
