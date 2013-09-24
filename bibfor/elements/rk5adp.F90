subroutine rk5adp(nbeq, param, t0, dt0, nbmax,&
                  errmax, y0, dy0, rkfct, resu,&
                  iret)
    implicit none
#include "asterfort/rk5app.h"
    integer :: nbeq
    real(kind=8) :: param(*)
    real(kind=8) :: t0
    real(kind=8) :: dt0
    integer :: nbmax
    real(kind=8) :: errmax
    real(kind=8) :: y0(nbeq)
    real(kind=8) :: dy0(nbeq)
    real(kind=8) :: resu(2*nbeq)
    integer :: iret
    interface
        subroutine rkfct(pp, nbeq, yy0, dy0, dyy,&
                         decoup)
            integer :: nbeq
            real(kind=8) :: pp(*)
            real(kind=8) :: yy0(nbeq)
            real(kind=8) :: dy0(nbeq)
            real(kind=8) :: dyy(nbeq)
            logical :: decoup
        end subroutine rkfct
    end interface
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jean-luc.flejou at edf.fr
! --------------------------------------------------------------------------------------------------
!
!          INTÉGRATION PAR MÉTHODE DE RUNGE KUTTA D'ORDRE 5
!
!  appel à rk5app
!  calcul de l'erreur
!  adaptation du pas de temps
!
! --------------------------------------------------------------------------------------------------
!
!  IN
!     nbeq     : nombre d'équations
!     param    : paramètres du comportement
!     t0       : temps
!     dt0      : incrément de temps
!     nbmax    : nombre d'adaptation successive
!     errmax   : erreur pour seuil d'adaptation
!     y0       : valeurs à t
!     dy0      : vitesse à t
!     rkfct    : subroutine du système à intégrer
!
!  OUT
!     resu     : résultat de l'intégration
!        resu(1:nbeq)            : variables intégrées
!        resu(nbeq+1:2*nbeq)     : dérivées a t+dt
!     iret     : redécoupage du pas de temps global
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbbou, ii
    real(kind=8) :: t9, dt9, y9(nbeq), erreur, xbid1, solu(3*nbeq)
    logical :: decoup
!
    real(kind=8) :: puplus, pumoin, creduc, cforce, coeffm, seuil, precis, grlog
!   puissance pour augmenter le pas de temps
    parameter (puplus = -0.20d0)
!   puissance pour diminuer le pas de temps
    parameter (pumoin = -0.25d0)
!   coefficient de reduction sur la variation du pas de temps
    parameter (creduc =  0.90d0)
!   coefficient de diminution du pas de temps en cas de forcage
    parameter (cforce =  0.30d0)
!   augmentation maximale de pas de temps
    parameter (coeffm =  5.0d0)
    parameter (seuil  = (coeffm/creduc)**(1.0d0/puplus) )
    parameter (precis =  1.0e-08)
    parameter (grlog  =  1.0e+08)
!
    nbbou = 0
    t9 = t0
    dt9 = dt0
    y9(:) = y0(:)
!   ON COMMENCE
100  continue
!
!   dépassement du nombre d'itération maximum ==> découpage global
    if (nbbou .gt. nbmax) then
        iret = 1
        goto 999
    endif
    decoup = .false.
    call rk5app(nbeq, param, dt9, y9, dy0,&
                rkfct, solu, decoup)
    nbbou = nbbou + 1
!   découpage forcé
    if (decoup) then
        dt9 = cforce * dt9
        goto 100
    endif
!   calcul de l'erreur
    erreur = 0.0d0
    do ii = 1, nbeq
        if (abs(y9(ii)) .gt. precis) then
            xbid1 = abs( solu(2*nbeq + ii)/y9(ii) )
        else
            xbid1 = abs( solu(2*nbeq + ii) )
        endif
        if (xbid1 .gt. grlog) then
            if (log10(xbid1) .gt. 100.0d0) then
!               découpage forcé
                dt9 = cforce * dt9
                goto 100
            endif
        endif
        erreur = erreur + (xbid1**2)
    enddo
    erreur = sqrt(erreur)/errmax
    if (erreur .gt. 1.0d0) then
!       on ne converge pas ==> diminution du pas de temps
        xbid1 = creduc * dt9 * (erreur**pumoin)
        dt9 = max(0.10d0 * dt9, xbid1)
        goto 100
    else if (abs(t9 + dt9 - t0 - dt0) .gt. precis) then
!       on a converge ==> augmentation du pas de temps
        nbbou = 0
!       temps convergé
        t9 = t9 + dt9
!       solution convergée
        y9(1:nbeq) = solu(1:nbeq)
!       augmente le pas d'intégration dans la limite de coeffm
        if (erreur .gt. seuil) then
            dt9 = creduc * dt9 * (erreur**puplus)
        else
            dt9 = coeffm * dt9
        endif
!       on ne peut pas dépasser t0 + dt0
        if (t9 + dt9 .gt. t0 + dt0) then
            dt9 = t0 + dt0 - t9
        endif
        goto 100
    endif
!   résultat
    resu = solu(1:2*nbeq)
999  continue
end subroutine
