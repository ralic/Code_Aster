subroutine piesgv(neps, tau, mat, vim, epsm,&
                  epsp, epsd, typmod, etamin, etamax,&
                  copilo)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "asterfort/lcerma.h"
#include "asterfort/lcervf.h"
#include "asterfort/lcesvf.h"
#include "asterfort/r8inir.h"
#include "asterfort/utmess.h"
#include "blas/ddot.h"
    character(len=8) :: typmod
    integer :: neps, mat
    real(kind=8) :: tau, epsm(neps), epsd(neps), epsp(neps), etamin, etamax
    real(kind=8) :: vim(3), copilo(2, 2)
!
! ----------------------------------------------------------------------
!     PILOTAGE PRED_ELAS POUR ENDO_SCALAIRE (EN GRAD_VARI)
! ----------------------------------------------------------------------
!
! IN  NEPS    DIMENSION DES DEFORMATIONS (3*NDIM+2)
! IN  MAT     NATURE DU MATERIAU
! IN  VIM     VARIABLES INTERNES EN T-
! IN  EPSM    CHAMP DE DEFORMATION EN T-
! IN  EPSP    INCREMENT FIXE
! IN  EPSD    INCREMENT PILOTE
! IN  ETAMIN  BORNE INF DU PILOTAGE (INUTILISE)
! IN  ETAMAX  BORNE SUP DU PILOTAGE (INUTILISE)
! OUT COPILO  COEFFICIENTS DE PILOTAGE :
!               F := COPILO(1,1)+COPILO(2,1)*ETA = TAU
!               F := COPILO(1,2)+COPILO(2,2)*ETA = TAU
! ----------------------------------------------------------------------
    logical :: cplan, croiss, gauche, droite
    integer :: ndim, ndimsi, i, n
    real(kind=8) :: coplan
    real(kind=8) :: am, a, drda, d1a0, d2a0
    real(kind=8) :: etam, etap, etal, precvg
    real(kind=8) :: ep0(6), trep0, ep0dv(6), gamep0, dgade0(6), phi0
    real(kind=8) :: ep1(6), trep1, ep1dv(6), gamep1, dgade1(6), phi1
    real(kind=8) :: epm(6), trepm, epmdv(6), gamepm, dgadem(6), phim, gm, dgm
    real(kind=8) :: epp(6), trepp, eppdv(6), gamepp, dgadep(6), phip, gp, dgp
    real(kind=8) :: epl(6), trepl, epldv(6), gamepl, dgadel(6), phil, gl, dgl
! ----------------------------------------------------------------------
!  ITEMAX: NOMBRE MAX D'ITERATIONS POUR LA METHODE DE NEWTON
!  PREC  : VARIATION PREC*DA POUR REPRESENTATIF DE LA CONVERGENCE
!  PREMIN: BORNE INF POUR DA DANS LE CRITERE PRECEDENT
    integer :: itemax
    real(kind=8) :: prec, premin
    parameter (prec=1.d-2,premin=1.d-10,itemax=100)
! ----------------------------------------------------------------------
    real(kind=8) :: lambda, deuxmu, troisk, rigmin, pc, pr, epsth
    common /lcee/ lambda,deuxmu,troisk,rigmin,pc,pr,epsth
! ----------------------------------------------------------------------
    real(kind=8) :: pk, pm, pp
    common /lces/ pk,pm,pp
! ----------------------------------------------------------------------
    real(kind=8) :: pct, pch, pcs
    common /lcer/ pch,pct,pcs
! ----------------------------------------------------------------------
!
! -- INITIALISATION
!
    cplan = typmod.eq.'C_PLAN  '
    ndim = (neps-2)/3
    ndimsi = 2*ndim
    am = vim(1)
!
!
! -- LECTURE DES CARACTERISTIQUES MATERIAU
!
    call lcerma(mat, 'NONE', 1, 1, '+')
!
!
! -- ENDOMMAGEMENT CIBLE
!
    a = am+tau
!
!    NON PILOTABLE CAR TROP PRES DE L'ENDOMMAGEMENT ULTIME
    if (a .ge. 1) then
        call r8inir(4, 0.d0, copilo, 1)
        goto 9999
    endif
    drda = lcesvf(1,a)
!
!
! -- EXTRACTION DES DEFORMATIONS ET DES PARAMETRES NON LOCAUX
!
    do 10 i = 1, ndimsi
        ep0(i) = epsm(i)+epsp(i)
        ep1(i) = epsd(i)
10  end do
!
    if (cplan) then
        coplan = -lambda / (lambda + deuxmu)
        ep0(3) = coplan*(ep0(1)+ep0(2))
        ep1(3) = coplan*(ep1(1)+ep1(2))
    endif
!
    call lcervf(0, ndimsi, ep0, trep0, ep0dv,&
                gamep0, dgade0)
    call lcervf(0, ndimsi, ep1, trep1, ep1dv,&
                gamep1, dgade1)
!
    phi0 = epsm(ndimsi+2) + pr*epsm(ndimsi+1) + epsp(ndimsi+2) + pr*epsp(ndimsi+1)
    phi1 = epsd(ndimsi+2) + pr*epsd(ndimsi+1)
!
!
! -- ESTIMATION DU CRITERE DE CONVERGENCE
!
    d1a0 = lcesvf(1,0.d0)
    d2a0 = lcesvf(2,0.d0)
    precvg = abs(d2a0/d1a0)*pk*max((a-am),premin)*prec
! -- RESOLUTION EN FONCTION DES BORNES
!
    do 20 i = 1, ndimsi
        epm(i) = ep0(i) + etamin*ep1(i)
        epp(i) = ep0(i) + etamax*ep1(i)
20  end do
    call lcervf(1, ndimsi, epm, trepm, epmdv,&
                gamepm, dgadem)
    call lcervf(1, ndimsi, epp, trepp, eppdv,&
                gamepp, dgadep)
    phim = phi0+etamin*phi1
    phip = phi0+etamax*phi1
    gm = -drda*gamepm-pk-pr*a+phim
    gp = -drda*gamepp-pk-pr*a+phip
    dgm = -drda*ddot(ndimsi,dgadem,1,ep1,1)+phi1
    dgp = -drda*ddot(ndimsi,dgadep,1,ep1,1)+phi1
!
!
! 1. NE CONTRIBUE PAS AU PILOTAGE (TOUJOURS SOUS LE SEUIL)
!
    if (gm .le. 0 .and. gp .le. 0) then
        call r8inir(4, 0.d0, copilo, 1)
        goto 9999
    endif
!
!
! 2. BORNES SUPERIEURES AU SEUIL : DOUBLE NEWTON
!
    if (gm .ge. 0 .and. gp .ge. 0) then
!
!      INITIALISATION A GAUCHE ET A DROITE
        etam = etamin
        etap = etamax
!
        do 100 n = 1, itemax
!
!        TEST DE CONVERGENCE
            gauche = abs(gm).le.precvg
            droite = abs(gp).le.precvg
            if (gauche .and. droite) goto 150
!
!        ABSENCE DE SOLUTION SI FONCTION AU-DESSUS DE ZERO
            if (dgm .ge. 0 .or. dgp .le. 0) then
                goto 9999
            endif
!
!        METHODE DE NEWTON A GAUCHE ET A DROITE
            if (.not.gauche) etam = etam - gm/dgm
            if (.not.droite) etap = etap - gp/dgp
!
!        CALCUL DE LA FONCTION ET DERIVEE
            if (.not.gauche) then
                do 110 i = 1, ndimsi
                    epm(i) = ep0(i) + etam*ep1(i)
110              continue
                call lcervf(1, ndimsi, epm, trepm, epmdv,&
                            gamepm, dgadem)
                phim = phi0+etam*phi1
                gm = -drda*gamepm-pk-pr*a+phim
                dgm = -drda*ddot(ndimsi,dgadem,1,ep1,1)+phi1
            endif
!
            if (.not.droite) then
                do 120 i = 1, ndimsi
                    epp(i) = ep0(i) + etap*ep1(i)
120              continue
                call lcervf(1, ndimsi, epp, trepp, eppdv,&
                            gamepp, dgadep)
                phip = phi0+etap*phi1
                gp = -drda*gamepp-pk-pr*a+phip
                dgp = -drda*ddot(ndimsi,dgadep,1,ep1,1)+phi1
            endif
!
100      continue
!
!      ECHEC DE LA RESOLUTION AVEC LE NOMBRE D'ITERATIONS REQUIS
        call utmess('F', 'PILOTAGE_83')
!
!      POST-TRAITEMENT DES SOLUTIONS
150      continue
        copilo(1,1) = tau+etam
        copilo(2,1) = -1
        copilo(1,2) = tau-etap
        copilo(2,2) = 1
        goto 9999
    endif
!
!
! 3. BORNES DE PART ET D'AUTRE DU SEUIL --> NEWTON DEPUIS POSITIVE
!
    if (gm .ge. 0) then
        croiss = .false.
        etal = etamin
        gl = gm
        dgl = dgm
    else
        croiss = .true.
        etal = etamax
        gl = gp
        dgl = dgp
    endif
!
    do 200 n = 1, itemax
!
!      TEST DE CONVERGENCE
        if (abs(gl) .le. precvg) goto 250
!
!      METHODE DE NEWTON A GAUCHE ET A DROITE
        etal = etal - gl/dgl
!
!      CALCUL DE LA FONCTION ET DERIVEE
        do 210 i = 1, ndimsi
            epl(i) = ep0(i) + etal*ep1(i)
210      continue
        call lcervf(1, ndimsi, epl, trepl, epldv,&
                    gamepl, dgadel)
        phil = phi0+etal*phi1
        gl = -drda*gamepl-pk-pr*a+phil
        dgl = -drda*ddot(ndimsi,dgadel,1,ep1,1)+phi1
200  end do
!
!    ECHEC DE LA RESOLUTION AVEC LE NOMBRE D'ITERATIONS REQUIS
    call utmess('F', 'PILOTAGE_83')
!
!    POST-TRAITEMENT DES SOLUTIONS
250  continue
    copilo(1,2)=0
    copilo(2,2)=0
    if (croiss) then
        copilo(1,1) = tau-etal
        copilo(2,1) = 1
    else
        copilo(1,1) = tau+etal
        copilo(2,1) = -1
    endif
!
9999  continue
end subroutine
