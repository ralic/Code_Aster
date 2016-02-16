subroutine piesgv(neps, tau, mat, lccrma, vim,&
                  epsm, epsp, epsd, typmod, lcesga,&
                  etamin, etamax, lcesbo, copilo)
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
#include "asterfort/lcesma.h"
#include "asterfort/lcesvf.h"
#include "asterfort/piesfg.h"
#include "asterfort/utmess.h"
    interface
        subroutine lccrma(mat, fami, kpg, ksp, poum)
            integer, intent(in) :: mat, kpg, ksp
            character(len=1), intent(in) :: poum
            character(len=*), intent(in) :: fami
        end subroutine lccrma
!
        subroutine lcesga(mode, eps, gameps, dgamde, itemax,&
                          precvg, iret)
            integer, intent(in) :: mode, itemax
            real(kind=8), intent(in) :: eps(6), precvg
            integer, intent(out) :: iret
            real(kind=8), intent(out) :: gameps, dgamde(6)
        end subroutine lcesga
!
        subroutine lcesbo(ep0, ep1, l0, l1, etamin,&
                          etamax, vide, etam, etap)
            real(kind=8), intent(in) :: ep0(6), ep1(6), l0, l1, etamin, etamax
            aster_logical, intent(out) :: vide
            real(kind=8), intent(out) :: etam, etap
        end subroutine lcesbo
    end interface
!
    character(len=8), intent(in) :: typmod(*)
    integer, intent(in) :: neps, mat
    real(kind=8), intent(in) :: tau, epsm(neps), epsd(neps), epsp(neps), etamin, etamax, vim(3)
    real(kind=8), intent(out) :: copilo(2, *)
! --------------------------------------------------------------------------------------------------
!     PILOTAGE PRED_ELAS POUR ENDO_SCALAIRE (EN GRAD_VARI)
! --------------------------------------------------------------------------------------------------
! IN  NEPS    DIMENSION DES DEFORMATIONS (3*NDIM+2)
! IN  MAT     NATURE DU MATERIAU
! IN  LCCRMA  ROUTINE POUR LECTURE DES PARAMETRES DU CRITERE
! IN  VIM     VARIABLES INTERNES EN T-
! IN  EPSM    CHAMP DE DEFORMATION EN T-
! IN  EPSP    INCREMENT FIXE
! IN  EPSD    INCREMENT PILOTE
! IN  LCESGA  ROUTINE POUR CALCUL DU CRITERE EN DEFORMATION
! IN  ETAMIN  BORNE INF DU PILOTAGE
! IN  ETAMAX  BORNE SUP DU PILOTAGE
! OUT COPILO  COEFFICIENTS DE PILOTAGE :
!               F := COPILO(1,1)+COPILO(2,1)*ETA = TAU
!               F := COPILO(1,2)+COPILO(2,2)*ETA = TAU
! ----------------------------------------------------------------------
!  ITEMAX: NOMBRE MAX D'ITERATIONS POUR LA METHODE DE NEWTON
!  ERRA  : ERREUR TOLEREE SUR A DANS LA LDC (CRIT CVG)
!  RED   : REDUCTION DE L'ERREUR POUR EN FAIRE UN CRITERE DE PRECISION
    integer, parameter :: itemax=100
    real(kind=8), parameter :: red=1.d-2, erra=1.d-6
! ----------------------------------------------------------------------
    aster_logical :: cplan, croiss, gauche, droite, vide
    integer :: ndim, ndimsi, i, n
    real(kind=8) :: coplan
    real(kind=8) :: etam, etap, etal, precvg, l0, l1, etm, etp
    real(kind=8) :: gm, dgm, gp, dgp, gl, dgl
! ----------------------------------------------------------------------
    real(kind=8) :: lambda, deuxmu, troisk, gamma, rigmin, pc, pr, epsth
    common /lcee/ lambda,deuxmu,troisk,gamma,rigmin,pc,pr,epsth
! ----------------------------------------------------------------------
    real(kind=8) :: pk, pm, pp, pq
    common /lces/ pk,pm,pp,pq
! ----------------------------------------------------------------------
    real(kind=8) :: ep0(6), ep1(6), phi0, phi1, a, drda, precga
    common /pies/ ep0,ep1,phi0,phi1,a,drda,precga
! ----------------------------------------------------------------------
!
!
!
! -- INITIALISATION
!
    cplan = typmod(1).eq.'C_PLAN  '
    ndim = (neps-2)/3
    ndimsi = 2*ndim
    if (ndim .eq. 2) then
        ep0(5:6) = 0
        ep1(5:6) = 0
    endif
!
! -- LECTURE DES CARACTERISTIQUES MATERIAU
!
    call lcesma(mat, 'NONE', 1, 1, '+',&
                lccrma)
!
!
! -- ENDOMMAGEMENT CIBLE
!
    a = vim(1)+tau
!
!
!  NON PILOTABLE CAR TROP PRES DE L'ENDOMMAGEMENT ULTIME
    if (a .ge. 0.99) goto 999
    drda = lcesvf(1,a)
!
!
! -- EXTRACTION DES DEFORMATIONS ET DES PARAMETRES NON LOCAUX
!
    do i = 1, ndimsi
        ep0(i) = epsm(i)+epsp(i)
        ep1(i) = epsd(i)
    end do
!
    if (cplan) then
        coplan = -lambda / (lambda + deuxmu)
        ep0(3) = coplan*(ep0(1)+ep0(2))
        ep1(3) = coplan*(ep1(1)+ep1(2))
    endif
!
    phi0 = epsm(ndimsi+2) + pr*epsm(ndimsi+1) + epsp(ndimsi+2) + pr*epsp(ndimsi+1)
    phi1 = epsd(ndimsi+2) + pr*epsd(ndimsi+1)
!
!
!
! -- AFFINER LES BORNES
!
    l0 = pm/pk/drda * (pk+pr*a-phi0)
    l1 = - pm/pk/drda * phi1
    call lcesbo(ep0, ep1, l0, l1, etamin,&
                etamax, vide, etm, etp)
!
!  PAS DE SOLUTION POUR LE PILOTAGE
    if (vide) then
        copilo(1,3) = 0
        goto 999
    endif
!
!
!
! -- ESTIMATION INITIALE DU CRITERE DE CONVERGENCE
!
    precvg = red*pr*erra
    precga = min(red*precvg/abs(drda), pk/pm*1.d-3)
!
!
! -- RESOLUTION EN FONCTION DES BORNES
!
    call piesfg(lcesga, etm, gm, dgm)
    call piesfg(lcesga, etp, gp, dgp)
!
!
! 1. NE CONTRIBUE PAS AU PILOTAGE (TOUJOURS SOUS LE SEUIL)
!
    if (gm .le. 0 .and. gp .le. 0) then
        goto 999
    end if
!
!
! 2. BORNES SUPERIEURES AU SEUIL : DOUBLE NEWTON
!
        if (gm .ge. 0 .and. gp .ge. 0) then
!
!          INITIALISATION A GAUCHE ET A DROITE
            etam = etm
            etap = etp
            do n = 1, itemax
!
!              TEST DE CONVERGENCE
                gauche = abs(gm).le.precvg
                droite = abs(gp).le.precvg
                if (gauche .and. droite) goto 150
!
!              ABSENCE DE SOLUTION SI FONCTION AU-DESSUS DE ZERO
                if (dgm .ge. 0 .or. dgp .le. 0) then
                    copilo(1,3) = 0
                    goto 999
                endif
!
!              METHODE DE NEWTON A GAUCHE ET A DROITE
                if (.not.gauche) etam = etam - gm/dgm
                if (.not.droite) etap = etap - gp/dgp

!              ABSENCE DE SOLUTION SI FONCTION AU-DESSUS DE ZERO
                if (etap.lt.etam) then
                    copilo(1,3) = 0
                    goto 999
                end if
!
!              CALCUL DE LA FONCTION ET DERIVEE
                if (.not.gauche) call piesfg(lcesga, etam, gm, dgm)
                if (.not.droite) call piesfg(lcesga, etap, gp, dgp)
!
            end do
!
!          ECHEC DE LA RESOLUTION AVEC LE NOMBRE D'ITERATIONS REQUIS
            call utmess('F', 'PILOTAGE_83')
!
!
!          POST-TRAITEMENT DES SOLUTIONS
150         continue
            copilo(1,1) = tau+etam
            copilo(2,1) = -1
            copilo(1,2) = tau-etap
            copilo(2,2) = 1
            goto 999
        endif
!
!
!     3. BORNES DE PART ET D'AUTRE DU SEUIL --> NEWTON DEPUIS POSITIVE
        if (gm .ge. 0) then
            croiss = .false.
            etal = etm
            gl = gm
            dgl = dgm
        else
            croiss = .true.
            etal = etp
            gl = gp
            dgl = dgp
        endif
!
        do n = 1, itemax
!
!          TEST DE CONVERGENCE
            if (abs(gl) .le. precvg) goto 250
!
!          METHODE DE NEWTON A GAUCHE ET A DROITE
            etal = etal - gl/dgl
            call piesfg(lcesga, etal, gl, dgl)
        end do
!
        call utmess('F', 'PILOTAGE_83')
!
!      POST-TRAITEMENT DES SOLUTIONS
250     continue
        if (croiss) then
            copilo(1,1) = tau-etal
            copilo(2,1) = 1
        else
            copilo(1,1) = tau+etal
            copilo(2,1) = -1
        endif
!
999 continue
    end subroutine
