subroutine te0499(option, nomte)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elref4.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/pronor.h"
#include "asterfort/rcvalb.h"
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
! aslint: disable=W0104
!
    character(len=16), intent(in) :: option
    character(len=16), intent(in) :: nomte
!
! --------------------------------------------------------------------------------------------------
!
! Elementary computation
!
! Elements: 2D
! Option: ONDE_PLAN
!
! --------------------------------------------------------------------------------------------------
!
    character(len=8) :: nomres(3), fami, poum
    integer :: icodre(3), kpg, spt
    character(len=1) :: type
    real(kind=8) :: poids, nx, ny, valres(3), e, nu, lambda, mu, cp, cs
    real(kind=8) :: rho, taux, tauy, nux, nuy, scal
    real(kind=8) :: sigma(2, 2), epsi(2, 2), grad(2, 2)
    real(kind=8) :: vondn(2), vondt(2)
    real(kind=8) :: taondx, taondy, norx, nory, dirx, diry, cele
    real(kind=8) :: trace, norm, jac
    integer :: nno, kp, npg, ipoids, ivf, idfde, igeom
    integer :: ivectu, k, i, mater
    integer :: ier, ii, imate, indic1, indic2, iondc, ionde
    integer :: j, jgano, jinst, ndim, nnos
    real(kind=8) :: coedir, r8b, typer, valfon
!
! --------------------------------------------------------------------------------------------------
!
    ASSERT(option.eq.'ONDE_PLAN')
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PONDPLA', 'L', ionde)
    call jevech('PONDPLR', 'L', iondc)
    call jevech('PTEMPSR', 'L', jinst)
    call jevech('PVECTUR', 'E', ivectu)
!
    if (zk8(ionde)(1:7) .eq. '&FOZERO') goto 99
!
!     --- INITIALISATION DE SIGMA
!
    do i = 1, 2
        do j = 1, 2
            sigma(i,j) =0.d0
        enddo
    enddo
!
    mater = zi(imate)
    nomres(1) = 'E'
    nomres(2) = 'NU'
    nomres(3) = 'RHO'
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, mater,&
                ' ', 'ELAS', 0, ' ', [r8b],&
                3, nomres, valres, icodre, 1)
!
    e = valres(1)
    if (e .lt. 1.d-1) goto 99
    nu = valres(2)
    rho = valres(3)
    lambda = e*nu/ (1.d0+nu)/ (1.d0-2.d0*nu)
    mu = e/2.d0/ (1.d0+nu)
!
    cp = sqrt((lambda+2.d0*mu)/rho)
    cs = sqrt(mu/rho)
!
!     --- CARACTERISTIQUES DE L'ONDE PLANE
!
    dirx =zr(iondc)
    diry =zr(iondc+1)
    typer=zr(iondc+3)
!
    if (typer .eq. 0.d0) type = 'P'
    if (typer .eq. 1.d0) type = 'S'
!
!     --- CALCUL DU VECTEUR DIRECTEUR UNITAIRE DE L'ONDE PLANE
!
    norm = sqrt(dirx**2.d0+diry**2.d0)
    dirx = dirx/norm
    diry = diry/norm
!
!     CALCUL DU REPERE ASSOCIE A L'ONDE
    norx = -diry
    nory = dirx
!
    if (type .eq. 'P') then
        cele = cp
    else
        cele = cs
    endif
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do kp = 1, npg
        k = (kp-1)*nno
!
!        --- CALCUL DU CHARGEMENT PAR ONDE PLANE
!KH          ON SUPPOSE QU'ON RECUPERE UNE VITESSE
        call fointe('F ', zk8(ionde), 1, 'INST', zr(jinst),&
                    valfon, ier)
!
        valfon = -valfon/cele
!        VALFON = VALFON/CELE
!
!        CALCUL DES CONTRAINTES ASSOCIEES A L'ONDE PLANE
!        CALCUL DU GRADIENT DU DEPLACEMENT
        if (type .eq. 'P') then
!
            grad(1,1) = dirx*valfon*dirx
            grad(1,2) = diry*valfon*dirx
            grad(2,1) = dirx*valfon*diry
            grad(2,2) = diry*valfon*diry
!
        else if (type.eq.'S') then
!
            grad(1,1) = dirx*valfon*norx
            grad(1,2) = diry*valfon*norx
            grad(2,1) = dirx*valfon*nory
            grad(2,2) = diry*valfon*nory
!
        endif
!
!        CALCUL DES DEFORMATIONS
        do indic1 = 1, 2
            do indic2 = 1, 2
                epsi(indic1,indic2) = .5d0* ( grad(indic1,indic2)+ grad(indic2,indic1))
            enddo
        enddo
!
!        CALCUL DES CONTRAINTES
        trace = 0.d0
        do indic1 = 1, 2
            trace = trace + epsi(indic1,indic1)
        enddo
        do indic1 = 1, 2
            do indic2 = 1, 2
                if (indic1 .eq. indic2) then
                    sigma(indic1,indic2) = lambda*trace + 2.d0*mu* epsi( indic1,indic2)
                else
                    sigma(indic1,indic2) = 2.d0*mu*epsi(indic1,indic2)
                endif
            enddo
        enddo
!
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom), nx, ny, poids)
!
        jac = sqrt(nx*nx+ny*ny)
!
!        --- CALCUL DE LA NORMALE UNITAIRE ---
!
        nux = nx/jac
        nuy = ny/jac
!
!        --- TEST DU SENS DE LA NORMALE PAR RAPPORT A LA DIRECTION
!            DE L'ONDE
!
        scal = nux*dirx + nuy*diry
        if (scal .gt. 0.d0) then
            coedir = 1.d0
        else
            coedir = -1.d0
        endif
!
!        --- CALCUL DE V.N ---
!
        vondt(1) = 0.d0
        vondt(2) = 0.d0
!
        if (type .eq. 'P') then
            vondt(1) = -cele*valfon*dirx
            vondt(2) = -cele*valfon*diry
        else if (type.eq.'S') then
            vondt(1) = -cele*valfon*norx
            vondt(2) = -cele*valfon*nory
        endif
!
        scal = nux*vondt(1) + nuy*vondt(2)
!
!        --- CALCUL DE LA VITESSE NORMALE ET DE LA VITESSE TANGENCIELLE
!
        vondn(1) = nux*scal
        vondn(2) = nuy*scal
!
        vondt(1) = vondt(1) - vondn(1)
        vondt(2) = vondt(2) - vondn(2)
!
!        --- CALCUL DU VECTEUR CONTRAINTE
!
        taux = -rho* (cp*vondn(1)+cs*vondt(1))
        tauy = -rho* (cp*vondn(2)+cs*vondt(2))
!
!        --- CALCUL DU VECTEUR CONTRAINTE DU A UNE ONDE PLANE
!
        taondx = sigma(1,1)*nux
        taondx = taondx + sigma(1,2)*nuy
!
        taondy = sigma(2,1)*nux
        taondy = taondy + sigma(2,2)*nuy
!
!        --- CALCUL DU VECTEUR ELEMENTAIRE
!
        do i = 1, nno
            ii = 2*i-1
            zr(ivectu+ii-1) = zr(ivectu+ii-1) + (taux+coedir*taondx)* zr(ivf+k+i-1)*poids
            zr(ivectu+ii+1-1) = zr(ivectu+ii+1-1) + (tauy+coedir* taondy)*zr(ivf+k+i-1)*poids
        enddo
!
    enddo
!
99  continue
!
end subroutine
