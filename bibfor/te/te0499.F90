subroutine te0499(option, nomte)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/pronor.h"
#include "asterfort/rcvalb.h"
#include "asterfort/vff2dn.h"
!
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
    character(len=8) :: fami, poum
    character(len=16) :: nomres(5)
    integer :: icodre(5), kpg, spt
    character(len=1) :: type
    real(kind=8) :: poids, nx, ny, valres(5), e, nu, lambda, mu, cp, cs
    real(kind=8) :: rho, taux, tauy, nux, nuy, scal
    real(kind=8) :: sigma(2, 2), epsi(2, 2), grad(2, 2)
    real(kind=8) :: xgg(4), ygg(4), vondn(2), vondt(2), uondn(2), uondt(2)
    real(kind=8) :: taondx, taondy, norx, nory, dirx, diry, cele
    real(kind=8) :: trace, norm, jac
    real(kind=8) :: param0, param, param2, h, h2, instd, instd2, ris, rip, l0, usl0
    integer :: nno, kp, npg, ipoids, ivf, idfde, igeom
    integer :: ivectu, k, i, mater
    integer :: ier, ii, imate, indic1, indic2, iondc, ionde
    integer :: j, jgano, jinst, ndim, nnos, ndim2
    real(kind=8) :: coedir, typer, valfon, valfon2
    character(len=8) :: nompar(2)
    real(kind=8) :: valpar(2)
!
! --------------------------------------------------------------------------------------------------
!
    ASSERT(option.eq.'ONDE_PLAN')
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PONDPLA', 'L', ionde)
    call jevech('PONDPLR', 'L', iondc)
    call jevech('PTEMPSR', 'L', jinst)
    call jevech('PVECTUR', 'E', ivectu)
!
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
    nomres(4) = 'LONG_CARA'
    nomres(5) = 'COEF_AMOR'
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    ndim2 = 2
!
    nompar(1) = 'X'
    nompar(2) = 'Y'
!   coordonnées du barycentre de l'élément
    valpar(:) = 0.d0
    do i = 1, nnos
        do j = 1, ndim2
            valpar(j) = valpar(j) + zr(igeom-1+(i-1)*ndim2+j)/nnos
        enddo
    enddo
!    
    call rcvalb(fami, kpg, spt, poum, mater,&
                ' ', 'ELAS', 2, nompar, valpar,&
                5, nomres, valres, icodre, 1)
!
    e = valres(1)
    if (e .lt. 1.d-1) goto 99
    nu = valres(2)
    rho = valres(3)
    l0 = valres(4)
    if (l0 .lt. 1.d-2) then
      usl0= 0.d0
    else
      usl0=1.d0/l0
    endif
    lambda = e*nu/ (1.d0+nu)/ (1.d0-2.d0*nu)
    mu = e/2.d0/ (1.d0+nu)
!
    cp = sqrt((lambda+2.d0*mu)/rho)
    cs = sqrt(mu/rho)
    rip = (lambda+2.d0*mu)*usl0
    ris = mu*usl0
!
!     --- CARACTERISTIQUES DE L'ONDE PLANE
!
    dirx =zr(iondc)
    diry =zr(iondc+1)
    typer=zr(iondc+3)
    h = zr(iondc+4)
    h2 = zr(iondc+5)
!ER h2 équivalent de h pour définir la position du toit du rocher par rapport a l'onde

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
    do kp = 1, npg
       xgg(kp)=0.d0
       ygg(kp)=0.d0
    enddo
!
!    write(6,*) 'npg=',npg,'nno=',nno
    do kp = 1, npg
       k = (kp-1)*nno
       do i = 1, nno
          ii = 2*i-1
          xgg(kp)=xgg(kp)+zr(igeom+ii-1)*zr(ivf+k+i-1)
          ygg(kp)=ygg(kp)+zr(igeom+ii)*zr(ivf+k+i-1)
       enddo
!      write(6,*) 'kp=',kp,'xgg=',xgg(kp),'ygg=',ygg(kp)
    enddo
!
    if (type .eq. 'P') then
        cele = cp
    else
        cele = cs
    endif
!    write(6,*) 'cele=',cele
!    write(6,*) 'inst=',zr(jinst)
!    write(6,*) 'h=',h
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do kp = 1, npg
        k = (kp-1)*nno
!
!        CALCUL DU CHARGEMONT PAR ONDE PLANE
        param0=dirx*xgg(kp)+diry*ygg(kp)
!        write(6,*) 'param av=',param
        param = param0 -h
!        write(6,*) 'param 1=',param
!        write(6,*) 'param 2=',param2
        instd = zr(jinst) - param/cele
        if (instd .lt. 0.d0) then
          valfon = 0.d0
        else
          call fointe('F ', zk8(ionde), 1, 'INST', [instd], valfon, ier)
        endif
        if (h2 .ne. r8vide()) then
          param2= 2.0d0*(h2-h)-param
          instd2 = zr(jinst) - param2/cele
          if (instd2 .lt. 0.d0) then
            valfon2 = 0.d0
          else
            call fointe('F ', zk8(ionde), 1, 'INST', [instd2], valfon2, ier)
          endif
        else
           valfon2 = 0.d0
        endif
!
        valfon = -valfon/cele
        valfon2 = -valfon2/cele
!        VALFON = VALFON/CELE
!
!        CALCUL DES CONTRAINTES ASSOCIEES A L'ONDE PLANE
!        CALCUL DU GRADIENT DU DEPLACEMENT
        if (type .eq. 'P') then
!
            grad(1,1) = dirx*(valfon-valfon2)*dirx
            grad(1,2) = diry*(valfon-valfon2)*dirx
            grad(2,1) = dirx*(valfon-valfon2)*diry
            grad(2,2) = diry*(valfon-valfon2)*diry
!
        else if (type.eq.'S') then
!
            grad(1,1) = dirx*(valfon-valfon2)*norx
            grad(1,2) = diry*(valfon-valfon2)*norx
            grad(2,1) = dirx*(valfon-valfon2)*nory
            grad(2,2) = diry*(valfon-valfon2)*nory
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
!ER        scal = nux*dirx + nuy*diry
!ER        if (scal .gt. 0.d0) then
!ER            coedir = 1.d0
!ER        else
        coedir = -1.d0
!ER        endif
!
!        --- CALCUL DE V.N ---
!
        vondt(1) = 0.d0
        vondt(2) = 0.d0
!
        if (type .eq. 'P') then
            vondt(1) = -cele*(valfon+valfon2)*dirx
            vondt(2) = -cele*(valfon+valfon2)*diry
        else if (type.eq.'S') then
            vondt(1) = -cele*(valfon+valfon2)*norx
            vondt(2) = -cele*(valfon+valfon2)*nory
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
        taux = -rho* (cp*vondn(1)+cs*vondt(1))*valres(5)
        tauy = -rho* (cp*vondn(2)+cs*vondt(2))*valres(5)
!
        if (zk8(ionde+1)(1:7) .eq. '&FOZERO') goto 98
        uondt(1) = 0.d0
        uondt(2) = 0.d0
!
        if (instd .lt. 0.d0) then
          valfon = 0.d0
        else
          call fointe('F ', zk8(ionde+1), 1, 'INST', [instd], valfon, ier)
        endif
        if (h2 .ne. r8vide()) then
          if (instd2 .lt. 0.d0) then
            valfon2 = 0.d0
          else
            call fointe('F ', zk8(ionde+1), 1, 'INST', [instd2], valfon2, ier)
          endif
        else
          valfon2 = 0.d0
        endif
        if (type .eq. 'P') then
            uondt(1) = (valfon+valfon2)*dirx
            uondt(2) = (valfon+valfon2)*diry
        else if (type.eq.'S') then
            uondt(1) = (valfon+valfon2)*norx
            uondt(2) = (valfon+valfon2)*nory
        endif
        scal = nux*uondt(1) + nuy*uondt(2)
        uondn(1) = nux*scal
        uondn(2) = nuy*scal
        uondt(1) = uondt(1) - uondn(1)
        uondt(2) = uondt(2) - uondn(2)
!
        taux = taux -(rip*uondn(1)+ris*uondt(1))
        tauy = tauy -(rip*uondn(2)+ris*uondt(2))
98      continue
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
            zr(ivectu+ii+1-1) = zr(ivectu+ii+1-1) + (tauy+coedir*taondy)*zr(ivf+k+i-1)*poids
        enddo
!
    enddo
!
99  continue
!
end subroutine
