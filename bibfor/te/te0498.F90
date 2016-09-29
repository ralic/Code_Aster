subroutine te0498(option, nomte)
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
! Elements: 3D
! Option: ONDE_PLAN
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ipoids, ivf, idfdx, idfdy, igeom, i, j
    integer :: ndim, nno, ipg, npg1, ino, jno
    integer :: idec, jdec, kdec, ldec, ires, imate
    integer :: ii, mater, jinst, indic1, indic2
    integer :: ionde, iondc, ier, nnos, jgano
    real(kind=8) :: jac, nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9)
    real(kind=8) :: valres(5), e, nu, lambda, mu, cp, cs, rho, typer
    real(kind=8) :: taux, tauy, tauz, dirx, diry, dirz
    real(kind=8) :: norm, tanx, tany, norx, nory, norz
    real(kind=8) :: taondx, taondy, taondz
    real(kind=8) :: nux, nuy, nuz, scal, coedir
    real(kind=8) ::  nortan, cele, trace
    real(kind=8) :: param0, param, param2, h, h2, instd, instd2, ris, rip, l0, usl0
    real(kind=8) :: sigma(3, 3), epsi(3, 3), grad(3, 3), valfon, valfon2
    real(kind=8) :: xgg(9), ygg(9), zgg(9), vondn(3), vondt(3), uondn(3), uondt(3)
    integer :: icodre(5), kpg, spt, ndim2
    character(len=2) :: type
    character(len=8) :: fami, poum
    character(len=16) :: nomres(5)
    character(len=8) :: nompar(3)
    real(kind=8) :: valpar(3)
! --------------------------------------------------------------------------------------------------
!
    ASSERT(option.eq.'ONDE_PLAN')
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfdx,jgano=jgano)
    idfdy = idfdx + 1
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PONDPLA', 'L', ionde)
    call jevech('PONDPLR', 'L', iondc)
    call jevech('PTEMPSR', 'L', jinst)
    call jevech('PVECTUR', 'E', ires)
!
    if (zk8(ionde)(1:7) .eq. '&FOZERO') goto 99
!
!     --- INITIALISATION DE SIGMA
!
    do i = 1, 3
        do j = 1, 3
            sigma(i,j) = 0.d0
        enddo
    enddo
!
    mater=zi(imate)
    nomres(1)='E'
    nomres(2)='NU'
    nomres(3) = 'RHO'
    nomres(4) = 'LONG_CARA'
    nomres(5) = 'COEF_AMOR'
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    ndim2 = 3
!
    nompar(1) = 'X'
    nompar(2) = 'Y'
    nompar(3) = 'Z'
!   coordonnées du barycentre de l'élément
    valpar(:) = 0.d0
    do i = 1, nnos
        do j = 1, ndim2
            valpar(j) = valpar(j) + zr(igeom-1+(i-1)*ndim2+j)/nnos
        enddo
    enddo
!
    call rcvalb(fami, kpg, spt, poum, mater,&
                ' ', 'ELAS', 3, nompar, valpar,&
                5, nomres, valres, icodre, 1)
    e = valres(1)
    nu = valres(2)
    rho = valres(3)
    l0 = valres(4)
    if (l0 .lt. 1.d-2) then
      usl0= 0.d0
    else
      usl0=1.d0/l0
    endif
!
    lambda = e*nu/(1.d0+nu)/(1.d0-2.d0*nu)
    mu = e/2.d0/(1.d0+nu)
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
    dirz =zr(iondc+2)
    typer=zr(iondc+3)
    h = zr(iondc+4)
    h2 = zr(iondc+5)
!    h2 = 5.135d0
!
    if (typer .eq. 0.d0) type = 'P'
    if (typer .eq. 1.d0) type = 'SV'
    if (typer .eq. 2.d0) type = 'SH'
!
!     --- CALCUL DU VECTEUR DIRECTEUR UNITAIRE DE L'ONDE PLANE
!
    norm = sqrt(dirx**2.d0+diry**2.d0+dirz**2.d0)
    dirx = dirx/norm
    diry = diry/norm
    dirz = dirz/norm
!
!     CALCUL DU REPERE ASSOCIE A L'ONDE
    tanx = diry
    tany = -dirx
!
    nortan = sqrt(tanx**2.d0+tany**2.d0)
!
    if (nortan .ne. 0.d0) then
        tanx = tanx/nortan
        tany = tany/nortan
!
        norx = tany*dirz
        nory = -tanx*dirz
        norz = tanx*diry - tany*dirx
    else
        norx = dirz
        nory = 0.d0
        norz = 0.d0
!
        tanx = 0.d0
        tany = dirz
    endif
    if (type .eq. 'P') then
        cele = cp
    else
        cele = cs
    endif
!
!     --- CALCUL DES PRODUITS VECTORIELS OMI X OMJ ---
!
    do ino = 1, nno
        i = igeom + 3*(ino-1) -1
        do jno = 1, nno
            j = igeom + 3*(jno-1) -1
            sx(ino,jno) = zr(i+2)*zr(j+3) - zr(i+3)*zr(j+2)
            sy(ino,jno) = zr(i+3)*zr(j+1) - zr(i+1)*zr(j+3)
            sz(ino,jno) = zr(i+1)*zr(j+2) - zr(i+2)*zr(j+1)
        enddo
    enddo
    do ipg = 1, npg1
       xgg(ipg)=0.d0
       ygg(ipg)=0.d0
       zgg(ipg)=0.d0
    enddo
!
!    write(6,*) 'npg=',npg1,'nno=',nno
    do ipg = 1, npg1
       ldec = (ipg-1)*nno
       do i = 1, nno
          ii = 3*i-2
          xgg(ipg)=xgg(ipg)+zr(igeom+ii-1)*zr(ivf+ldec+i-1)
          ygg(ipg)=ygg(ipg)+zr(igeom+ii)*zr(ivf+ldec+i-1)
          zgg(ipg)=zgg(ipg)+zr(igeom+ii+1)*zr(ivf+ldec+i-1)
       enddo
!       write(6,*) 'kp=',ipg,'xgg=',xgg(ipg),'ygg=',ygg(ipg),'zgg=',zgg(ipg)
    enddo
!    write(6,*) 'cele=',cele
!    write(6,*) 'inst=',zr(jinst)
!    write(6,*) 'h=',h
!
!     --- BOUCLE SUR LES POINTS DE GAUSS ---
!
    do ipg = 1, npg1
!
        kdec = (ipg-1)*nno*ndim
        ldec = (ipg-1)*nno
!
!        --- CALCUL DU CHARGEMENT PAR ONDE PLANE
!KH          ON SUPPOSE QU'ON RECUPERE UNE VITESSE
        param0=dirx*xgg(ipg)+diry*ygg(ipg)+dirz*zgg(ipg)
!    write(6,*) 'param av=',param
!    dist=cele*tf
        param = param0 -h
!    write(6,*) 'param ap=',param
        instd = zr(jinst) - param/cele
        if (instd .lt. 0.d0) then
          valfon = 0.d0
        else
          call fointe('F ', zk8(ionde), 1, 'INST', [instd], valfon, ier)
        endif
        if (h2 .ne. r8vide()) then
          param2 = 2.d0*(h2-h)-param
          instd2 = zr(jinst) - param2/cele
          if (instd2 .lt. 0.d0) then
            valfon2 = 0.d0
          else
            call fointe('F ', zk8(ionde), 1, 'INST', [instd2], valfon2, ier)
          endif
        else
           valfon2 = 0.d0
        endif

        valfon = -valfon/cele
        valfon2 = -valfon2/cele
!         VALFON = +VALFON/CELE
!
!        CALCUL DES CONTRAINTES ASSOCIEES A L'ONDE PLANE
!        CALCUL DU GRADIENT DU DEPLACEMENT
        if (type .eq. 'P') then
!
            grad(1,1) = dirx*(valfon-valfon2)*dirx
            grad(1,2) = diry*(valfon-valfon2)*dirx
            grad(1,3) = dirz*(valfon-valfon2)*dirx
!
            grad(2,1) = dirx*(valfon-valfon2)*diry
            grad(2,2) = diry*(valfon-valfon2)*diry
            grad(2,3) = dirz*(valfon-valfon2)*diry
!
            grad(3,1) = dirx*(valfon-valfon2)*dirz
            grad(3,2) = diry*(valfon-valfon2)*dirz
            grad(3,3) = dirz*(valfon-valfon2)*dirz
!
        else if (type.eq.'SV') then
!
            grad(1,1) = dirx*(valfon-valfon2)*norx
            grad(1,2) = diry*(valfon-valfon2)*norx
            grad(1,3) = dirz*(valfon-valfon2)*norx
!
            grad(2,1) = dirx*(valfon-valfon2)*nory
            grad(2,2) = diry*(valfon-valfon2)*nory
            grad(2,3) = dirz*(valfon-valfon2)*nory
!
            grad(3,1) = dirx*(valfon-valfon2)*norz
            grad(3,2) = diry*(valfon-valfon2)*norz
            grad(3,3) = dirz*(valfon-valfon2)*norz
!
        else if (type.eq.'SH') then
!
            grad(1,1) = dirx*(valfon-valfon2)*tanx
            grad(1,2) = diry*(valfon-valfon2)*tanx
            grad(1,3) = dirz*(valfon-valfon2)*tanx
!
            grad(2,1) = dirx*(valfon-valfon2)*tany
            grad(2,2) = diry*(valfon-valfon2)*tany
            grad(2,3) = dirz*(valfon-valfon2)*tany
!
            grad(3,1) = 0.d0
            grad(3,2) = 0.d0
            grad(3,3) = 0.d0
!
        endif
!
!        CALCUL DES DEFORMATIONS
        do indic1 = 1, 3
            do indic2 = 1, 3
                epsi(indic1,indic2) = .5d0*( grad(indic1,indic2) + grad(indic2,indic1) )
            enddo
        enddo
!
!        CALCUL DES CONTRAINTES
        trace = 0.d0
        do indic1 = 1, 3
            trace = trace + epsi(indic1,indic1)
        enddo
!
        do indic1 = 1, 3
            do indic2 = 1, 3
                if (indic1 .eq. indic2) then
                    sigma(indic1,indic2) = lambda*trace +2.d0*mu*epsi( indic1,indic2)
                else
                    sigma(indic1,indic2) = 2.d0*mu*epsi(indic1,indic2)
                endif
            enddo
        enddo
!
        nx = 0.0d0
        ny = 0.0d0
        nz = 0.0d0
!
!        --- CALCUL DE LA NORMALE AU POINT DE GAUSS IPG ---
!
        do i = 1, nno
            idec = (i-1)*ndim
            do j = 1, nno
                jdec = (j-1)*ndim
                nx = nx + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sx(i,j)
                ny = ny + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sy(i,j)
                nz = nz + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sz(i,j)
            enddo
        enddo
!
!        --- LE JACOBIEN EST EGAL A LA NORME DE LA NORMALE ---
!
        jac = sqrt(nx*nx + ny*ny + nz*nz)
!
!        --- CALCUL DE LA NORMALE UNITAIRE ---
!
        nux = nx /jac
        nuy = ny /jac
        nuz = nz /jac
!
!        --- TEST DU SENS DE LA NORMALE PAR RAPPORT A LA DIRECTION
!            DE L'ONDE
!
        scal = nux*dirx + nuy*diry + nuz*dirz
        if (scal .gt. 0.d0) then
            coedir = 1.d0
        else
            coedir = -1.d0
        endif
        coedir = -1.d0
!
!        --- CALCUL DE V.N ---
!
        vondt(1) = 0.d0
        vondt(2) = 0.d0
        vondt(3) = 0.d0
!
        if (type .eq. 'P') then
            vondt(1) = -cele*(valfon+valfon2)*dirx
            vondt(2) = -cele*(valfon+valfon2)*diry
            vondt(3) = -cele*(valfon+valfon2)*dirz
        else if (type.eq.'SV') then
            vondt(1) = -cele*(valfon+valfon2)*norx
            vondt(2) = -cele*(valfon+valfon2)*nory
            vondt(3) = -cele*(valfon+valfon2)*norz
        else if (type.eq.'SH') then
            vondt(1) = -cele*(valfon+valfon2)*tanx
            vondt(2) = -cele*(valfon+valfon2)*tany
            vondt(3) = 0.d0
        endif
!
!        --- CALCUL DE LA VITESSE NORMALE ET DE LA VITESSE TANGENTIELLE
        call pronor(nux, nuy, nuz, vondt, vondn)
!
!        --- CALCUL DU VECTEUR CONTRAINTE
!
        taux = - rho*(cp*vondn(1) + cs*vondt(1))*valres(5)
        tauy = - rho*(cp*vondn(2) + cs*vondt(2))*valres(5)
        tauz = - rho*(cp*vondn(3) + cs*vondt(3))*valres(5)
!
        if (zk8(ionde+1)(1:7) .eq. '&FOZERO') goto 98

        uondt(1) = 0.d0
        uondt(2) = 0.d0
        uondt(3) = 0.d0
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
            uondt(3) = (valfon+valfon2)*dirz
        else if (type.eq.'SV') then
            uondt(1) = (valfon+valfon2)*norx
            uondt(2) = (valfon+valfon2)*nory
            uondt(3) = (valfon+valfon2)*norz
        else if (type.eq.'SH') then
            uondt(1) = (valfon+valfon2)*tanx
            uondt(2) = (valfon+valfon2)*tany
            uondt(3) = 0.d0
        endif
!        --- CALCUL DES DEPLACEMENTS NORMAL ET TANGENTIEL
        call pronor(nux, nuy, nuz, uondt, uondn)
!        --- CALCUL DU VECTEUR CONTRAINTE
        taux = taux -(rip*uondn(1)+ris*uondt(1))
        tauy = tauy -(rip*uondn(2)+ris*uondt(2))
        tauz = tauz -(rip*uondn(3)+ris*uondt(3))
98      continue
!
!        --- CALCUL DU VECTEUR CONTRAINTE DU A UNE ONDE PLANE
!
        taondx = sigma(1,1)*nux
        taondx = taondx + sigma(1,2)*nuy
        taondx = taondx + sigma(1,3)*nuz
!
        taondy = sigma(2,1)*nux
        taondy = taondy + sigma(2,2)*nuy
        taondy = taondy + sigma(2,3)*nuz
!
        taondz = sigma(3,1)*nux
        taondz = taondz + sigma(3,2)*nuy
        taondz = taondz + sigma(3,3)*nuz
!
!        --- CALCUL DU VECTEUR ELEMENTAIRE
!
        do i = 1, nno
            ii = 3*i-2
            zr(ires+ii-1) = zr(ires+ii-1) + (taux+coedir*taondx)*zr( ivf+ldec+i-1)*jac*zr(ipoids+&
                            &ipg-1)
            zr(ires+ii+1-1) = zr(ires+ii+1-1) + (tauy+coedir*taondy)* zr(ivf+ldec+i-1)*jac*zr(ipo&
                              &ids+ipg-1)
            zr(ires+ii+2-1) = zr(ires+ii+2-1) + (tauz+coedir*taondz)* zr(ivf+ldec+i-1)*jac*zr(ipo&
                              &ids+ipg-1)
        enddo
!
    enddo
!
99  continue
!
end subroutine
