subroutine te0498(option, nomte)
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
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/elref4.h'
    include 'asterfort/fointe.h'
    include 'asterfort/jevech.h'
    include 'asterfort/pronor.h'
    include 'asterfort/rcvalb.h'
    character(len=16) :: option, nomte
! ......................................................................
!
!     BUT: CALCUL DES VECTEURS ELEMENTAIRES EN MECANIQUE
!          CORRESPONDANT A UN CHARGEMENT PAR ONDE PLANE
!          SUR DES FACES D'ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'IMPE_ABSO'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: ipoids, ivf, idfdx, idfdy, igeom, i, j
    integer :: ndim, nno, ipg, npg1, ino, jno
    integer :: idec, jdec, kdec, ldec, ires, imate
    integer :: ii, mater, jinst, indic1, indic2
    integer :: ionde, iondc, ier, nnos, jgano
    real(kind=8) :: jac, nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9)
    real(kind=8) :: valres(3), e, nu, lambda, mu, cp, cs, rho, typer
    real(kind=8) :: taux, tauy, tauz, dirx, diry, dirz
    real(kind=8) :: norm, tanx, tany, norx, nory, norz
    real(kind=8) :: taondx, taondy, taondz
    real(kind=8) :: nux, nuy, nuz, scal, coedir
    real(kind=8) :: r8b, nortan, cele, trace
    real(kind=8) :: sigma(3, 3), epsi(3, 3), grad(3, 3), valfon
    real(kind=8) :: vondn(3), vondt(3)
    integer :: icodre(3), kpg, spt
    character(len=2) :: type
    character(len=8) :: nomres(3), fami, poum
!     ------------------------------------------------------------------
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfdx, jgano)
    idfdy = idfdx + 1
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PONDPLA', 'L', ionde)
    call jevech('PONDPLR', 'L', iondc)
    call jevech('PTEMPSR', 'L', jinst)
    call jevech('PVECTUR', 'E', ires)
!
    if (zk24(ionde)(1:7) .eq. '&FOZERO') goto 140
!
!     --- INITIALISATION DE SIGMA
!
    do 21 i = 1, 3
        do 21 j = 1, 3
            sigma(i,j) = 0.d0
21      continue
!
    mater=zi(imate)
    nomres(1)='E'
    nomres(2)='NU'
    nomres(3)='RHO'
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, mater,&
                ' ', 'ELAS', 0, ' ', r8b,&
                3, nomres, valres, icodre, 1)
    e = valres(1)
    nu = valres(2)
    rho = valres(3)
!
    lambda = e*nu/(1.d0+nu)/(1.d0-2.d0*nu)
    mu = e/2.d0/(1.d0+nu)
!
    cp = sqrt((lambda+2.d0*mu)/rho)
    cs = sqrt(mu/rho)
!
!     --- CARACTERISTIQUES DE L'ONDE PLANE
!
    dirx =zr(iondc)
    diry =zr(iondc+1)
    dirz =zr(iondc+2)
    typer=zr(iondc+3)
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
    do 30 ino = 1, nno
        i = igeom + 3*(ino-1) -1
        do 30 jno = 1, nno
            j = igeom + 3*(jno-1) -1
            sx(ino,jno) = zr(i+2)*zr(j+3) - zr(i+3)*zr(j+2)
            sy(ino,jno) = zr(i+3)*zr(j+1) - zr(i+1)*zr(j+3)
            sz(ino,jno) = zr(i+1)*zr(j+2) - zr(i+2)*zr(j+1)
30      continue
!
!     --- BOUCLE SUR LES POINTS DE GAUSS ---
!
    do 100 ipg = 1, npg1
!
        kdec = (ipg-1)*nno*ndim
        ldec = (ipg-1)*nno
!
!        --- CALCUL DU CHARGEMENT PAR ONDE PLANE
!KH          ON SUPPOSE QU'ON RECUPERE UNE VITESSE
        call fointe('FM', zk24(ionde), 1, 'INST', zr(jinst),&
                    valfon, ier)
        valfon = -valfon/cele
!         VALFON = +VALFON/CELE
!
!        CALCUL DES CONTRAINTES ASSOCIEES A L'ONDE PLANE
!        CALCUL DU GRADIENT DU DEPLACEMENT
        if (type .eq. 'P') then
!
            grad(1,1) = dirx*valfon*dirx
            grad(1,2) = diry*valfon*dirx
            grad(1,3) = dirz*valfon*dirx
!
            grad(2,1) = dirx*valfon*diry
            grad(2,2) = diry*valfon*diry
            grad(2,3) = dirz*valfon*diry
!
            grad(3,1) = dirx*valfon*dirz
            grad(3,2) = diry*valfon*dirz
            grad(3,3) = dirz*valfon*dirz
!
        else if (type.eq.'SV') then
!
            grad(1,1) = dirx*valfon*norx
            grad(1,2) = diry*valfon*norx
            grad(1,3) = dirz*valfon*norx
!
            grad(2,1) = dirx*valfon*nory
            grad(2,2) = diry*valfon*nory
            grad(2,3) = dirz*valfon*nory
!
            grad(3,1) = dirx*valfon*norz
            grad(3,2) = diry*valfon*norz
            grad(3,3) = dirz*valfon*norz
!
        else if (type.eq.'SH') then
!
            grad(1,1) = dirx*valfon*tanx
            grad(1,2) = diry*valfon*tanx
            grad(1,3) = dirz*valfon*tanx
!
            grad(2,1) = dirx*valfon*tany
            grad(2,2) = diry*valfon*tany
            grad(2,3) = dirz*valfon*tany
!
            grad(3,1) = 0.d0
            grad(3,2) = 0.d0
            grad(3,3) = 0.d0
!
        endif
!
!        CALCUL DES DEFORMATIONS
        do 201 indic1 = 1, 3
            do 201 indic2 = 1, 3
                epsi(indic1,indic2) = .5d0*( grad(indic1,indic2) + grad(indic2,indic1) )
201          continue
!
!        CALCUL DES CONTRAINTES
        trace = 0.d0
        do 203 indic1 = 1, 3
            trace = trace + epsi(indic1,indic1)
203      continue
!
        do 204 indic1 = 1, 3
            do 204 indic2 = 1, 3
                if (indic1 .eq. indic2) then
                    sigma(indic1,indic2) = lambda*trace +2.d0*mu*epsi( indic1,indic2)
                else
                    sigma(indic1,indic2) = 2.d0*mu*epsi(indic1,indic2)
                endif
204          continue
!
        nx = 0.0d0
        ny = 0.0d0
        nz = 0.0d0
!
!        --- CALCUL DE LA NORMALE AU POINT DE GAUSS IPG ---
!
        do 102 i = 1, nno
            idec = (i-1)*ndim
            do 102 j = 1, nno
                jdec = (j-1)*ndim
                nx = nx + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sx(i,j)
                ny = ny + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sy(i,j)
                nz = nz + zr(idfdx+kdec+idec) * zr(idfdy+kdec+jdec) * sz(i,j)
102          continue
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
!
!        --- CALCUL DE V.N ---
!
        vondt(1) = 0.d0
        vondt(2) = 0.d0
        vondt(3) = 0.d0
!
        if (type .eq. 'P') then
            vondt(1) = -cele*valfon*dirx
            vondt(2) = -cele*valfon*diry
            vondt(3) = -cele*valfon*dirz
        else if (type.eq.'SV') then
            vondt(1) = -cele*valfon*norx
            vondt(2) = -cele*valfon*nory
            vondt(3) = -cele*valfon*norz
        else if (type.eq.'SH') then
            vondt(1) = -cele*valfon*tanx
            vondt(2) = -cele*valfon*tany
            vondt(3) = 0.d0
        endif
!
!        --- CALCUL DE LA VITESSE NORMALE ET DE LA VITESSE TANGENCIELLE
        call pronor(nux, nuy, nuz, vondt, vondn)
!
!        --- CALCUL DU VECTEUR CONTRAINTE
!
        taux = - rho*(cp*vondn(1) + cs*vondt(1))
        tauy = - rho*(cp*vondn(2) + cs*vondt(2))
        tauz = - rho*(cp*vondn(3) + cs*vondt(3))
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
        do 100 i = 1, nno
            ii = 3*i-2
            zr(ires+ii-1) = zr(ires+ii-1) + (taux+coedir*taondx)*zr( ivf+ldec+i-1)*jac*zr(ipoids+&
                            &ipg-1)
            zr(ires+ii+1-1) = zr(ires+ii+1-1) + (tauy+coedir*taondy)* zr(ivf+ldec+i-1)*jac*zr(ipo&
                              &ids+ipg-1)
            zr(ires+ii+2-1) = zr(ires+ii+2-1) + (tauz+coedir*taondz)* zr(ivf+ldec+i-1)*jac*zr(ipo&
                              &ids+ipg-1)
100      continue
!
140  continue
!
end subroutine
