subroutine te0433(option, nomte)
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
    include 'jeveux.h'
    include 'asterc/r8prem.h'
    include 'asterfort/cargri.h'
    include 'asterfort/dxqpgl.h'
    include 'asterfort/dxtpgl.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/nmgrib.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/verift.h'
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES OPTIONS DE POST-TRAITEMENT :
!                                  - SIEF_ELGA
!                                  - EPOT_ELEM
!                                  - EPSI_ELGA
!                                  - MASS_INER
!                          POUR LES GRILLES MEMBRANES EXCENTREES OU NON
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: codres(2)
    character(len=4) :: fami
    character(len=8) :: nomres(2)
    integer :: nddl, nno, nnos, npg, ndim, i, j, n, kpg
    integer :: jgano, ipoids, ivf, idfde, igeom, imate
    integer :: icontp, iret, imass, idepl, idefo, inr
    real(kind=8) :: dff(2, 8), vff(8), b(6, 8), p(3, 6), jac
    real(kind=8) :: dir11(3), densit, pgl(3, 3), distn, vecn(3)
    real(kind=8) :: epsm, epsg(9), epsthe, sig, sigg(9), rho, valres(2), epot
    real(kind=8) :: x(8), y(8), z(8), volume, cdg(3), ppg, xxi, yyi, zzi
    real(kind=8) :: matine(6), vro
    logical :: lexc
!
! - BOOLEEN POUR LES GRILLES EXCENTREES
!
    lexc = (nomte(1:4).eq.'MEGC')
!
! - FONCTIONS DE FORMES ET POINTS DE GAUSS
!
    fami = 'RIGI'
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
! - PARAMETRES EN ENTREE
!
    call jevech('PGEOMER', 'L', igeom)
!
    if ((option.eq.'SIEF_ELGA') .or. (option.eq.'EPOT_ELEM')) then
        call jevech('PDEPLAR', 'L', idepl)
        call jevech('PMATERC', 'L', imate)
!
    else if (option.eq.'EPSI_ELGA') then
        call jevech('PDEPLAR', 'L', idepl)
        call r8inir(9, 0.d0, epsg, 1)
!
    else if (option.eq.'MASS_INER') then
        call jevech('PMATERC', 'L', imate)
    endif
!
! - PARAMETRES EN SORTIE
!
    if (option .eq. 'SIEF_ELGA') then
        call jevech('PCONTRR', 'E', icontp)
        call r8inir(9, 0.d0, sigg, 1)
!
    else if (option.eq.'EPOT_ELEM') then
        call jevech('PENERDR', 'E', inr)
        epot = 0.d0
!
    else if (option.eq.'EPSI_ELGA') then
        call jevech('PDEFOPG', 'E', idefo)
!
    else if (option.eq.'MASS_INER') then
        call jevech('PMASSINE', 'E', imass)
        call rcvalb(fami, kpg, 1, '+', zi(imate),&
                    ' ', 'ELAS', 0, ' ', 0.d0,&
                    1, 'RHO', rho, codres, 1)
        if (rho .le. r8prem()) then
            call u2mess('F', 'ELEMENTS5_45')
        endif
    endif
!
! - LECTURE DES CARACTERISTIQUES DE GRILLE ET
!   CALCUL DE LA DIRECTION D'ARMATURE
!
    call cargri(lexc, densit, distn, dir11)
!
! - SI EXCENTREE : RECUPERATION DE LA NORMALE ET DE L'EXCENTREMENT
!
    if (lexc) then
!
        if (nomte .eq. 'MEGCTR3') then
            call dxtpgl(zr(igeom), pgl)
        else if (nomte.eq.'MEGCQU4') then
            call dxqpgl(zr(igeom), pgl, 'S', iret)
        endif
!
        do 8 i = 1, 3
            vecn(i)=distn*pgl(3,i)
 8      continue
        nddl=6
!
    else
        nddl = 3
    endif
!
! - COORDONNEES PHYSIQUES DES NOEUDS
!
    if (option .eq. 'MASS_INER') then
        do 40 i = 1, nno
            x(i) = zr(igeom+3* (i-1))
            y(i) = zr(igeom+3*i-2)
            z(i) = zr(igeom+3*i-1)
40      continue
        if (lexc) then
            x(i) = x(i) + vecn(1)
            y(i) = y(i) + vecn(2)
            z(i) = z(i) + vecn(3)
        endif
        call r8inir(3, 0.d0, cdg, 1)
        call r8inir(6, 0.d0, matine, 1)
    endif
!
    volume = 0.d0
!
! - DEBUT DE LA BOUCLE SUR LES POINTS DE GAUSS
!
    do 800 kpg = 1, npg
!
! --- MISE SOUS FORME DE TABLEAU DES VALEURS DES FONCTIONS DE FORME
!     ET DES DERIVEES DE FONCTION DE FORME
!
        do 11 n = 1, nno
            vff(n) =zr(ivf+(kpg-1)*nno+n-1)
            dff(1,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2)
            dff(2,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2+1)
11      continue
!
! --- CALCUL DE LA MATRICE "B" : DEPL NODAL --> EPS11 ET DU JACOBIEN
!
        call nmgrib(nno, zr(igeom), dff, dir11, lexc,&
                    vecn, b, jac, p)
!
! --- SIEF_ELGA, EPOT_ELEM : ON CALCULE LA CONTRAINTE AU PG
!
        if ((option.eq.'SIEF_ELGA') .or. (option.eq.'EPOT_ELEM')) then
!
!         CALCUL DE LA DEFORMATION AU PG
            epsm=0.d0
            do 210 i = 1, nno
                do 210 j = 1, nddl
                    epsm=epsm+b(j,i)*zr(idepl+(i-1)*nddl+j-1)
210              continue
!
!         CALCUL DE LA CONTRAINTE
            call verift(fami, kpg, 1, '+', zi(imate),&
                        'ELAS', 1, epsthe, iret)
            nomres(1) = 'E'
            call rcvalb(fami, kpg, 1, '+', zi(imate),&
                        ' ', 'ELAS', 0, ' ', 0.d0,&
                        1, nomres, valres, codres, 0)
            epsm=epsm-epsthe
            sig = valres(1)*epsm
!
            if (option .eq. 'EPOT_ELEM') then
                epot = epot+(sig*epsm*zr(ipoids+kpg-1)*jac*densit)/2
            else
                sigg(kpg) = sig
            endif
!
! --- EPSI_ELGA : ON CALCULE LA DEFORMATION AU PG
!
        else if (option.eq.'EPSI_ELGA') then
!
!         CALCUL DE LA DEFORMATION AU PG
            do 30 i = 1, nno
                do 30 j = 1, nddl
                    epsg(kpg)=epsg(kpg)+b(j,i)*zr(idepl+(i-1)*nddl+j-&
                    1)
30              continue
!
! --- MASS_INER : ON SOMME LA CONTRIBUTION DU PG A LA MASSE TOTALE
!
        else if (option.eq.'MASS_INER') then
            volume = volume + zr(ipoids+kpg-1)*densit*jac
            ppg = zr(ipoids+kpg-1)*jac*densit
            do 300 i = 1, nno
                cdg(1) = cdg(1) + ppg*vff(i)*x(i)
                cdg(2) = cdg(2) + ppg*vff(i)*y(i)
                cdg(3) = cdg(3) + ppg*vff(i)*z(i)
                xxi = 0.d0
                yyi = 0.d0
                zzi = 0.d0
                do 310 j = 1, nno
                    xxi = xxi + x(i)*vff(i)*vff(j)*x(j)
                    yyi = yyi + y(i)*vff(i)*vff(j)*y(j)
                    zzi = zzi + z(i)*vff(i)*vff(j)*z(j)
                    matine(2) = matine(2) + x(i)*vff(i)*vff(j)*y(j)* ppg
                    matine(4) = matine(4) + x(i)*vff(i)*vff(j)*z(j)* ppg
                    matine(5) = matine(5) + y(i)*vff(i)*vff(j)*z(j)* ppg
310              continue
                matine(1) = matine(1) + ppg*(yyi+zzi)
                matine(3) = matine(3) + ppg*(xxi+zzi)
                matine(6) = matine(6) + ppg*(xxi+yyi)
300          continue
        endif
!
! - FIN DE LA BOUCLE SUR LES POINTS DE GAUSS
800  end do
!
    if (option .eq. 'SIEF_ELGA') then
        do 510 kpg = 1, npg
            zr(icontp+kpg-1)=sigg(kpg)
510      continue
!
    else if (option.eq.'EPOT_ELEM') then
        zr(inr) = epot
!
    else if (option.eq.'EPSI_ELGA') then
        do 500 kpg = 1, npg
            zr(idefo+kpg-1)=epsg(kpg)
500      continue
!
    else if (option.eq.'MASS_INER') then
        vro = rho / volume
        zr(imass) = rho * volume
        zr(imass+1) = cdg(1)/volume
        zr(imass+2) = cdg(2)/volume
        zr(imass+3) = cdg(3)/volume
        zr(imass+4) = matine(1)*rho - vro*(cdg(2)*cdg(2)+cdg(3)*cdg(3) )
        zr(imass+5) = matine(3)*rho - vro*(cdg(1)*cdg(1)+cdg(3)*cdg(3) )
        zr(imass+6) = matine(6)*rho - vro*(cdg(1)*cdg(1)+cdg(2)*cdg(2) )
        zr(imass+7) = matine(2)*rho - vro*(cdg(1)*cdg(2))
        zr(imass+8) = matine(4)*rho - vro*(cdg(1)*cdg(3))
        zr(imass+9) = matine(5)*rho - vro*(cdg(2)*cdg(3))
    endif
!
end subroutine
