subroutine te0433(option, nomte)
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
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/cargri.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/nmgrib.h"
#include "asterfort/r8inir.h"
#include "asterfort/lteatt.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
#include "asterfort/verift.h"
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
    integer :: codres(2), iret
    character(len=4) :: fami
    character(len=16) :: nomres(2)
    integer :: nddl, nno, nnos, npg, ndim, i, j, n, kpg
    integer :: jgano, ipoids, ivf, idfde, igeom, imate
    integer :: icontp, imass, idepl, idefo, inr
    real(kind=8) :: dff(2, 8), vff(8), b(6, 8), p(3, 6), jac
    real(kind=8) :: dir11(3), densit, pgl(3, 3), distn, vecn(3)
    real(kind=8) :: epsm, epsg(9), epsthe, sig, sigg(9), rho(1), valres(2), epot
    real(kind=8) :: x(8), y(8), z(8), volume, cdg(3), ppg, xxi, yyi, zzi
    real(kind=8) :: matine(6), vro
    aster_logical :: lexc
!
! - BOOLEEN POUR LES GRILLES EXCENTREES
!
    lexc = (lteatt('MODELI','GRC'))
!
! - FONCTIONS DE FORMES ET POINTS DE GAUSS
!
    fami = 'RIGI'
    call elrefe_info(fami='RIGI', ndim=ndim, nno=nno, nnos=nnos, npg=npg,&
                     jpoids=ipoids, jvf=ivf, jdfde=idfde, jgano=jgano)
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
                    ' ', 'ELAS', 0, ' ', [0.d0],&
                    1, 'RHO', rho, codres, 1)
        if (rho(1) .le. r8prem()) then
            call utmess('F', 'ELEMENTS5_45')
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
        do i = 1, 3
            vecn(i)=distn*pgl(3,i)
        end do
        nddl=6
!
    else
        nddl = 3
    endif
!
! - COORDONNEES PHYSIQUES DES NOEUDS
!
    if (option .eq. 'MASS_INER') then
        do i = 1, nno
            x(i) = zr(igeom+3* (i-1))
            y(i) = zr(igeom+3*i-2)
            z(i) = zr(igeom+3*i-1)
        end do
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
    do kpg = 1, npg
!
! --- MISE SOUS FORME DE TABLEAU DES VALEURS DES FONCTIONS DE FORME
!     ET DES DERIVEES DE FONCTION DE FORME
!
        do n = 1, nno
            vff(n) =zr(ivf+(kpg-1)*nno+n-1)
            dff(1,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2)
            dff(2,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2+1)
        end do
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
            do i = 1, nno
                do j = 1, nddl
                    epsm=epsm+b(j,i)*zr(idepl+(i-1)*nddl+j-1)
                end do
            end do
!
!         CALCUL DE LA CONTRAINTE
            call verift(fami, kpg, 1, '+', zi(imate),&
                        epsth_=epsthe)
            nomres(1) = 'E'
            call rcvalb(fami, kpg, 1, '+', zi(imate),&
                        ' ', 'ELAS', 0, ' ', [0.d0],&
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
            do i = 1, nno
                do j = 1, nddl
                    epsg(kpg)=epsg(kpg)+b(j,i)*zr(idepl+(i-1)*nddl+j-1)
                end do
            end do
!
! --- MASS_INER : ON SOMME LA CONTRIBUTION DU PG A LA MASSE TOTALE
!
        else if (option.eq.'MASS_INER') then
            volume = volume + zr(ipoids+kpg-1)*densit*jac
            ppg = zr(ipoids+kpg-1)*jac*densit
            do i = 1, nno
                cdg(1) = cdg(1) + ppg*vff(i)*x(i)
                cdg(2) = cdg(2) + ppg*vff(i)*y(i)
                cdg(3) = cdg(3) + ppg*vff(i)*z(i)
                xxi = 0.d0
                yyi = 0.d0
                zzi = 0.d0
                do j = 1, nno
                    xxi = xxi + x(i)*vff(i)*vff(j)*x(j)
                    yyi = yyi + y(i)*vff(i)*vff(j)*y(j)
                    zzi = zzi + z(i)*vff(i)*vff(j)*z(j)
                    matine(2) = matine(2) + x(i)*vff(i)*vff(j)*y(j)* ppg
                    matine(4) = matine(4) + x(i)*vff(i)*vff(j)*z(j)* ppg
                    matine(5) = matine(5) + y(i)*vff(i)*vff(j)*z(j)* ppg
                end do
                matine(1) = matine(1) + ppg*(yyi+zzi)
                matine(3) = matine(3) + ppg*(xxi+zzi)
                matine(6) = matine(6) + ppg*(xxi+yyi)
            end do
        endif
!
! - FIN DE LA BOUCLE SUR LES POINTS DE GAUSS
    end do
!
    if (option .eq. 'SIEF_ELGA') then
        do kpg = 1, npg
            zr(icontp+kpg-1)=sigg(kpg)
        end do
!
    else if (option.eq.'EPOT_ELEM') then
        zr(inr) = epot
!
    else if (option.eq.'EPSI_ELGA') then
        do kpg = 1, npg
            zr(idefo+kpg-1)=epsg(kpg)
        end do
!
    else if (option.eq.'MASS_INER') then
        vro = rho(1) / volume
        zr(imass) = rho(1) * volume
        zr(imass+1) = cdg(1)/volume
        zr(imass+2) = cdg(2)/volume
        zr(imass+3) = cdg(3)/volume
        zr(imass+4) = matine(1)*rho(1) - vro*(cdg(2)*cdg(2)+cdg(3)*cdg(3) )
        zr(imass+5) = matine(3)*rho(1) - vro*(cdg(1)*cdg(1)+cdg(3)*cdg(3) )
        zr(imass+6) = matine(6)*rho(1) - vro*(cdg(1)*cdg(1)+cdg(2)*cdg(2) )
        zr(imass+7) = matine(2)*rho(1) - vro*(cdg(1)*cdg(2))
        zr(imass+8) = matine(4)*rho(1) - vro*(cdg(1)*cdg(3))
        zr(imass+9) = matine(5)*rho(1) - vro*(cdg(2)*cdg(3))
    endif
!
end subroutine
