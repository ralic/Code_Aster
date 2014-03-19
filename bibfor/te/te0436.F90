subroutine te0436(option, nomte)
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
! aslint: disable=W0104
    implicit none
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterc/r8prem.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/mbcine.h"
#include "asterfort/mbrigi.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
#include "asterfort/verift.h"
!
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
!    - FONCTION REALISEE:  CALCUL DES OPTIONS DE POST-TRAITEMENT :
!                                  - SIEF_ELGA
!                                  - EPOT_ELEM
!                                  - EPSI_ELGA
!                                  - MASS_INER
!                          POUR LES MEMBRANES
!    - ARGUMENTS :
!        DONNEES :      OPTION       -->  OPTION DE CALCUL
!                       NOMTE        -->  NOM DU TYPE ELEMENT
! ----------------------------------------------------------------------
!
    integer :: codres(2)
    character(len=4) :: fami
    character(len=8) :: materi
    integer :: nddl, nno, nnos, npg, ndim, ncomp
    integer :: i, j, n, c, cc, kpg, iret
    integer :: ipoids, ivf, idfde, jgano
    integer :: igeom, icacoq, imate, idepl, icontp, inr, idefo, imass
    real(kind=8) :: dff(2, 8), vff(8), b(3, 3, 8), jac
    real(kind=8) :: alpha, beta, epot
    real(kind=8) :: epsm(3), epsg(3, 9), epsthe, sig(3), sigg(3, 9), rig(6, 6)
    real(kind=8) :: rho(1)
    real(kind=8) :: x(8), y(8), z(8), surfac, cdg(3), ppg, xxi, yyi, zzi
    real(kind=8) :: matine(6)
    real(kind=8) :: vro
!
    materi = ' '
!
! - NOMBRE DE COMPOSANTES DES TENSEURS
!
    ncomp = 3
    nddl = 3
!
! - FONCTIONS DE FORMES ET POINTS DE GAUSS
!
    fami = 'RIGI'
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
! - PARAMETRES EN ENTREE
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PCACOQU', 'L', icacoq)
!
    if ((option.eq.'SIEF_ELGA') .or. (option.eq.'EPOT_ELEM')) then
        call jevech('PDEPLAR', 'L', idepl)
        call jevech('PMATERC', 'L', imate)
!
    else if (option.eq.'EPSI_ELGA') then
        call jevech('PDEPLAR', 'L', idepl)
        call r8inir(3*9, 0.d0, epsg, 1)
!
    else if (option.eq.'MASS_INER') then
        call jevech('PMATERC', 'L', imate)
    endif
!
! - PARAMETRES EN SORTIE
!
    if (option .eq. 'SIEF_ELGA') then
        call jevech('PCONTRR', 'E', icontp)
        call r8inir(3*9, 0.d0, sigg, 1)
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
                    ' ', 'ELAS_MEMBRANE', 0, ' ', [0.d0],&
                    1, 'RHO', rho, codres, 1)
        if (rho(1) .le. r8prem()) then
            call utmess('F', 'ELEMENTS5_45')
        endif
    endif
!
! - LE VECTEUR NORME QUI DETERMINE LE REPERE LOCAL DE LA MEMBRANE
!   (COMPORTEMENT ANISOTROPE)
!
    alpha = zr(icacoq) * r8dgrd()
    beta = zr(icacoq+1) * r8dgrd()
!
! - COORDONNEES PHYSIQUES DES NOEUDS
!
    if (option .eq. 'MASS_INER') then
        do 10 i = 1, nno
            x(i) = zr(igeom+3* (i-1))
            y(i) = zr(igeom+3*i-2)
            z(i) = zr(igeom+3*i-1)
 10     continue
        call r8inir(3, 0.d0, cdg, 1)
        call r8inir(6, 0.d0, matine, 1)
        surfac = 0.d0
    endif
!
!
! - DEBUT DE LA BOUCLE SUR LES POINTS DE GAUSS
!
    do 800 kpg = 1, npg
!
! --- MISE SOUS FORME DE TABLEAU DES VALEURS ET DES DERIVEES
!     DES FONCTIONS DE FORME
!
        do 110 n = 1, nno
            vff(n) =zr(ivf+(kpg-1)*nno+n-1)
            dff(1,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2)
            dff(2,n)=zr(idfde+(kpg-1)*nno*2+(n-1)*2+1)
110     continue
!
! --- CALCUL DE LA MATRICE "B" :
!              DEPL NODAL --> DEFORMATIONS MEMBRANAIRES ET JACOBIEN
!
        call mbcine(nno, zr(igeom), dff, alpha, beta,&
                    b, jac)
!
! --- SIEF_ELGA, EPOT_ELEM : ON CALCULE LA CONTRAINTE AU PG
!
        if ((option.eq.'SIEF_ELGA') .or. (option.eq.'EPOT_ELEM')) then
!
!         CALCUL DE LA DEFORMATION MEMBRANAIRE DANS LE REPERE LOCAL
            call r8inir(3, 0.d0, epsm, 1)
            do 130 n = 1, nno
                do 130 i = 1, nddl
                    do 130 c = 1, ncomp
                        epsm(c)=epsm(c)+b(c,i,n)*zr(idepl+(n-1)*nddl+&
                        i-1)
130                 continue
!
!         RETRAIT DE LA DEFORMATION THERMIQUE
            call verift(fami, kpg, 1, '+', zi(imate),&
                        materi, 'ELAS_MEMBRANE', iret, epsth=epsthe)
            epsm(1) = epsm(1) - epsthe
            epsm(2) = epsm(2) - epsthe
!
!         CALCUL DE LA CONTRAINTE AU PG
            call mbrigi(fami, kpg, imate, rig)
!
            call r8inir(3, 0.d0, sig, 1)
            do 140 c = 1, ncomp
                do 140 cc = 1, ncomp
                    sig(c) = sig(c) + epsm(cc)*rig(cc,c)
140             continue
!
            if (option .eq. 'EPOT_ELEM') then
                do 150 c = 1, ncomp
                    epot = epot+(sig(c)*epsm(c)*zr(ipoids+kpg-1)*jac)/ 2
150             continue
            else
                do 160 c = 1, ncomp
                    sigg(c,kpg) = sig(c)
160             continue
            endif
!
! --- EPSI_ELGA : ON CALCULE LA DEFORMATION AU PG
!
        else if (option.eq.'EPSI_ELGA') then
!
!         CALCUL DE LA DEFORMATION MEMBRANAIRE DANS LE REPERE LOCAL
            do 200 n = 1, nno
                do 200 i = 1, nddl
                    do 200 c = 1, ncomp
                        epsg(c,kpg)= epsg(c,kpg) + b(c,i,n)*zr(idepl+(&
                        n-1)*nddl+i-1)
200                 continue
!
! --- MASS_INER : ON SOMME LA CONTRIBUTION DU PG A LA MASSE TOTALE
!
        else if (option.eq.'MASS_INER') then
            surfac = surfac + zr(ipoids+kpg-1)*jac
            ppg = zr(ipoids+kpg-1)*jac
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
310             continue
                matine(1) = matine(1) + ppg*(yyi+zzi)
                matine(3) = matine(3) + ppg*(xxi+zzi)
                matine(6) = matine(6) + ppg*(xxi+yyi)
300         continue
        endif
!
! - FIN DE LA BOUCLE SUR LES POINTS DE GAUSS
800 end do
!
    if (option .eq. 'SIEF_ELGA') then
        do 500 kpg = 1, npg
            do 500 c = 1, ncomp
                zr(icontp+(kpg-1)*ncomp+c-1)=sigg(c,kpg)
500         continue
!
    else if (option.eq.'EPOT_ELEM') then
        zr(inr) = epot
!
    else if (option.eq.'EPSI_ELGA') then
        do 510 kpg = 1, npg
            do 510 c = 1, ncomp
                zr(idefo+(kpg-1)*ncomp+c-1) = epsg(c,kpg)
510         continue
!
    else if (option.eq.'MASS_INER') then
        vro = rho(1) / surfac
        zr(imass) = rho(1) * surfac
        zr(imass+1) = cdg(1)/surfac
        zr(imass+2) = cdg(2)/surfac
        zr(imass+3) = cdg(3)/surfac
        zr(imass+4) = matine(1)*rho(1) - vro*(cdg(2)*cdg(2)+cdg(3)*cdg(3) )
        zr(imass+5) = matine(3)*rho(1) - vro*(cdg(1)*cdg(1)+cdg(3)*cdg(3) )
        zr(imass+6) = matine(6)*rho(1) - vro*(cdg(1)*cdg(1)+cdg(2)*cdg(2) )
        zr(imass+7) = matine(2)*rho(1) - vro*(cdg(1)*cdg(2))
        zr(imass+8) = matine(4)*rho(1) - vro*(cdg(1)*cdg(3))
        zr(imass+9) = matine(5)*rho(1) - vro*(cdg(2)*cdg(3))
    endif
!
end subroutine
