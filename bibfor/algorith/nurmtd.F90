subroutine nurmtd(ndim, nno1, nno2, npg, iw,&
                  vff1, vff2, ivf1, idff1, vu,&
                  vp, typmod, igeom, mate, mini,&
                  matr)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: sebastien.fayolle at edf.fr
! aslint: disable=W1306
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/calkbb.h"
#include "asterfort/calkbp.h"
#include "asterfort/calkce.h"
#include "asterfort/dfdmip.h"
#include "asterfort/ortrep.h"
#include "asterfort/pmat.h"
#include "asterfort/r8inir.h"
#include "asterfort/tanbul.h"
    aster_logical :: mini
    integer :: ndim, nno1, nno2, npg, iw, idff1
    integer :: mate
    integer :: vu(3, 27), vp(27)
    integer :: ivf1, igeom
    real(kind=8) :: vff1(nno1, npg), vff2(nno2, npg)
    character(len=8) :: typmod(*)
    real(kind=8) :: matr(*)
!
!-----------------------------------------------------------------------
!          CALCUL DE LA RIGIDITE MECANIQUE POUR LES ELEMENTS
!          INCOMPRESSIBLES POUR LES PETITES DEFORMATIONS
!          3D/D_PLAN/AXIS
!          ROUTINE APPELEE PAR TE0596
!-----------------------------------------------------------------------
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  NNO1    : NOMBRE DE NOEUDS DE L'ELEMENT LIES AUX DEPLACEMENTS
! IN  NNO2    : NOMBRE DE NOEUDS DE L'ELEMENT LIES A LA PRESSION
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  IW      : POIDS DES POINTS DE GAUSS
! IN  VFF1    : VALEUR  DES FONCTIONS DE FORME LIES AUX DEPLACEMENTS
! IN  VFF2    : VALEUR  DES FONCTIONS DE FORME LIES A LA PRESSION
! IN  IDFF1   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  VU      : TABLEAU DES INDICES DES DDL DE DEPLACEMENTS
! IN  VP      : TABLEAU DES INDICES DES DDL DE PRESSION
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  IGEOM   : POINTEUR SUR LES COORDONEES DES NOEUDS
! IN  MATE    : MATERIAU CODE
! IN  MINI    : STABILISATION BULLE - MINI ELEMENT
! OUT MATR    : MATRICE DE RIGIDITE
!-----------------------------------------------------------------------
!
    aster_logical :: axi
    integer :: g
    integer :: ia, na, sa, ib, nb, sb, ja, jb
    integer :: os, kk
    integer :: vuiana, vpsa
    integer :: idim
    integer :: idecpg, idecno
    real(kind=8) :: rac2
    real(kind=8) :: r, w, dff1(nno1, ndim)
    real(kind=8) :: dsidep(2*ndim, 2*ndim)
    real(kind=8) :: def(2*ndim, nno1, ndim), deftr(nno1, ndim)
    real(kind=8) :: ddev(2*ndim, 2*ndim), devd(2*ndim, 2*ndim)
    real(kind=8) :: dddev(2*ndim, 2*ndim)
    real(kind=8) :: xyzgau(3), bary(3), repere(7)
    real(kind=8) :: t1
    real(kind=8) :: idev(6, 6), idev2(4, 4)
    real(kind=8) :: alpha, trepst
    real(kind=8) :: presm(nno2), presd(nno2)
    real(kind=8) :: kbb(ndim, ndim), kbp(ndim, nno2)
    real(kind=8) :: kce(nno2, nno2), rce(nno2)
    real(kind=8) :: fm(3, 3)
    character(len=16) :: compor, option
!
    data         fm   / 1.d0, 0.d0, 0.d0,&
     &                  0.d0, 1.d0, 0.d0,&
     &                  0.d0, 0.d0, 1.d0/
    data         idev2/ 2.d0,-1.d0,-1.d0, 0.d0,&
     &                 -1.d0, 2.d0,-1.d0, 0.d0,&
     &                 -1.d0,-1.d0, 2.d0, 0.d0,&
     &                  0.d0, 0.d0, 0.d0, 3.d0/
    data         idev / 2.d0,-1.d0,-1.d0, 0.d0, 0.d0, 0.d0,&
     &                 -1.d0, 2.d0,-1.d0, 0.d0, 0.d0, 0.d0,&
     &                 -1.d0,-1.d0, 2.d0, 0.d0, 0.d0, 0.d0,&
     &                  0.d0, 0.d0, 0.d0, 3.d0, 0.d0, 0.d0,&
     &                  0.d0, 0.d0, 0.d0, 0.d0, 3.d0, 0.d0,&
     &                  0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 3.d0/
!-----------------------------------------------------------------------
!
! - INITIALISATION
    axi = typmod(1).eq.'AXIS'
    rac2 = sqrt(2.d0)
    option = 'RIGI_MECA       '
    compor = 'ELAS            '
!
    call r8inir(nno2, 0.d0, presm, 1)
    call r8inir(nno2, 0.d0, presd, 1)
!
! - RECUPERATION  DES DONNEEES RELATIVES AU REPERE D'ORTHOTROPIE
! - COORDONNEES DU BARYCENTRE ( POUR LE REPRE CYLINDRIQUE )
    bary(1) = 0.d0
    bary(2) = 0.d0
    bary(3) = 0.d0
    do ia = 1, nno1
        do idim = 1, ndim
            bary(idim) = bary(idim)+zr(igeom+idim+ndim*(ia-1)-1)/nno1
        end do
    end do
    call ortrep(mate, ndim, bary, repere)
!
! - CALCUL POUR CHAQUE POINT DE GAUSS
    do g = 1, npg
        idecpg = nno1* (g-1) - 1
!
! - COORDONNEES AU POINT D'INTEGRATION COURANT
        xyzgau(1) = 0.d0
        xyzgau(2) = 0.d0
        xyzgau(3) = 0.d0
        if (ndim .eq. 3) then
            do ia = 1, nno1
                idecno = 3* (ia-1) - 1
                xyzgau(1) = xyzgau(1)+zr(ivf1+ia+idecpg)*zr(igeom+1+ idecno)
                xyzgau(2) = xyzgau(2)+zr(ivf1+ia+idecpg)*zr(igeom+2+ idecno)
                xyzgau(3) = xyzgau(3)+zr(ivf1+ia+idecpg)*zr(igeom+3+ idecno)
            end do
        endif
!
! - CALCUL DES ELEMENTS GEOMETRIQUES
! - CALCUL DE DFDI,F,EPS,R(EN AXI) ET POIDS
        call dfdmip(ndim, nno1, axi, zr(igeom), g,&
                    iw, vff1(1, g), idff1, r, w,&
                    dff1)
!
! - CALCUL DE LA MATRICE B EPS_ij=B_ijkl U_kl
        if (ndim .eq. 2) then
            do na = 1, nno1
                do ia = 1, ndim
                    def(1,na,ia)= fm(ia,1)*dff1(na,1)
                    def(2,na,ia)= fm(ia,2)*dff1(na,2)
                    def(3,na,ia)= 0.d0
                    def(4,na,ia)=(fm(ia,1)*dff1(na,2)+fm(ia,2)*dff1(na,1))/rac2
                end do
            end do
!
! - TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
            if (axi) then
                do na = 1, nno1
                    def(3,na,1) = fm(3,3)*vff1(na,g)/r
                end do
            endif
        else
            do na = 1, nno1
                do ia = 1, ndim
                    def(1,na,ia)= fm(ia,1)*dff1(na,1)
                    def(2,na,ia)= fm(ia,2)*dff1(na,2)
                    def(3,na,ia)= fm(ia,3)*dff1(na,3)
                    def(4,na,ia)=(fm(ia,1)*dff1(na,2)+fm(ia,2)*dff1(na,1))/rac2
                    def(5,na,ia)=(fm(ia,1)*dff1(na,3)+fm(ia,3)*dff1(na,1))/rac2
                    def(6,na,ia)=(fm(ia,2)*dff1(na,3)+fm(ia,3)*dff1(na,2))/rac2
                end do
            end do
        endif
!
! - CALCUL DE TRACE(B)
        do na = 1, nno1
            do ia = 1, ndim
                deftr(na,ia) = def(1,na,ia) + def(2,na,ia) + def(3,na, ia)
            end do
        end do
!
! - CALCUL DE LA MATRICE D'ELASTICITE BULLE
        call tanbul(option, ndim, g, mate, compor,&
                    .false._1, .true._1, alpha, dsidep, trepst)
!
! - CALCUL DE LA MATRICE DE CONDENSATION STATIQUE
        if (mini) then
            call calkbb(nno1, ndim, w, def, dsidep,&
                        kbb)
            call calkbp(nno2, ndim, w, dff1, kbp)
            call calkce(nno1, ndim, kbp, kbb, presm,&
                        presd, kce, rce)
        else
            call r8inir(nno2*nno2, 0.d0, kce, 1)
        endif
!
        if (ndim .eq. 3) then
            call pmat(6, idev/3.d0, dsidep, devd)
            call pmat(6, dsidep, idev/3.d0, ddev)
            call pmat(6, devd, idev/3.d0, dddev)
        else
            call pmat(4, idev2/3.d0, dsidep, devd)
            call pmat(4, dsidep, idev2/3.d0, ddev)
            call pmat(4, devd, idev2/3.d0, dddev)
        endif
!
! - CALCUL DE LA MATRICE DE RIGIDITE
! - TERME K:UX
        do na = 1, nno1
            do ia = 1, ndim
                vuiana = vu(ia,na)
                os = (vuiana-1)*vuiana/2
!
! - TERME K:UU      KUU(NDIM,NNO1,NDIM,NNO1)
                do nb = 1, nno1
                    do ib = 1, ndim
                        if (vu(ib,nb) .le. vuiana) then
                            kk = os+vu(ib,nb)
                            t1 = 0.d0
                            do ja = 1, 2*ndim
                                do jb = 1, 2*ndim
                                    t1 = t1 + def(ja,na,ia)*dddev(ja, jb)*def(jb,nb,ib)
                                end do
                            end do
                            matr(kk) = matr(kk) + w*t1
                        endif
                    end do
                end do
!
! - TERME K:UP      KUP(NDIM,NNO1,NNO2)
                do sb = 1, nno2
                    if (vp(sb) .lt. vuiana) then
                        kk = os + vp(sb)
                        t1 = deftr(na,ia)*vff2(sb,g)
                        matr(kk) = matr(kk) + w*t1
                    endif
                end do
            end do
        end do
!
! - TERME K:PX
        do sa = 1, nno2
            vpsa = vp(sa)
            os = (vpsa-1)*vpsa/2
!
! - TERME K:PU      KPU(NDIM,NNO2,NNO1)
            do nb = 1, nno1
                do ib = 1, ndim
                    if (vu(ib,nb) .lt. vpsa) then
                        kk = os + vu(ib,nb)
                        t1 = vff2(sa,g)*deftr(nb,ib)
                        matr(kk) = matr(kk) + w*t1
                    endif
                end do
            end do
!
! - TERME K:PP      KPP(NNO2,NNO2)
            do sb = 1, nno2
                if (vp(sb) .le. vpsa) then
                    kk = os + vp(sb)
                    t1 = - vff2(sa,g)*vff2(sb,g)*alpha
                    matr(kk) = matr(kk) + w*t1 - kce(sa,sb)
                endif
            end do
        end do
    end do
end subroutine
