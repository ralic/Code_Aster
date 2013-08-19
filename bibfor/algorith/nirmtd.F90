subroutine nirmtd(ndim, nno1, nno2, nno3, npg,iw, vff2, vff3, ivf1, idff1,&
                  vu, vg, vp, igeom, mate,matr)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "jeveux.h"
!
#include "asterfort/bmatmc.h"
#include "asterfort/dmatmc.h"
#include "asterfort/nbsigm.h"
#include "asterfort/ortrep.h"
#include "asterfort/pmat.h"
#include "blas/dscal.h"
    integer :: ndim, nno1, nno2, nno3, npg, iw, idff1
    integer :: mate
    integer :: vu(3, 27), vg(27), vp(27)
    integer :: ivf1, igeom
    real(kind=8) :: vff2(nno2, npg), vff3(nno3, npg)
    real(kind=8) :: matr(*)
!-----------------------------------------------------------------------
!          CALCUL DE LA RIGIDITE MECANIQUE POUR LES ELEMENTS
!          INCOMPRESSIBLES POUR LES GRANDES DEFORMATIONS
!          3D/D_PLAN/AXIS
!          ROUTINE APPELEE PAR TE0592
!-----------------------------------------------------------------------
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  NNO1    : NOMBRE DE NOEUDS DE L'ELEMENT LIES AUX DEPLACEMENTS
! IN  NNO2    : NOMBRE DE NOEUDS DE L'ELEMENT LIES AU GONFLEMENT
! IN  NNO3    : NOMBRE DE NOEUDS DE L'ELEMENT LIES A LA PRESSION
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  IW      : POIDS DES POINTS DE GAUSS
! IN  VFF2    : VALEUR  DES FONCTIONS DE FORME LIES AU GONFLEMENT
! IN  VFF3    : VALEUR  DES FONCTIONS DE FORME LIES A LA PRESSION
! IN  IDFF1   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  VU      : TABLEAU DES INDICES DES DDL DE DEPLACEMENTS
! IN  VG      : TABLEAU DES INDICES DES DDL DE GONFLEMENT
! IN  VP      : TABLEAU DES INDICES DES DDL DE PRESSION
! IN  IGEOM   : POINTEUR SUR LES COORDONEES DES NOEUDS
! IN  MATE    : MATERIAU CODE
! OUT MATR    : MATRICE DE RIGIDITE
!-----------------------------------------------------------------------
!
    integer :: g
    integer :: ia, na, ra, sa, ib, nb, rb, sb, ja, jb
    integer :: os, kk
    integer :: vuiana, vgra, vpsa
    integer :: nbsig, idim
    integer :: idecpg, idecno
    real(kind=8) :: w
    real(kind=8) :: dsidep(2*ndim, 2*ndim)
    real(kind=8) :: b(2*ndim, 81), def(2*ndim, nno1, ndim), deftr(nno1, ndim)
    real(kind=8) :: ddev(2*ndim, 2*ndim), devd(2*ndim, 2*ndim)
    real(kind=8) :: dddev(2*ndim, 2*ndim)
    real(kind=8) :: iddid, devdi(2*ndim), iddev(2*ndim)
    real(kind=8) :: xyzgau(3), bary(3), repere(7)
    real(kind=8) :: t1, rac2
    real(kind=8) :: idev(6, 6), idev2(4, 4), kr(6), kd(6)
!
    data         kr   / 1.d0, 1.d0, 1.d0, 0.d0, 0.d0, 0.d0/
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
! - NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
    rac2 = sqrt(2.d0)
    nbsig = nbsigm()
    do ia = 1, 3
        kd(ia) = 1.d0
        kd(ia+1) = 2.d0/rac2
    end do
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
        call bmatmc(g, nbsig, zr(igeom), iw, ivf1, idff1, nno1, 0.d0, w, b)
!
        do ia = 1, 2*ndim
            do ja = 1, nno1
                do na = 1, ndim
                    def(ia,ja,na) = b(ia,(ja-1)*ndim+na)*kd(ia)
                end do
            end do
        end do
!
! - CALCUL DE TRACE(B)
        do na = 1, nno1
            do ia = 1, ndim
                deftr(na,ia) = def(1,na,ia) + def(2,na,ia) + def(3,na, ia)
            end do
        end do
!
! - CALCUL DE LA MATRICE DE HOOKE (LE MATERIAU POUVANT
! - ETRE ISOTROPE, ISOTROPE-TRANSVERSE OU ORTHOTROPE)
        call dmatmc('RIGI', '  ', mate, 0.d0, '+', g, 1, repere, xyzgau, nbsig, dsidep)
!
        call dscal(2*ndim-3, rac2, dsidep(4,1), 1)
        call dscal(2*ndim-3, rac2, dsidep(4,2), 1)
        call dscal(2*ndim-3, rac2, dsidep(4,3), 1)
        call dscal(3, rac2, dsidep(1,4), 1)
        call dscal(2*ndim-3, 2.d0, dsidep(4,4), 1)
        if (ndim .eq. 3) then
            call dscal(3, rac2, dsidep(1,5), 1)
            call dscal(3, rac2, dsidep(1,6), 1)
            call dscal(3, 2.d0, dsidep(4,5), 1)
            call dscal(3, 2.d0, dsidep(4,6), 1)
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
! - CALCUL DE D^DEV:ID ET ID:D^DEV ET ID:D:ID/9.D0
        iddid = 0.d0
        do ia = 1, 2*ndim
            devdi(ia) = devd(ia,1)+devd(ia,2)+devd(ia,3)
            iddev(ia) = ddev(1,ia)+ddev(2,ia)+ddev(3,ia)
            do ja = 1, 3
                iddid = iddid+kr(ia)*dsidep(ia,ja)
            end do
        end do
        iddid = iddid/9.d0
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
! - TERME K:UG      KUG(NDIM,NNO1,NNO2)
                t1 = 0.d0
                do ja = 1, 2*ndim
                    t1 = t1 + def(ja,na,ia)*devdi(ja)
                end do
                t1 = t1/3.d0
!
                do rb = 1, nno2
                    if (vg(rb) .lt. vuiana) then
                        kk = os + vg(rb)
                        matr(kk) = matr(kk) + w*t1*vff2(rb,g)
                    endif
                end do
!
! - TERME K:UP      KUP(NDIM,NNO1,NNO3)
                do sb = 1, nno3
                    if (vp(sb) .lt. vuiana) then
                        kk = os + vp(sb)
                        t1 = deftr(na,ia)*vff3(sb,g)
                        matr(kk) = matr(kk) + w*t1
                    endif
                end do
            end do
        end do
!
! - TERME K:GX
        do ra = 1, nno2
            vgra = vg(ra)
            os = (vgra-1)*vgra/2
!
! - TERME K:GU      KGU(NDIM,NNO2,NNO1)
            do nb = 1, nno1
                do ib = 1, ndim
                    if (vu(ib,nb) .lt. vgra) then
                        kk = os + vu(ib,nb)
                        t1 = 0.d0
                        do jb = 1, 2*ndim
                            t1 = t1 + iddev(jb)*def(jb,nb,ib)
                        end do
                        matr(kk) = matr(kk) + w*t1*vff2(ra,g)/3.d0
                    endif
                end do
            end do
!
! - TERME K:GG      KGG(NNO2,NNO2)
            do rb = 1, nno2
                if (vg(rb) .le. vgra) then
                    kk = os + vg(rb)
                    t1 = vff2(ra,g)*iddid*vff2(rb,g)
                    matr(kk) = matr(kk) + w*t1
                endif
            end do
!
! - TERME K:GP      KGP(NNO2,NNO3)
            do sb = 1, nno3
                if (vp(sb) .lt. vgra) then
                    kk = os + vp(sb)
                    t1 = - vff2(ra,g)*vff3(sb,g)
                    matr(kk) = matr(kk) + w*t1
                endif
            end do
        end do
!
! - TERME K:PX
        do sa = 1, nno3
            vpsa = vp(sa)
            os = (vpsa-1)*vpsa/2
!
! - TERME K:PU      KPU(NDIM,NNO3,NNO1)
            do nb = 1, nno1
                do ib = 1, ndim
                    if (vu(ib,nb) .lt. vpsa) then
                        kk = os + vu(ib,nb)
                        t1 = vff3(sa,g)*deftr(nb,ib)
                        matr(kk) = matr(kk) + w*t1
                    endif
                end do
            end do
!
! - TERME K:PG      KPG(NNO3,NNO2)
            do rb = 1, nno2
                if (vg(rb) .lt. vpsa) then
                    kk = os + vg(rb)
                    t1 = - vff3(sa,g)*vff2(rb,g)
                    matr(kk) = matr(kk) + w*t1
                endif
            end do
!
! - TERME K:PP = 0.D0      KPP(NNO3,NNO3)
        end do
    end do
end subroutine
