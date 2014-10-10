subroutine xpeshm(nno, nnop, nnops, ndim, nddls,&
                  nddlm, npg, igeom, jpintt, jpmilt, jlsn,&
                  ivf, ipoids, idfde, ivectu, ipesa,&
                  heavt, lonch, cnset, rho, axi,&
                  yaenrm)
!
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
!
! IN  ELREFP  : ÉLÉMENT DE RÉFÉRENCE PARENT
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  COORSE  : COORDONNÉES DES SOMMETS DU SOUS-ÉLÉMENT
! IN  IGEOM   : COORDONNÉES DES NOEUDS DE L'ÉLÉMENT PARENT
! IN  HE      : VALEUR DE LA FONCTION HEAVISIDE SUR LE SOUS-ÉLT
! IN  NNOP    : NOMBRE DE NOEUDS DE L'ELEMENT PARENT
! IN  NPG     : NOMBRE DE POINTS DE GAUSS DU SOUS-ÉLÉMENT
! IN  IVECTU  : INDICE DU SECONDE MEMBRE
!
    implicit none
#include "asterf_types.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elref1.h"
#include "asterfort/hmdeca.h"
#include "asterfort/indent.h"
#include "asterfort/reeref.h"
#include "asterfort/vecini.h"
#include "asterfort/xcalf_he.h"
#include "jeveux.h"
    aster_logical :: axi
    integer :: nse, ise, in, ino, nno, j, ndim
    integer :: nnop, nnops, n, nddls, nddlm, ipi, npg
    integer :: igeom, jpintt, jpmilt, ivf, ipoids, idfde, jlsn
    integer :: ivectu, yaenrm
    integer :: ipesa, dec1(nnop), dec2(nnop), icla, ienr
    integer :: heavt(36), lonch(10), cnset(4*32)
    real(kind=8) :: xg(ndim), xe(ndim), coorse(81), dbid(nnop, ndim)
    real(kind=8) :: ff(nnop), rbid1(4), rbid2(4), rbid3(4), poids, rho, he, rx
    character(len=8) :: elrefp
!
    call elref1(elrefp)
!
!     RÉCUPÉRATION DE LA SUBDIVISION DE L'ÉLÉMENT EN NSE SOUS ELEMEN
    nse=lonch(1)
!
!     BOUCLE D'INTEGRATION SUR LES NSE SOUS-ELEMENTS
    do ise = 1, nse
!
!     BOUCLE SUR LES 4/3 SOMMETS DU SOUS-TETRA/TRIA
        do in = 1, nno
            ino=cnset(nno*(ise-1)+in)
            do j = 1, ndim
                if (ino .lt. 1000) then
                    coorse(ndim*(in-1)+j)=zr(igeom-1+ndim*(ino-1)+j)
                else if (ino.gt.1000 .and. ino.lt.2000) then
                    coorse(ndim*(in-1)+j)=zr(jpintt-1+ndim*(ino-1000-&
                    1)+j)
                else if (ino.gt.2000 .and. ino.lt.3000) then
                    coorse(ndim*(in-1)+j)=zr(jpmilt-1+ndim*(ino-2000-&
                    1)+j)
                else if (ino.gt.3000) then
                    coorse(ndim*(in-1)+j)=zr(jpmilt-1+ndim*(ino-3000-&
                    1)+j)
                endif
            end do 
        end do
!
!     DEFINITION DE LA FONCTION HEAVISIDE POUR CHAQUE SS-ELT
        he=1.d0*heavt(ise)
!
        do 70 n = 1, nnop
            call indent(n, nddls, nddlm, nnops, dec1(n))
            call hmdeca(n, nddls, nddlm, nnops, dec2(n))
 70     continue
! =====================================================================
! --- BOUCLE SUR LES POINTS D'INTEGRATION -----------------------------
! =====================================================================
        do 10 ipi = 1, npg
!
!     COORDONNÉES DU PT DE GAUSS DANS LE REPÈRE RÉEL : XG
            call vecini(ndim, 0.d0, xg)
            do j = 1, ndim
                do in = 1, nno
                    xg(j)=xg(j)+zr(ivf-1+nno*(ipi-1)+in)* coorse(ndim*&
                    (in-1)+j)
                end do
            end do
!
!     XG -> XE (DANS LE REPERE DE l'ELREFP) ET VALEURS DES FF EN XE
            call vecini(ndim, 0.d0, xe)
!
!     CALCUL DES FF ET DES DERIVEES DFDI POUR L'ELEMENT PARENTS
!     QUDRATIQUE (MECANIQUE)
            call reeref(elrefp, nnop, zr(igeom), xg, ndim,&
                        xe, ff, dbid)
!
!     CALCUL DU JACOBIEN DE LA TRANSFORMATION SS-ELT -> SS-ELT REF
            if (ndim .eq. 2) then
                call dfdm2d(nno, ipi, ipoids, idfde, coorse,&
                            poids, rbid1, rbid2)
            else if (ndim .eq. 3) then
                call dfdm3d(nno, ipi, ipoids, idfde, coorse,&
                            poids, rbid1, rbid2, rbid3)
            endif
!
            poids = poids*rho*zr(ipesa)
            if (axi) then
                rx = 0.d0
                do 80 ino = 1, nnop
                    rx = rx + zr(igeom+2*ino-2)*ff(ino)
 80             continue
                poids = poids*rx
!     TERMES CLASSIQUES
                do 90 ino = 1, nnop
                    icla=dec1(ino)
                    ienr=dec2(ino)
!
                    zr(ivectu+icla+1)=zr(ivectu+icla+1) +poids*zr(&
                    ipesa+2)*ff(ino)
!     TERMES HEAVISIDE
                    if (yaenrm .eq. 1) then
                        zr(ivectu+ienr+ndim+1)=zr(ivectu+ienr+ndim+1)&
                        +xcalf_he(he,zr(jlsn-1+ino))*poids*zr(ipesa+2)*ff(ino)
                    endif
 90             continue
            else
!     TERMES CLASSIQUES
                do 100 ino = 1, nnop
                    icla=dec1(ino)
                    ienr=dec2(ino)
!
                    zr(ivectu+icla)=zr(ivectu+icla) +poids*zr(ipesa+1)&
                    *ff(ino)
!
                    zr(ivectu+icla+1)=zr(ivectu+icla+1) +poids*zr(&
                    ipesa+2)*ff(ino)
!
                    if (ndim .eq. 3) then
                        zr(ivectu+icla+2)=zr(ivectu+icla+2) +poids*zr(&
                                    ipesa+3)*ff(ino)
                    endif
!     TERMES HEAVISIDE
                    if (yaenrm .eq. 1) then
                        zr(ivectu+ienr+ndim)=zr(ivectu+ienr+ndim)&
                        +xcalf_he(he,zr(jlsn-1+ino))*poids*zr(ipesa+1)*ff(ino)
!
                        zr(ivectu+ienr+ndim+1)=zr(ivectu+ienr+ndim+1)&
                        +xcalf_he(he,zr(jlsn-1+ino))*poids*zr(ipesa+2)*ff(ino)
                        if (ndim .eq. 3) then
                            zr(ivectu+ienr+ndim+2)=zr(ivectu+ienr+ndim+2)&
                                      +xcalf_he(he,zr(jlsn-1+ino))*poids*zr(ipesa+3)*ff(ino)  
                        endif
                    endif
100             continue
            endif
 10     continue
    end do
end subroutine
