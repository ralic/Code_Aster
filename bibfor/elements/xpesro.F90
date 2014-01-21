subroutine xpesro(elrefp, ndim, coorse, igeom, jheavt,&
                  jfisno, nfh, ddlc, nfe, nfiss,&
                  ise, nnop, jlsn, jlst, ivectu,&
                  fno)
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
! aslint: disable=W1306
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elref4.h"
#include "asterfort/elref5.h"
#include "asterfort/iselli.h"
#include "asterfort/lteatt.h"
#include "asterfort/reeref.h"
#include "asterfort/tecach.h"
#include "asterfort/vecini.h"
#include "asterfort/xdeffe.h"
    character(len=8) :: elrefp
    real(kind=8) :: coorse(*)
    integer :: igeom, ndim, ddlc, nfe, nnop
    integer :: ivectu, jlsn, jlst
    integer :: jheavt, jfisno, nfh, nfiss, ise
    real(kind=8) :: fno(ndim*nnop)
!-----------------------------------------------------------------------
! FONCTION REALISEE : CALCUL DU SECOND MEMBRE AUX PG DU SOUS EL COURANT
!                     DANS LE CAS D'UNE FORCE VOLUMIQUE IMPOSEE SUR LES
!                     ELEMENTS X-FEM 2D ET 3D
!
!                          OPTIONS  :  CHAR_MECA_PESA_R
!                                      CHAR_MECA_ROTA_R
!
! IN  ELREFP  : ÉLÉMENT DE RÉFÉRENCE PARENT
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  COORSE  : COORDONNÉES DES SOMMETS DU SOUS-ÉLÉMENT
! IN  IGEOM   : COORDONNÉES DES NOEUDS DE L'ÉLÉMENT PARENT
! IN  HE      : VALEUR DE LA FONCTION HEAVISIDE SUR LE SOUS-ÉLT
! IN  DDLH    : NOMBRE DE DDL HEAVYSIDE (PAR NOEUD)
! IN  DDLC    : NOMBRE DE DDL DE CONTACT (PAR NOEUD)
! IN  NFE     : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT
! IN  NNOP    : NOMBRE DE NOEUDS DE L'ELEMENT PARENT
! IN  NPG     : NOMBRE DE POINTS DE GAUSS DU SOUS-ÉLÉMENT
! IN  JLSN    : INDICE DE LA LEVEL SET NORMALE AUX NOEUDS PARENTS
! IN  JLST    : INDICE DE LA LEVEL SET TANGENTE AUX NOEUDS PARENTS
! IN  IDECPG  : POSITION DANS LA FAMILLE 'XFEM' DU 1ER POINT DE GAUSS
!               DU SOUS ELEMENT COURRANT (EN FAIT 1ER POINT : IDECPG+1)
! IN ITEMPS   : INDICE DE L'INSTANT
! IN FNO      : FORCES VOLUMIQUES AUX NOEUDS DE L'ELEMENT PARENT
! IN IVECTU   : INDICE DU SECONDE MEMBRE
!
!
    integer :: i, ino, ig, j, n, ibid, jtab(2), iret, ncomp, ifiss
    integer :: ndimb, nno, nnos, nnops, npgbis, pos
    integer :: jcoopg, ipoids, ivf, idfde, jdfd2, jgano, kpg
    real(kind=8) :: xe(ndim), xg(ndim), ff(nnop), lsng, lstg, rg, tg
    real(kind=8) :: forvol(ndim)
    real(kind=8) :: fe(4), poids, r
    character(len=8) :: elrese(6), fami(6)
    logical :: grdepl, axi
    integer :: irese
!
    data    elrese /'SE2','TR3','TE4','SE3','TR6','T10'/
    data    fami   /'BID','XINT','XINT','BID','XINT','XINT'/
!
!-----------------------------------------------------------------------
    grdepl=.false.
!
    axi = lteatt('AXIS','OUI')
!
    call elref4(' ', 'RIGI', ibid, ibid, nnops,&
                ibid, ibid, ibid, ibid, ibid)
!
!     SOUS-ELEMENT DE REFERENCE
    if (.not.iselli(elrefp)) then
        irese=3
    else
        irese=0
    endif
    call elref5(elrese(ndim+irese), fami(ndim+irese), ndimb, nno, nnos,&
                npgbis, ipoids, jcoopg, ivf, idfde,&
                jdfd2, jgano)
    ASSERT(ndim.eq.ndimb)
!     NOMBRE DE COMPOSANTES DE PHEAVTO (DANS LE CATALOGUE)
    call tecach('OOO', 'PHEAVTO', 'L', iret, nval=2,&
                itab=jtab)
    ncomp = jtab(2)
    ifiss = 1
!
!     ------------------------------------------------------------------
!     BOUCLE SUR LES POINTS DE GAUSS DU SOUS-ELEMENT COURANT
!     ------------------------------------------------------------------
!
    do kpg = 1, npgbis
!
!       COORDONNÉES DU PT DE GAUSS DANS LA CONFIG RÉELLE DU SE : XG
        call vecini(ndim, 0.d0, xg)
        do i = 1, ndim
            do n = 1, nno
                xg(i) = xg(i) + zr(ivf-1+nno*(kpg-1)+n) * coorse(ndim* (n-1)+i)
            end do
        end do
!
!       CALCUL DES FF DE L'ELEMENT DE REFERENCE PARENT AU PG COURANT
        call reeref(elrefp, nnop, zr(igeom), xg, ndim, xe, ff)
!
!       POUR CALCULER LE JACOBIEN DE LA TRANSFO SS-ELT -> SS-ELT REF
!       ON ENVOIE DFDM3D/DFDM2D AVEC LES COORD DU SS-ELT
        if (ndim .eq. 3) call dfdm3d(nno, kpg, ipoids, idfde, coorse,&
                                     poids)
        if (ndim .eq. 2) call dfdm2d(nno, kpg, ipoids, idfde, coorse,&
                                     poids)
!
!
! -     CALCUL DE LA DISTANCE A L'AXE (AXISYMETRIQUE)
!       ET DU DEPL. RADIAL
        if (axi) then
            r = 0.d0
            do ino = 1, nnop
                r = r + ff(ino)*zr(igeom-1+2*(ino-1)+1)
            end do
!
!
!
            ASSERT(r.gt.0d0)
        endif
!
        if (axi) then
            poids= poids * r
        endif
!
!       CALCUL DES FONCTIONS D'ENRICHISSEMENT
!       -------------------------------------
!
        if (nfe .gt. 0) then
!         LEVEL SETS AU POINT DE GAUSS
            lsng = 0.d0
            lstg = 0.d0
            do ino = 1, nnop
                lsng = lsng + zr(jlsn-1+ino) * ff(ino)
                lstg = lstg + zr(jlst-1+ino) * ff(ino)
            end do
!
!         COORDONNÉES POLAIRES DU POINT
            rg=sqrt(lsng**2+lstg**2)
            tg = zi(jheavt-1+ise) * abs(atan2(lsng,lstg))
!
!         FONCTIONS D'ENRICHISSEMENT DANS LA BASE POLAIRE -> FE
            call xdeffe(rg, tg, fe)
!
        endif
!
!
!       CALCUL DE LA FORCE VOLUMIQUE AU PG COURANT
!       ------------------------------------------
!
        call vecini(ndim, 0.d0, forvol)
        do ino = 1, nnop
            do j = 1, ndim
                forvol(j)=forvol(j)+fno(ndim*(ino-1)+j)*ff(ino)
            end do
        end do
!
!
!       CALCUL EFFECTIF DU SECOND MEMBRE
!       --------------------------------
!
        pos=0
        do ino = 1, nnop
!
!         TERME CLASSIQUE
            do j = 1, ndim
                pos=pos+1
                zr(ivectu-1+pos) = zr(ivectu-1+pos) + forvol(j)*poids* ff(ino)
            end do
!
!         TERME HEAVISIDE
            do ig = 1, nfh
                if (nfiss .gt. 1) ifiss = zi(jfisno-1+(ino-1)*nfh+ig)
                do j = 1, ndim
                    pos=pos+1
                    zr(ivectu-1+pos) = zr(ivectu-1+pos) + zi(jheavt-1+ (ifiss-1)*ncomp+ise)*forvo&
                                       &l(j)*poids*ff(ino)
                end do
            end do
!
!         TERME SINGULIER
            do ig = 1, nfe
                do j = 1, ndim
                    pos=pos+1
                    zr(ivectu-1+pos) = zr(ivectu-1+pos) + fe(ig)* forvol(j)*poids*ff(ino)
                end do
            end do
!
!         ON SAUTE LES POSITIONS DES LAG DE CONTACT FROTTEMENT
            if (.not.iselli(elrefp)) then
                if (ino .le. nnops) pos = pos + ddlc
            else
                pos = pos + ddlc
            endif
!
!
        end do
!
!
    end do
!
!     ------------------------------------------------------------------
!     FIN BOUCLE SUR LES POINTS DE GAUSS DU SOUS-ELEMENT
!     ------------------------------------------------------------------
!
end subroutine
