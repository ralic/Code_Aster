subroutine xfovol(elrefp, ndim, coorse, igeom, he,&
                  ddlh, ddlc, nfe, nnop, jlsn,&
                  jlst, iforc, itemps, ivectu, fonc,&
                  fono)
!
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
!
! aslint: disable=W1306
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/fointe.h"
#include "asterfort/iselli.h"
#include "asterfort/lteatt.h"
#include "asterfort/reeref.h"
#include "asterfort/vecini.h"
#include "asterfort/xdeffe.h"
    character(len=8) :: elrefp
    real(kind=8) :: coorse(*)
    integer :: igeom, ndim, ddlh, ddlc, nfe, nnop
    integer :: iforc, itemps, ivectu, jlsn, jlst
    real(kind=8) :: he
    aster_logical :: fonc, fono
!-----------------------------------------------------------------------
! FONCTION REALISEE : CALCUL DU SECOND MEMBRE AUX PG DU SOUS EL COURANT
!                     DANS LE CAS D'UNE FORCE VOLUMIQUE IMPOSEE SUR LES
!                     ELEMENTS X-FEM 2D ET 3D
!
!                    OPTIONS  :  CHAR_MECA_FR3D3D
!                                CHAR_MECA_FR2D2D
!                                CHAR_MECA_FF3D3D
!                                CHAR_MECA_FF2D2D
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
! IN  JLSN    : INDICE DE LA LEVEL SET NORMALE AUX NOEUDS PARENTS
! IN  JLST    : INDICE DE LA LEVEL SET TANGENTE AUX NOEUDS PARENTS
! IN IFORC    : INDICE DE LA FORCE VOLUMIQUE
! IN ITEMPS   : INDICE DE L'INSTANT
! IN FONC     : .TRUE. SI LA FORCE EST FONCTION DE L'ESPACE OU DU TEMPS
! IN FONO     : .TRUE. SI LA FORCE EST FOURNIE AU NOEUD (.FALSE. AU PG)
! IN IVECTU   : INDICE DU SECONDE MEMBRE
!
!
!
    integer :: i, ino, ig, ier, j, n, mxstac
    integer :: ndimb, nno, nnos, nnops, npgbis, pos, irese
    integer :: jcoopg, ipoids, ivf, idfde, jdfd2, jgano, kpg
    real(kind=8) :: xe(ndim), xg(ndim), ff(nnop), lsng, lstg, rg, tg
    real(kind=8) :: forvol(ndim)
    real(kind=8) :: valpar(ndim+1), fe(4), poids
    character(len=8) :: elrese(6), fami(6), nompar(ndim+1)
    aster_logical :: grdepl, axi
    parameter      (mxstac=1000)
!
    data    elrese /'SE2','TR3','TE4','SE3','TR6','T10'/
    data    fami   /'BID','XINT','XINT','BID','XINT','XINT'/
!
!
!-----------------------------------------------------------------------
!     VERIF QUE LES TABLEAUX LOCAUX DYNAMIQUES NE SONT PAS TROP GRANDS
!     (VOIR CRS 1404)
    ASSERT(nnop.le.mxstac)
    ASSERT(ndim.le.mxstac)
!
    grdepl=.false.
!
    call elrefe_info(fami='RIGI', nnos=nnops)
!
    axi = lteatt('AXIS','OUI')
!
!     SOUS-ELEMENT DE REFERENCE
    if (.not.iselli(elrefp)) then
        irese=3
    else
        irese=0
    endif
    call elrefe_info(elrefe=elrese(ndim+irese), fami=fami(ndim+irese), ndim=ndimb, nno=nno,&
                     nnos=nnos, npg=npgbis, jpoids=ipoids, jcoopg=jcoopg, jvf=ivf,&
                     jdfde=idfde, jdfd2=jdfd2, jgano=jgano)
    ASSERT(ndim.eq.ndimb)
!
!     ------------------------------------------------------------------
!     BOUCLE SUR LES POINTS DE GAUSS DU SOUS-ELEMENT
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
        call reeref(elrefp, nnop, zr(igeom), xg, ndim,&
                    xe, ff)
!
!       POUR CALCULER LE JACOBIEN DE LA TRANSFO SS-ELT -> SS-ELT REF
!       ON ENVOIE DFDM3D/DFDM2D AVEC LES COORD DU SS-ELT
        if (ndim .eq. 3) call dfdm3d(nno, kpg, ipoids, idfde, coorse,&
                                     poids)
        if (ndim .eq. 2) call dfdm2d(nno, kpg, ipoids, idfde, coorse,&
                                     poids)
!
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
            tg = he * abs(atan2(lsng,lstg))
!
!         FONCTIONS D'ENRICHISSEMENT
            call xdeffe(rg, tg, fe)
!
        endif
!
!       CALCUL DE LA FORCE VOLUMIQUE AU PG COURANT
!       ------------------------------------------
!
        call vecini(ndim, 0.d0, forvol)
!
        if (fonc) then
!
!         FORCE AU PG COURANT A PARTIR DE LA FORCE FONCTION PAR ELEMENT
            do i = 1, ndim
                valpar(i) = xg(i)
            end do
            valpar(ndim+1) = zr(itemps)
            nompar(1)='X'
            nompar(2)='Y'
            if (ndim .eq. 3) then
                nompar(3)='Z'
                nompar(4)='INST'
                call fointe('FM', zk8(iforc ), 4, nompar, valpar,&
                            forvol(1), ier)
                call fointe('FM', zk8(iforc+1), 4, nompar, valpar,&
                            forvol( 2), ier)
                call fointe('FM', zk8(iforc+2), 4, nompar, valpar,&
                            forvol( 3), ier)
            else if (ndim.eq.2) then
                nompar(3)='INST'
                call fointe('FM', zk8(iforc ), 3, nompar, valpar,&
                            forvol(1), ier)
                call fointe('FM', zk8(iforc+1), 3, nompar, valpar,&
                            forvol( 2), ier)
            endif
!
        else
!
            if (fono) then
!           FORCE AU PG COURANT A PARTIR DE LA FORCE AUX NOEUDS
                do ino = 1, nnop
                    do j = 1, ndim
                        forvol(j)=forvol(j)+zr(iforc-1+ndim*(ino-1)+j)&
                        *ff(ino)
                    end do
                end do
            else
!           FORCE FOURNIE AU PG
                do j = 1, ndim
                    forvol(j) = zr(iforc+j-1)
                end do
            endif
!
        endif
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
!
            end do
!
!         TERME HEAVISIDE
            do j = 1, ddlh
                pos=pos+1
                zr(ivectu-1+pos) = zr(ivectu-1+pos) + he*forvol(j)* poids*ff(ino)
!
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
