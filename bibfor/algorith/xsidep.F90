subroutine xsidep(nnop, nfh, nfe, ddlc, ddlm,&
                  igeom, typmod, imate, compor, jpintt,&
                  cnset, heavt, lonch, basloc, idepl,&
                  lsn, lst, sig, jpmilt, nfiss,&
                  jfisno)
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
! person_in_charge: samuel.geniaut at edf.fr
! aslint: disable=W1306,W1504
    implicit none
!
!
!
!.......................................................................
!
!     BUT:  CALCUL DE L'OPTION SIEF_ELGA AVEC X-FEM
!.......................................................................
! IN  NNOP    : NOMBRE DE NOEUDS DE L'ELEMENT PARENT
! IN  NFH     : NOMBRE DE FONCTIONS HEAVYSIDE
! IN  NFE     : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT
! IN  DDLC    : NOMBRE DE DDL DE CONTACT (PAR NOEUD)
! IN  DDLM    : NOMBRE DE DDL PAR NOEUD MILIEU
! IN  IGEOM   : COORDONEES DES NOEUDS
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  IMATE   : MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT
!
! IN  JPINTT  : POINTEUR DE COORDONNÉES DES POINTS D'INTERSECTION
! IN  CNSET   : CONNECTIVITE DES SOUS-ELEMENTS
! IN  HEAVT   : VALEURS DE L'HEAVISIDE SUR LES SS-ELTS
! IN  LONCH   : LONGUEURS DES CHAMPS UTILISÉES
! IN  BASLOC  : BASE LOCALE AU FOND DE FISSURE
! IN  IDEPL   : DEPLACEMENT A PARTIR DE LA CONF DE REF
! IN  LSN     : VALEUR DE LA LEVEL SET NORMALE AUX NOEUDS PARENTS
! IN  LST     : VALEUR DE LA LEVEL SET TANGENTE AUX NOEUDS PARENTS
! IN  JPMILT  : POINTEUR DE COORDONNEES DES POINTS MILIEUX
! IN  NFISS   : NOMBRE DE FISSURES "VUES" PAR L'ÉLÉMENT
! IN  JFISNO  : POINTEUR DE CONNECTIVITÉ FISSURE/HEAVISIDE
!
! OUT SIG     : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
!..............................................................
!----------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/elref5.h"
#include "asterfort/iselli.h"
#include "asterfort/nbsigm.h"
#include "asterfort/tecach.h"
#include "asterfort/xside2.h"
#include "asterfort/xside3.h"
    integer :: nnop
    character(len=8) :: elrefp, elrese(6), fami(6), typmod(*)
    character(len=16) :: compor(4)
    real(kind=8) :: he(nfiss), sig(*), lsn(nnop), lst(nnop), basloc(*)
    real(kind=8) :: coorse(81)
    integer :: nse, npg, imate, ddlc, ddlm, ndim, nfh
    integer :: j, ise, in, ino, cnset(4*32), heavt(*), lonch(10)
    integer :: ibid, idecpg, nbsig, ig, ifiss, idebs, jpmilt, nfe, idepl
    integer :: nfiss, jfisno, jpintt, igeom
    integer :: irese, nno, fisno(nnop, nfiss), jtab(2), ncomp, iret
!
    data    elrese /'SE2','TR3','TE4','SE3','TR6','TE4'/
    data    fami   /'BID','XINT','XINT','BID','XINT','XINT'/
!
!
!     ATTENTION, DEPL ET VECTU SONT ICI DIMENSIONNÉS DE TELLE SORTE
!     QU'ILS NE PRENNENT PAS EN COMPTE LES DDL SUR LES NOEUDS MILIEU
!
    call elref1(elrefp)
!
!     NOMBRE DE COMPOSANTES DE PHEAVTO (DANS LE CATALOGUE)
    call tecach('OOO', 'PHEAVTO', 'L', 2, jtab,&
                iret)
    ncomp = jtab(2)
!
!     ELEMENT DE REFERENCE PARENT : RECUP DE NDIM
    call elref4(' ', 'RIGI', ndim, ibid, ibid,&
                ibid, ibid, ibid, ibid, ibid)
!
!     SOUS-ELEMENT DE REFERENCE : RECUP DE NPG
    if (.not.iselli(elrefp) .and. ndim .le. 2) then
        irese=3
    else
        irese=0
    endif
    call elref5(elrese(ndim+irese), fami(ndim+irese), ibid, nno, ibid,&
                npg, ibid, ibid, ibid, ibid,&
                ibid, ibid)
!
!     NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
    nbsig = nbsigm()
!
!     RECUPERATION DE LA CONNECTIVITÉ FISSURE - DDL HEAVISIDES
!     ATTENTION !!! FISNO PEUT ETRE SURDIMENTIONNÉ
    if (nfiss .eq. 1) then
        do 30 ino = 1, nnop
            fisno(ino,1) = 1
30      continue
    else
        do 10 ig = 1, nfh
!    ON REMPLIT JUSQU'A NFH <= NFISS
            do 20 ino = 1, nnop
                fisno(ino,ig) = zi(jfisno-1+(ino-1)*nfh+ig)
20          continue
10      continue
    endif
!
!     RÉCUPÉRATION DE LA SUBDIVISION DE L'ÉLÉMENT EN NSE SOUS ELEMENT
    nse=lonch(1)
!
!       BOUCLE D'INTEGRATION SUR LES NSE SOUS-ELEMENTS
    do 110 ise = 1, nse
!
!       BOUCLE SUR LES 4/3 SOMMETS DU SOUS-TETRA/TRIA
        do 111 in = 1, nno
            ino=cnset(nno*(ise-1)+in)
            do 112 j = 1, ndim
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
112          continue
111      continue
!
!       FONCTION HEAVYSIDE CSTE POUR CHAQUE FISSURE SUR LE SS-ELT
        do 113 ifiss = 1, nfiss
            he(ifiss) = heavt(ncomp*(ifiss-1)+ise)
113      continue
!
!       DEBUT DE LA ZONE MEMOIRE DE SIG CORRESPONDANTE
        idecpg = npg * (ise-1)
        idebs = nbsig * idecpg
!
        if (ndim .eq. 3) then
!
            ASSERT(nbsig.eq.6)
!
            call xside3(elrefp, ndim, coorse, elrese(ndim+irese), igeom,&
                        he, nfh, ddlc, ddlm, nfe,&
                        basloc, nnop, npg, idecpg, imate,&
                        compor, idepl, lsn, lst, nfiss,&
                        fisno, sig(idebs+1))
        else if (ndim.eq.2) then
!
            ASSERT(nbsig.eq.4)
!
            call xside2(elrefp, ndim, coorse, elrese(ndim+irese), igeom,&
                        he, nfh, ddlc, ddlm, nfe,&
                        basloc, nnop, npg, idecpg, typmod,&
                        imate, compor, idepl, lsn, lst,&
                        nfiss, fisno, sig(idebs+1))
!
        endif
!
110  end do
!
end subroutine
