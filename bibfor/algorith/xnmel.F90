subroutine xnmel(poum, nnop, nfh, nfe, ddlc,&
                 ddlm, igeom, typmod, option, imate,&
                 compor, lgpg, crit, jpintt, cnset,&
                 heavt, lonch, basloc, idepl, lsn,&
                 lst, sig, vi, matuu, ivectu,&
                 codret, jpmilt, nfiss, jheavn)
!
! aslint: disable=W1306,W1504
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/iselli.h"
#include "asterfort/nbsigm.h"
#include "asterfort/tecach.h"
#include "asterfort/xxnmel.h"
    integer :: nnop, imate, lgpg, codret, igeom, nfiss, jheavn
    integer :: cnset(4*32), heavt(*), lonch(10), ndim
    integer :: nfh, nfe, ddlc, ddlm
    integer :: ivectu, idepl, jpintt, jpmilt
    character(len=*) :: poum
    character(len=8) :: typmod(*)
    character(len=16) :: option, compor(*)
    real(kind=8) :: crit(*), vi(*), crit2(1), vi2(1), sig2(1)
    real(kind=8) :: lsn(nnop)
    real(kind=8) :: lst(nnop), matuu(*), sig(*), basloc(*)
!
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
! person_in_charge: samuel.geniaut at edf.fr
!
!
!
!
!
!.......................................................................
!
!     BUT:  PRÉLIMINAIRES AU CALCUL DES OPTIONS RIGI_MECA_TANG,
!           RAPH_MECA ET FULL_MECA  EN HYPER-ELASTICITE AVEC X-FEM
!.......................................................................
! IN  NNOP    : NOMBRE DE NOEUDS DE L'ELEMENT PARENT
! IN  IVF     : VALEUR  DES FONCTIONS DE FORME
! IN  NFH     : NOMBRE DE FONCTIONS HEAVYSIDE
! IN  NFE     : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT
! IN  DDLC    : NOMBRE DE DDL DE CONTACT (PAR NOEUD)
! IN  DDLM    : NOMBRE DE DDL PAR NOEUD MILIEU
! IN  IGEOM   : COORDONEES DES NOEUDS
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  OPTION  : OPTION DE CALCUL
! IN  NOMTE   : NOM DU TE
! IN  IMATE   : MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT
! IN  LGPG  : "LONGUEUR" DES VARIABLES INTERNES POUR 1 POINT DE GAUSS
!              CETTE LONGUEUR EST UN MAJORANT DU NBRE REEL DE VAR. INT.
! IN  CRIT    : CRITERES DE CONVERGENCE LOCAUX
! IN  PINTT   : COORDONNÉES DES POINTS D'INTERSECTION
! IN  CNSET   : CONNECTIVITE DES SOUS-ELEMENTS
! IN  HEAVT   : VALEURS DE L'HEAVISIDE SUR LES SS-ELTS
! IN  LONCH   : LONGUEURS DES CHAMPS UTILISÉES
! IN  BASLOC  : BASE LOCALE AU FOND DE FISSURE
! IN  IDEPL   : DEPLACEMENT A PARTIR DE LA CONF DE REF
! IN  LSN     : VALEUR DE LA LEVEL SET NORMALE AUX NOEUDS PARENTS
! IN  LST     : VALEUR DE LA LEVEL SET TANGENTE AUX NOEUDS PARENTS
! IN  PMILT   : COORDONNEES DES POINTS MILIEUX
! IN  NFISS   : NOMBRE DE FISSURES "VUES" PAR L'ÉLÉMENT
! IN  JHEAVN  : POINTEUR VERS LA DEFINITION HEAVISIDE
!
! OUT SIG     : CONTRAINTES DE CAUCHY (RAPH_MECA ET FULL_MECA)
! OUT VI      : VARIABLES INTERNES    (RAPH_MECA ET FULL_MECA)
! OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
! OUT IVECTU  : VECTEUR FORCES NODALES (RAPH_MECA ET FULL_MECA)
!..............................................................
!----------------------------------------------------------------
    character(len=8) :: elrefp, elrese(6), fami(6)
    real(kind=8) :: coorse(81), he(nfiss)
    integer :: nse, npg
    integer :: j, ise, in, ino, idebs, idebv, nnops
    integer :: ibid, idecpg, nbsig, ig, ifiss, ibid2, ncompn, heavn(nnop,5)
    integer :: irese, nno, jtab(7), ncomp, iret
!
    data    elrese /'SE2','TR3','TE4','SE3','TR6','T10'/
    data    fami   /'BID','XINT','XINT','BID','XINT','XINT'/
!
!
!     ATTENTION, DEPL ET VECTU SONT ICI DIMENSIONNÉS DE TELLE SORTE
!     QU'ILS NE PRENNENT PAS EN COMPTE LES DDL SUR LES NOEUDS MILIEU
!
    call elref1(elrefp)
!
!     NOMBRE DE COMPOSANTES DE PHEAVTO (DANS LE CATALOGUE)
    call tecach('OOO', 'PHEAVTO', 'L', iret, nval=2,&
                itab=jtab)
    ncomp = jtab(2)
!
!     ELEMENT DE REFERENCE PARENT : RECUP DE NDIM
    call elrefe_info(fami='RIGI',ndim=ndim,nnos=nnops)
!
!     SOUS-ELEMENT DE REFERENCE : RECUP DE NPG
    if (.not.iselli(elrefp)) then
        irese=3
    else
        irese=0
    endif
    call elrefe_info(elrefe=elrese(ndim+irese),fami=fami(ndim+irese),nno=nno,&
    npg=npg)
!
!     NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
    nbsig = nbsigm()
!
!     RECUPERATION DE LA DEFINITION DES DDL HEAVISIDES
    if (nfh.gt.0) then
      call tecach('OOO', 'PHEA_NO', 'L', iret, nval=7,&
                itab=jtab)
      ncompn = jtab(2)/jtab(3)
      ASSERT(ncompn.eq.5)
      do ino = 1, nnop
        do ig = 1 , ncompn
          heavn(ino,ig) = zi(jheavn-1+ncompn*(ino-1)+ig)
        enddo
      enddo
    endif
!     RÉCUPÉRATION DE LA SUBDIVISION DE L'ÉLÉMENT EN NSE SOUS ELEMENT
    nse=lonch(1)
!
!       BOUCLE D'INTEGRATION SUR LES NSE SOUS-ELEMENTS
    do ise = 1, nse
!
!       BOUCLE SUR LES 4/3 SOMMETS DU SOUS-TETRA/TRIA
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
!       FONCTION HEAVYSIDE CSTE POUR CHAQUE FISSURE SUR LE SS-ELT
        do ifiss = 1, nfiss
            he(ifiss) = heavt(ncomp*(ifiss-1)+ise)
        end do
!
!       DEBUT DE LA ZONE MEMOIRE DE SIG ET VI CORRESPONDANTE
        idecpg = npg * (ise-1)
        idebs = nbsig * idecpg
        idebv = lgpg * idecpg
!
        if (ndim .eq. 3) then
            ASSERT(nbsig.eq.6)
        else if (ndim.eq.2) then
            ASSERT(nbsig.eq.4)
        endif
!
        if (option .eq. 'RIGI_MECA') then
            call xxnmel(poum, elrefp, elrese(ndim+irese), ndim, coorse,&
                        igeom, he, nfh, ddlc, ddlm,&
                        nnops, nfe, basloc, nnop, npg,&
                        typmod, option, imate, compor, lgpg,&
                        crit2, ibid, lsn, lst, idecpg,&
                        sig2, vi2, matuu, ibid2, codret,&
                        nfiss, heavn)
!
            elseif (option(1:9).eq.'RAPH_MECA' .or. option(1:9)&
        .eq.'FULL_MECA' .or. option(1:10).eq.'RIGI_MECA_') then
!
!
            call xxnmel(poum, elrefp, elrese(ndim+irese), ndim, coorse,&
                        igeom, he, nfh, ddlc, ddlm,&
                        nnops, nfe, basloc, nnop, npg,&
                        typmod, option, imate, compor, lgpg,&
                        crit, idepl, lsn, lst, idecpg,&
                        sig(idebs+1), vi(idebv+1), matuu, ivectu, codret,&
                        nfiss, heavn)
        endif
!
!
!
    end do
!
end subroutine
