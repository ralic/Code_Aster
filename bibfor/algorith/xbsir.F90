subroutine xbsir(ndim, nnop, nfh, nfe, ddlc,&
                 ddlm, igeom, compor, jpintt, cnset,&
                 heavt, lonch, basloc, sigref, nbsig,&
                 idepl, lsn, lst, ivectu, jpmilt,&
                 nfiss, jheavn)
!
! aslint: disable=W1306,W1504
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/iselli.h"
#include "asterfort/tecach.h"
#include "asterfort/xxbsig.h"
!
    integer :: ndim, nnop, nfh, nfe, ddlc, ddlm, igeom, nbsig, ivectu
    integer :: nfiss
    integer :: cnset(4*32), heavt(*), lonch(10), idepl, jpintt, jpmilt, jheavn
    real(kind=8) :: basloc(*), sigref(*), lsn(nnop), lst(nnop)
    character(len=16) :: compor(*)
!
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
!
!      BSIGMC  -- CALCUL DES FORCES INTERNES B*SIGMA AUX NOEUDS
!                 DE L'ELEMENT DUES AU CHAMP DE CONTRAINTES SIGMA
!                 DEFINI AUX POINTS D'INTEGRATION DANS LE CADRE DE
!                 LA MÉTHODE X-FEM
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  NNOP    : NOMBRE DE NOEUDS DE L'ELEMENT PARENT
! IN  NFH     : NOMBRE DE FONCTIONS HEAVYSIDE
! IN  NFE     : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT
! IN  DDLC    : NOMBRE DE DDLS DE CONTACT (PAR NOEUD)
! IN  DDLM    : NOMBRE DE DDL PAR NOEUD MILIEU
! IN  IGEOM   : COORDONEES DES NOEUDS
! IN  PINTT   : COORDONNÉES DES POINTS D'INTERSECTION
! IN  CNSET   : CONNECTIVITE DES SOUS-ELEMENTS
! IN  HEAVT   : VALEURS DE L'HEAVISIDE SUR LES SS-ELTS
! IN  LONCH   : LONGUEURS DES CHAMPS UTILISÉES
! IN  BASLOC  : BASE LOCALE AU FOND DE FISSURE
! IN  SIGMA   : CONTRAINTES DE CAUCHY AUX POINTS DE GAUSS DES SOUS-ÉLTS
! IN  NBSIG   : NOMBRE DE CONTRAINTES ASSOCIE A L'ELEMENT
! IN  LSN     : VALEUR DE LA LEVEL SET NORMALE AUX NOEUDS PARENTS
! IN  LST     : VALEUR DE LA LEVEL SET TANGENTE AUX NOEUDS PARENTS
! IN  PMILT   : COORDONNEES DES POINTS MILIEUX
! IN  NFISS   : NOMBRE DE FISSURES "VUES" PAR L'ÉLÉMENT
! IN  JHEAVN  : POINTEUR VERS LA DEFINITION HEAVISIDE
!
! OUT IVECTU  : ADRESSE DU VECTEUR BT.SIGMA
!
!     VARIABLES LOCALES
    real(kind=8) :: he(nfiss), coorse(81)
    character(len=8) :: elrefp, elrese(6), fami(6)
    integer :: nse, jtab(7), ncomp, iret
    integer :: ncompn, heavn(nnop, 5)
    integer :: ise, in, ino, npg, j, codopt
    integer :: irese, nno, ifiss, ig
!
    data          elrese /'SE2','TR3','TE4','SE3','TR6','T10'/
    data          fami   /'BID','XINT','XINT','BID','XINT','XINT'/
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
!
    call elref1(elrefp)
!
!     NOMBRE DE COMPOSANTES DE PHEAVTO (DANS LE CATALOGUE)
    call tecach('OOO', 'PHEAVTO', 'L', iret, nval=2,&
                itab=jtab)
    ncomp = jtab(2)
!
!   RECUPERATION DE LA DEFINITION DES FONCTIONS HEAVISIDES
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
!
!     SOUS-ELEMENT DE REFERENCE : RECUP DE NNO ET NPG
    if (.not.iselli(elrefp)) then
        irese=3
    else
        irese=0
    endif
    call elrefe_info(elrefe=elrese(ndim+irese),fami=fami(ndim+irese),nno=nno,npg=npg)
!
!     RÉCUPÉRATION DE LA SUBDIVISION DE L'ÉLÉMENT EN NSE SOUS ELEMENT
    nse=lonch(1)
!
!       BOUCLE SUR LES NSE SOUS-ELEMENTS
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
        codopt=0
        if (ndim .eq. 3) then
            ASSERT(nbsig.eq.6)
        else if (ndim.eq.2) then
            ASSERT(nbsig.eq.4)
        endif
!
        call xxbsig(elrefp, elrese(ndim+irese), ndim, coorse,&
                    igeom, he, nfh, ddlc, ddlm,&
                    nfe, basloc, nnop, npg, sigref,&
                    compor, idepl, lsn, lst, nfiss,&
                    heavn, codopt, ivectu)
!
    end do
!
!
!.============================ FIN DE LA ROUTINE ======================
!
end subroutine
