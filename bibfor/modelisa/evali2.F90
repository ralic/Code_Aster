subroutine evali2(isz, pg, nma, phi, valpar,&
                  posmai, ipg, pdgi, icmp, nocmpi,&
                  sphi)
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
    implicit none
!-----------------------------------------------------------------------
!     OPERATEUR PROJ_SPEC_BASE
!     CREATION DE LA MATRICE DES MODES PROPRES DEFINIS SUR LES POINTS DE
!     GAUSS ET DE LA LISTE DES POINTS DE GAUSS ASSOCIES AVEC LEURS
!     COORDONNEES
!-----------------------------------------------------------------------
! IN      : ISZ    : INTER-SPECTRE CONTENANT LES FONCTIONS ANALYTIQUES
!                    A PROJETER SUR LES MODES
! IN      : COL    : INDICE DE LA COLONNE COURANTE DE CALCUL
! IN      : PG     : CHAM_ELEM_S CONTENANT LES COORDONNEES DES POINTS
!                    DE GAUSS ET LEURS POIDS RESPECTIFS
! IN      : NMA    : NOMBRE DE MAILLES DU LIGREL
! IN      : PHI    : VECTEUR CONTENANT LES NOMS DES MODES PROPRES
!                    DEFINIS AUX POINTS DE GAUSS (CHAM_ELEM_S)
! IN      : VALPAR : COORDONNEES DU POINT DE GAUSS COURANT I (VALEURS 1
!                    A 3) ET FREQUENCE COURANTE (VAL 7). lES VALEURS
!                    4 A 6 STOCKENT LES COORDONNEES DU PDG J
! IN      : POSMAI : POSITION DE LA MAILLE DE CALCUL COURANT I
! IN      : IPG    : POINT DE GAUSS DE LA MAILLE DU CALCUL COURANT I
! IN/OUT  : SPHI   : VECTEUR CONTENANT LES NOMS DE CHAM_ELEM_S POUR
!                    Y RANGER LE RESULTAT DE S.PHI
!-----------------------------------------------------------------------
!
!
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/fointc.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: icmpi, icmpj, ier, ifo
    integer :: ili, nbpara, ipara, nbl, ipg, jpg, nbpg,  jma
    integer :: modj, nbcmp, nbm, nbsp, nma, iphi, posma,  idfi, ilfi
    integer :: ikfi,  icmp, iret, ivpg, isphi, posmai, posmaj
    integer :: jcmp
    real(kind=8) :: valpar(7), pdgj, pdgi, valphi, zerod
    real(kind=8) :: resur, resui
    complex(kind=8) :: resu, valsph
    character(len=19) :: phi, sphi, phii, sphii, pg, is
    character(len=24) :: k24, cmpis, cmpjs, nofos
    character(len=8) :: nompar(7), fonc, cmpi, cmpj, nocmpi, nocmpj
    character(len=8) ::  isz
    integer, pointer :: tbnp(:) => null()
    integer, pointer :: vpg(:) => null()
    character(len=8), pointer :: cesc(:) => null()
    character(len=24), pointer :: tblp(:) => null()
    real(kind=8), pointer :: vfi(:) => null()
    complex(kind=8), pointer :: vsfi(:) => null()
!-----------------------------------------------------------------------
!                         _       _    _   _
!   CALCUL DE LA MATRICE | SXX SXY |  | PHI |
!                        |_SYX SYY_|  |_   _|
!   LES FONCTIONS SZZ SONT DEFINIES ANALYTIQUEMENT DANS UNE TABLE INTER
!   SPECTRALE. DANS LE CAS CI-DESSUS, ON SUPPOSE QUE LES EXCITATIONS
!   SONT DANS LES DIRECTIONS X ET Y ET QU'ELLES SONT CORRELEES ENTRE
!   ELLES (TERME CROISE SXY ET SYX). NORMALEMENT, IL FAUT QUE SXY ET
!   SYX SOIENT CONJGUEES L'UNE DE L'AUTRE, MAIS ON NE LE VERIFIE PAS
!   TODO !!!! AJOUTER CETTE VERIFICATION ?
!
!   STRUCTURE DE LA ROUTINE : LE ROUTINE EVALIS FAIT UNE BOUCLE SUR
!   LE POINT DE GAUSS I. ICI, ON BOUCLE SUR LES PDG J :
!   1 - LECTURE DE L'INTER-SPECTRE
!   2 - BOUCLE SUR LES LIGNES DE L'INTER-SPECTRE (COMPOSANTES I ET J)
!         BOUCLE SUR LES MAILLES
!           BOUCLE SUR LES POINTS DE GAUSS
!             EVALUATION DE LA FONCTION ANALYTIQUE CORRESPONDANT AUX
!             COMPOSANTES DONNEES A LA LIGENDE L'INTER-SPECTRE
!             BOUCLE SUR LES MODES
!               BOUCLE SUR LES COMPOSANTES
!                 SI LA COMPOSANTE COURANTE EST CELLE DE LA COLONNE
!                 DE L'INTER-SPECTRE
!                  CALCUL DE PHI(IMA,IPG,IMOD,ICMP).WI.WJ.SZZ(I,J,IFREQ)
!
    call jemarq()
!
    zerod=0.0d0
!
!
! NOMS DES PARAMETRES DES FONCTIONS ANALYTIQUES
    nompar(1)='X1'
    nompar(2)='Y1'
    nompar(3)='Z1'
    nompar(4)='X2'
    nompar(5)='Y2'
    nompar(6)='Z2'
    nompar(7)='FREQ'
!
! EXPLORATION DE LA TABLE INTER-SPECTRE CONTENANT LES FONCTIONS DE FORME
    is='                   '
    is(1:8)=isz
    call jeveuo(is//'.TBNP', 'L', vi=tbnp)
    call jeveuo(is//'.TBLP', 'L', vk24=tblp)
    nbpara=tbnp(1)
    nbl=tbnp(2)
    do 1 ipara = 1, nbpara
        k24=tblp(1-4+ipara*4)
        if (k24(1:10) .eq. 'FONCTION_C') then
            nofos=tblp(1-4+ipara*4+2)
        else if (k24(1:12).eq.'NUME_ORDRE_I') then
            cmpis=tblp(1-4+ipara*4+2)
        else if (k24(1:12).eq.'NUME_ORDRE_J') then
            cmpjs=tblp(1-4+ipara*4+2)
        endif
 1  end do
!
! CHAMP CONTENANT LES COORDONNEES DES POINTS DE GAUSS DU MAILLAGE
    call jeveuo(pg//'.CESV', 'L', ivpg)
    call jeveuo(pg//'.CESD', 'L', vi=vpg)
!
! RECUPERATION DES NOMS DES CHAMPS PHI ET SPHI
    call jeveuo(phi, 'L', iphi)
    call jeveuo(sphi, 'L', isphi)
!
! NOMBRE DE MODES
    call jelira(phi, 'LONMAX', nbm)
!
! BOUCLES SUR LES LIGNES DE LA TABLE INTER-SPECTRE ANALYTIQUE
    do 3 ili = 1, nbl
        call jeveuo(cmpis, 'L', icmpi)
        cmpi=zk8(icmpi-1+ili)
        call jeveuo(cmpjs, 'L', icmpj)
        cmpj=zk8(icmpj-1+ili)
        call jeveuo(nofos, 'L', ifo)
        fonc=zk8(ifo-1+ili)
! BOUCLE SUR LES MAILLES ET POINTS DE GAUSS & EVALUATION DE LA FONCTION
! DE L'IS CORRESPONDANT AUX COMP CMPI ET CMPJ
        do 4 jma = 1, nma
!  NOMBRE DE PDG ET DE SOUS PDG DE LA MAILLE JMA
            nbpg=vpg(5+4*(jma-1)+1)
            nbsp=vpg(5+4*(jma-1)+2)
            posma=vpg(5+4*(jma-1)+4)
            ASSERT(nbsp.eq.1)
            do 5 jpg = 1, nbpg
!  COORDONNEES DU POINT DE GAUSS JPG X2,Y2,Z2
                valpar(4)=zr(ivpg+posma+4*(jpg-1))
                valpar(5)=zr(ivpg+posma+4*(jpg-1)+1)
                valpar(6)=zr(ivpg+posma+4*(jpg-1)+2)
                pdgj=zr(ivpg+posma+4*(jpg-1)+3)
                call fointc('F', fonc, 7, nompar, valpar,&
                            resur, resui, ier)
                resu=dcmplx(resur,resui)
!
! BOUCLE SUR LES MODES
                do 6 modj = 1, nbm
! ALLER CHERCHER LA VALEUR DE PHI(MODJ) AU POINT DE GAUSS DONNE
! POUR LA COMPOSANTE DONNEE
                    phii=zk24(iphi-1+modj)(1:19)
                    sphii=zk24(isphi-1+modj)(1:19)
                    call jeveuo(phii//'.CESV', 'L', vr=vfi)
                    call jeveuo(phii//'.CESD', 'L', idfi)
                    call jeveuo(phii//'.CESL', 'L', ilfi)
                    call jeveuo(phii//'.CESK', 'L', ikfi)
                    call jeveuo(phii//'.CESC', 'L', vk8=cesc)
                    call jeveuo(sphii//'.CESV', 'E', vc=vsfi)
                    nbcmp=zi(idfi-1+5+4*(jma-1)+3)
                    posmaj=zi(idfi-1+5+4*(jma-1)+4)
                    do 7 jcmp = 1, nbcmp
                        nocmpj=cesc(jcmp)
                        if ((nocmpj.eq.cmpj) .and. (nocmpi.eq.cmpi)) then
                            call cesexi('S', idfi, ilfi, jma, jpg,&
                                        1, jcmp, iret)
                            if (iret .lt. 0) goto 8
                            valphi=vfi(1+posmaj+nbcmp*(jpg-1)+jcmp-&
                            1)
! CALCUL DE WI.WJ.SFF(XI,XJ,FREQ).PHI(XJ,MODJ)
                            valsph = vsfi(1+posmai+nbcmp*(ipg-1)+ icmp-1)
                            valsph = valsph + dcmplx(valphi,zerod)* resu *dcmplx(pdgi,zerod)*dcmp&
                                     &lx(pdgj, zerod)
                            vsfi(1+posmai+nbcmp*(ipg-1)+icmp-1) =&
                            valsph
                        endif
 8                      continue
 7                  continue
 6              continue
 5          continue
 4      continue
 3  end do
!
    call jedema()
end subroutine
