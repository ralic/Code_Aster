subroutine evalis(isz, pg, phi, sphi, freq,&
                  iff, nomres)
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
    implicit none
!-----------------------------------------------------------------------
!     OPERATEUR PROJ_SPEC_BASE
!     CREATION DE LA MATRICE DES MODES PROPRES DEFINIS SUR LES POINTS DE
!     GAUSS ET DE LA LISTE DES POINTS DE GAUSS ASSOCIES AVEC LEURS
!     COORDONNEES
!-----------------------------------------------------------------------
! IN      : PG     : CHAM_ELEM_S CONTENANT LES COORDONNEES DES POINTS
!                    DE GAUSS ET LEURS POIDS RESPECTIFS
! IN      : PHI    : VECTEUR CONTENANT LES NOMS DES MODES PROPRES
!                    DEFINIS AUX POINTS DE GAUSS (CHAM_ELEM_S)
! IN      : SPHI   : VECTEUR CONTENANT CHAM_ELEM_S DEFNIS SUR LE MEME
!                    LIGREL QUE PHI INITIALISES A 0 ET COMPLEXES
! IN      : JPG    : NUMERO DU POINT DE GAUSS POUR LEQUEL ON CALCULE
! IN      : NPG    : NOMBRE DE POINTS DE GAUSS
! IN      : FREQ   : FREQUENCE A LAQUELLE ON CALCULE
! IN      : LVALE  : ADRESSE DES INTERSPECTRES
!-----------------------------------------------------------------------
!
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/evali2.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    integer :: ima, nma, nbpg, nbsp,  ivpg, ipg, posma, lvale
    integer :: im1, im2, ivfi, ivsfi, nbval, iphi, isphi, nbm, icmp, idfi, ilfi
    integer :: iret, nbcmp, ival, ind, iff
    integer ::  posmai, nbpoin
    integer :: lnumi, lnumj, nbabs
    real(kind=8) :: valpar(7), freq, pdgi
    complex(kind=8) :: val
    character(len=8) :: isz, nocmpi, nomres
    character(len=19) :: pg, phi, sphi, phii, sphii
    character(len=24) :: chnumi, chnumj, chfreq, chvale
    integer, pointer :: vpg(:) => null()
    character(len=8), pointer :: cesc(:) => null()
!
!-----------------------------------------------------------------------
!
    call jemarq()
!
! CHAMP CONTENANT LES POINTS DE GAUSS
    call jeveuo(pg//'.CESV', 'L', ivpg)
    call jeveuo(pg//'.CESD', 'L', vi=vpg)
!
! NOMBRE DE MAILLES
    nma=vpg(1)
!
! NOMBRE DE MODES
    call jelira(phi, 'LONMAX', nbm)
!
! PAS DE FREQUENCE COURANT
    valpar(7)=freq
!
! RECUPERATION DES NOMS DES CHAMPS PHI ET SPHI
    call jeveuo(phi, 'L', iphi)
    call jeveuo(sphi, 'L', isphi)
!
! EXPLORATION DU MODE 1. NB : ON SUPPOSE QUE TOUS LES MODES SONT DEFINIS
! AUX MEMES DDL
    phii=zk24(iphi)(1:19)
    call jeveuo(phii//'.CESV', 'L', ivfi)
    call jeveuo(phii//'.CESD', 'L', idfi)
    call jeveuo(phii//'.CESL', 'L', ilfi)
    call jeveuo(phii//'.CESC', 'L', vk8=cesc)
!
! CALCUL DE LA MATRICE S.PHI
! BOUCLE SUR LES MAILLES ET POINTS DE GAUSS I
    do 4 ima = 1, nma
!  NOMBRE DE PDG ET DE SOUS PDG DE LA MAILLE IMA
        nbpg=vpg(5+4*(ima-1)+1)
        nbsp=vpg(5+4*(ima-1)+2)
        posma=vpg(5+4*(ima-1)+4)
        ASSERT(nbsp.eq.1)
        do 5 ipg = 1, nbpg
!  COORDONNEES DU POINT DE GAUSS IPG X1,Y1,Z1 ET POIDS DE GAUSS
            valpar(1)=zr(ivpg+posma+4*(ipg-1))
            valpar(2)=zr(ivpg+posma+4*(ipg-1)+1)
            valpar(3)=zr(ivpg+posma+4*(ipg-1)+2)
            pdgi=zr(ivpg+posma+4*(ipg-1)+3)
            nbcmp=zi(idfi-1+5+4*(ima-1)+3)
            posmai=zi(idfi-1+5+4*(ima-1)+4)
            do 6 icmp = 1, nbcmp
                nocmpi=cesc(icmp)
                call cesexi('S', idfi, ilfi, ima, ipg,&
                            1, icmp, iret)
                if (iret .lt. 0) goto 7
! CALCUL DE LA S.PHI POUR LA MAILLE IMA, LE PDG IPG, ET LA COMPOSANTE
! ICMP
                call evali2(isz, pg, nma, phi, valpar,&
                            posmai, ipg, pdgi, icmp, nocmpi,&
                            sphi)
!
 7              continue
 6          continue
 5      continue
 4  end do
!
!
! A CE NIVEAU, ON A DEUX BASES DE MODES PROPRES DEFINIS AUX POINTS
! DE GAUSS
!  - LA MATRICE DES MODES PROPRES PHI : '&&SFIFJ.PHI'
!  - LA MATRICE S.PHI : '&&SFIFJ.SPHI'
! CES NOMS SONT DES VECTEURS DE TAILLE NB_MODES CONTENANT LES NOMS DES
! CHAMPS CHAM_ELEM_S CORRESPONDANT
!
! CALCUL DE LA LONGUEUR DES CHAMPS PHI ET S.PHI. NB : ON SUPPOSE QUE
! TOUS LES MODES ONT LA MEME TAILLE
    phii=zk24(iphi)(1:19)
    call jelira(phii//'.CESV', 'LONMAX', nbval)
!
    chnumi = nomres//'.NUMI'
    call jeveuo(chnumi, 'E', lnumi)
    chnumj = nomres//'.NUMJ'
    call jeveuo(chnumj, 'E', lnumj)
    chvale = nomres//'.VALE'
    chfreq = nomres//'.DISC'
    call jelira(chfreq, 'LONMAX', nbpoin)
!
! PRODUIT PHI^T.S.PHI (UNIQUEMENT LA PARTIE TRIANGULAIRE SUPERIEURE)
    ind=1
    do 11 im1 = 1, nbm
        do 12 im2 = im1, nbm
            if (iff .eq. 0) then
                zi(lnumi-1+ind) = im1
                zi(lnumj-1+ind) = im2
                if (im1 .eq. im2) then
                    nbabs = nbpoin
                else
                    nbabs = 2*nbpoin
                endif
                call jecroc(jexnum(chvale, ind))
                call jeecra(jexnum(chvale, ind), 'LONMAX', nbabs)
                call jeecra(jexnum(chvale, ind), 'LONUTI', nbabs)
            endif
            call jeveuo(jexnum(chvale, ind), 'E', lvale)
!
            val=(0.0d0,0.0d0)
            phii=zk24(iphi-1+im1)(1:19)
            sphii=zk24(isphi-1+im2)(1:19)
            call jeveuo(phii//'.CESV', 'L', ivfi)
            call jeveuo(sphii//'.CESV', 'L', ivsfi)
            do 13 ival = 1, nbval
                val=val+dcmplx(zr(ivfi-1+ival),0.0d0)*zc(ivsfi-1+ival)
13          continue
            if (im1 .eq. im2) then
                zr(lvale+iff)=dble(val)
            else
                zr(lvale+2*iff)=dble(val)
                zr(lvale+2*iff+1)=dimag(val)
            endif
!
            ind=ind+1
12      continue
11  end do
!
! REMISE A 0 DES CHAM_ELEM_S DE SPHI
    do 14 im1 = 1, nbm
        sphii=zk24(isphi-1+im1)(1:19)
        call jeveuo(sphii//'.CESV', 'E', ivsfi)
        do 15 ival = 1, nbval
            zc(ivsfi-1+ival)=(0.0d0,0.0d0)
15      continue
14  end do
!
!-----------------------------------------------------------------------
!
    call jedema()
end subroutine
