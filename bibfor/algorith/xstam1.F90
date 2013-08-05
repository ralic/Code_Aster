subroutine xstam1(nomo, noma, nbma, nmafis, mafis,&
                  stano, mafon, maen1, maen2, maen3,&
                  nmafon, nmaen1, nmaen2, nmaen3)
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/panbno.h"
    integer :: nmafis, nmafon, nmaen1, nmaen2, nmaen3, nbma
    integer :: stano(*), mafis(nmafis)
    integer :: mafon(nmafis), maen1(nbma), maen2(nbma), maen3(nbma)
    character(len=8) :: nomo, noma
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM
!
! CALCUL DU STATUT DES MAILLES SUIVANT LE STATUT DES NOEUDS EN COURS
!
! ----------------------------------------------------------------------
!
! IN  NOMO   : NOM DU MODELE
! IN  NOMA   : NOM DU MAILLAGE
! IN  NBMA   : NOMBRE DE MAILLES DU MAILLAGE
! IN  NMAFIS : NOMBRE DE MAILLES DE LA ZONE FISSURE
! IN  MAFIS  : VECTUER DES MAILLES DE LA ZONE FISSURE
! IN  STANO  : VECTEUR STATUT DES NOEUDS
!
! OUT  NMAFON : NOMBRE DE MAILLES CONTENANT LE FOND DE FISSURE
! OUT  NMAEN1 : NOMBRE DE MAILLES 'HEAVISIDE'
! OUT  NMAEN2 : NOMBRE DE MAILLES 'CRACKTIP'
! OUT  NMAEN3 : NOMBRE DE MAILLES 'HEAVISIDE-CRACKTIP'
! OUT  MAFON  : VECTEUR DES MAILLES 'CONTENANT LE FOND DE FISSURE
! OUT  MAEN1  : VECTEUR DES MAILLES 'HEAVISIDE'
! OUT  MAEN2  : VECTEUR DES MAILLES 'CRACKTIP'
! OUT  MAEN3  : VECTEUR DES MAILLES 'HEAVISIDE-CRACKTIP'
!
!
!
!
    integer :: jmoma, jma, nuno, jconx1, jconx2
    integer :: i, im1, im2, im3, ima, itypma, in, imae
    integer :: em, em1, em2, nmaabs, nbnott(3), nno, en
    character(len=8) :: typma
    character(len=19) :: mai
!
    call jemarq()
!
    i=0
    im1=0
    im2=0
    im3=0
!
    mai=noma//'.TYPMAIL'
    call jeveuo(mai, 'L', jma)
    call jeveuo(nomo//'.MAILLE', 'L', jmoma)
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
!
!     BOUCLE SUR LES MAILLES DU MAILLAGE
    do 310 ima = 1, nbma
!
        nmaabs=ima
!
        itypma=zi(jma-1+nmaabs)
        call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
        if (typma(1:3) .eq. 'POI') goto 310
!
!       ON ZAPPE LES MAILLES SANS MODELE
        if (zi(jmoma-1+nmaabs) .eq. 0) goto 310
!
        em=0
        em1=0
        em2=0
        call panbno(itypma, nbnott)
        nno = nbnott(1) + nbnott(2) + nbnott(3)
!
!       BOUCLE SUR LES NOEUDS DE LA MAILLE
        do 311 in = 1, nno
            nuno = zi(jconx1-1+zi(jconx2+nmaabs-1)+in-1)
            en = stano(nuno)
            if (en .eq. 1 .or. en .eq. 3) em1=em1+1
            if (en .eq. 2 .or. en .eq. 3) em2=em2+1
311      continue
        if (em1 .ge. 1) em=1
        if (em2 .ge. 1) em=2
        if (em1 .ge. 1 .and. em2 .ge. 1) em=3
!
        if (em2 .eq. nno .and. typma(1:3) .ne. 'SEG') then
!
!         MAILLE RETENUE POUR MAFOND (TS LS NOEUDS SONT 'CARRÉS')
!         SOUS RÉSERVE QUE CE SOIT UNE MAILLE DE MAFIS
!         ET PAS UNE MAILLE SEG
            do 312 imae = 1, nmafis
                if (nmaabs .eq. mafis(imae)) then
                    i=i+1
                    ASSERT(i.le.nmafis)
                    mafon(i)=nmaabs
!             ON SORT DE LA BOUCLE 312
                    goto 313
                endif
312          continue
313          continue
        endif
!
!       ON RÉCUPÈRE LES NUMEROS DES MAILLES ENRICHIES
        if (em .eq. 1) then
            im1=im1+1
            ASSERT(im1.le.nbma)
            maen1(im1)=nmaabs
        else if (em.eq.2) then
            im2=im2+1
            ASSERT(im2.le.nbma)
            maen2(im2)=nmaabs
        else if (em.eq.3) then
            im3=im3+1
            ASSERT(im3.le.nbma)
            maen3(im3)=nmaabs
        endif
310  end do
!
    nmafon=i
    nmaen1=im1
    nmaen2=im2
    nmaen3=im3
!
    call jedema()
end subroutine
