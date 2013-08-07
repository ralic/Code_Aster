subroutine utmavo(mail, kdim, lima, nlima, base,&
                  nomz, nbmavo, mailvo)
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/cncinv.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
    integer :: lima(*), nlima, nbmavo, mailvo(*)
    character(len=1) :: base
    character(len=2) :: kdim
    character(len=8) :: mail
    character(len=*) :: nomz
!-----------------------------------------------------------------------
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
!     DETERMINE LES MAILLES VOISINES D'UNE LISTE DE MAILLES
!     MAILLE --> LISTE DES MAILLES VOISINES
!
!   ARGUMENT EN ENTREE
!   ------------------
!     MAIL   : NOM DE L'OJB REPRESENTANT LE MAILLAGE
!     KDIM   : '3D' RECHERCHE LES MAILLES 3D VOISINES
!              '2D' RECHERCHE LES MAILLES 2D VOISINES
!              '1D' RECHERCHE LES MAILLES 1D VOISINES
!              '  ' RECHERCHE TOUTES LES MAILLES VOISINES
!     LIMA   : LISTE DES NUMEROS DE MAILLES
!     NLIMA  : NOMBRE DE MAILLES
!     BASE   : BASE DE CREATION
!     NOMZ   : NOM DE L' OJB A CREER
!     MAILVO : SI ORIE_PEAU_3D ("GROUP_MA_VOLU"):
!                  = LISTE DES MAILLES VOLUMIQUES
!                    UTILES A LA REORIENTATION
!              SI ORIE_PEAU_2D ("GROUP_MA_SURF"):
!                  = LISTE DES MAILLES SURFACIQUES
!                    UTILES A LA REORIENTATION
!              SINON: MAILVO N'EST PAS UTILISE
!     NBMAVO : NB DE MAILLES DE MAILVO
!
!   ORGANISATION
!   ------------
!     TYPE : XC V I ACCES(NUMEROTE) LONG(VARIABLE)
!-----------------------------------------------------------------------
!
    integer :: ibid, nare, numa, nbno, nbmat, ino, nuno, p1, p2
    integer :: i, j, k, jmail, nbman, adrvlc, acncin, ima, ii
    integer :: adra, iad, jtr1(1000), jtr2, idtyma, nutyma, iexinv
    character(len=8) :: type
    character(len=24) :: nom, ncninv
!     ------------------------------------------------------------------
    call jemarq()
!
    nom = nomz
!
! --- APPEL A LA CONNECTIVITE INVERSE
!
    ibid = 0
    ncninv = '&&UTMAVO.CONNEC_INV'
!     EST-CE QUE LA CONNECTIVITE INVERSE A DEJA ETE CALCULEE ?
    call jeexin(ncninv, iexinv)
    if (nbmavo .eq. 0) then
        if (iexinv .eq. 0) call cncinv(mail, ibid, ibid, 'V', ncninv)
    else
!        ON FORCE LE CALCUL DE LA CONNECTIVITE INVERSE
        call jedetr(ncninv)
        call cncinv(mail, mailvo, nbmavo, 'V', ncninv)
    endif
    call jeveuo(jexatr(ncninv, 'LONCUM'), 'L', adrvlc)
    call jeveuo(ncninv, 'L', acncin)
!
! --- APPEL A LA CONNECTIVITE
!
    call jeveuo(jexatr(mail//'.CONNEX', 'LONCUM'), 'L', p2)
    call jeveuo(mail//'.CONNEX', 'L', p1)
!
    call jeveuo(mail//'.TYPMAIL', 'L', idtyma)
!
! --- DIMENSIONNEMENT DE LA SD
!
    call wkvect('&&UTMAVO.TRAV2', 'V V I', nlima, jtr2)
    nare = 0
    do 100 i = 1, nlima
        numa = lima(i)
        nbno = zi(p2+numa+1-1) - zi(p2+numa-1)
        iad = zi(p2+numa-1)
        nbmat = 0
        do 110 ino = 1, nbno
            nuno = zi(p1+iad-1+ino-1)
            nbman = zi(adrvlc+nuno+1-1) - zi(adrvlc+nuno-1)
            adra = zi(adrvlc+nuno-1)
            do 120 j = 1, nbman
                ii = zi(acncin+adra-1+j-1)
!              -- SI UN NOEUD EST ORPHELIN : II=0
!                 (PAS D'OBJET JEVEUX DE LONG=0)
                if (ii .eq. 0) then
                    ASSERT(nbman.eq.1)
                    goto 120
                endif
                if (nbmavo .eq. 0) then
                    ima=ii
                else
                    ima=mailvo(ii)
                endif
                if (ima .eq. numa) goto 120
                nutyma = zi(idtyma+ima-1)
                call jenuno(jexnum('&CATA.TM.NOMTM', nutyma), type)
                if (type(1:4) .eq. 'HEXA') then
                    if (kdim .eq. '2D' .or. kdim .eq. '1D') goto 120
                else if (type(1:4).eq.'PENT') then
                    if (kdim .eq. '2D' .or. kdim .eq. '1D') goto 120
                else if (type(1:4).eq.'PYRA') then
                    if (kdim .eq. '2D' .or. kdim .eq. '1D') goto 120
                else if (type(1:4).eq.'TETR') then
                    if (kdim .eq. '2D' .or. kdim .eq. '1D') goto 120
                else if (type(1:4).eq.'QUAD') then
                    if (kdim .eq. '3D' .or. kdim .eq. '1D') goto 120
                else if (type(1:4).eq.'TRIA') then
                    if (kdim .eq. '3D' .or. kdim .eq. '1D') goto 120
                else if (type(1:3).eq.'SEG') then
                    if (kdim .eq. '3D' .or. kdim .eq. '2D') goto 120
                else if (type(1:3).eq.'POI') then
                    if (kdim .ne. '  ') goto 120
                else
                    call u2mesk('F', 'PREPOST4_89', 1, type)
                endif
                do 122 k = 1, nbmat
                    if (jtr1(k) .eq. ima) goto 120
122              continue
                nbmat = nbmat + 1
                jtr1(nbmat) = ima
120          continue
110      continue
        zi(jtr2-1+i) = nbmat
        nare = nare + max(nbmat,1)
100  end do
!
! --- CREATION DE LA SD
!
    call jecrec(nom, base//' V I', 'NU', 'CONTIG', 'VARIABLE',&
                nlima)
    call jeecra(nom, 'LONT', nare)
!
! --- ON REMPLIT LA SD
!
    do 200 i = 1, nlima
        numa = lima(i)
        nbno = zi(p2+numa+1-1) - zi(p2+numa-1)
        iad = zi(p2+numa-1)
        call jecroc(jexnum(nom, i))
        if (zi(jtr2-1+i) .eq. 0) then
            call jeecra(jexnum(nom, i), 'LONMAX', 1)
            call jeecra(jexnum(nom, i), 'LONUTI', 0)
            goto 200
        else
            call jeecra(jexnum(nom, i), 'LONMAX', zi(jtr2-1+i))
            call jeecra(jexnum(nom, i), 'LONUTI', zi(jtr2-1+i))
            call jeveuo(jexnum(nom, i), 'E', jmail)
        endif
!
        nbmat = 0
        do 210 ino = 1, nbno
            nuno = zi(p1+iad-1+ino-1)
            nbman = zi(adrvlc+nuno+1-1) - zi(adrvlc+nuno-1)
            adra = zi(adrvlc+nuno-1)
            do 220 j = 1, nbman
                ii = zi(acncin+adra-1+j-1)
                if (ii .eq. 0) goto 220
!
                if (nbmavo .eq. 0) then
                    ima=ii
                else
                    ima=mailvo(ii)
                endif
                if (ima .eq. numa) goto 220
                nutyma = zi(idtyma+ima-1)
                call jenuno(jexnum('&CATA.TM.NOMTM', nutyma), type)
                if (type(1:4) .eq. 'HEXA') then
                    if (kdim .eq. '2D') goto 220
                    if (kdim .eq. '1D') goto 220
                else if (type(1:4).eq.'PENT') then
                    if (kdim .eq. '2D') goto 220
                    if (kdim .eq. '1D') goto 220
                else if (type(1:4).eq.'PYRA') then
                    if (kdim .eq. '2D') goto 220
                    if (kdim .eq. '1D') goto 220
                else if (type(1:4).eq.'TETR') then
                    if (kdim .eq. '2D') goto 220
                    if (kdim .eq. '1D') goto 220
                else if (type(1:4).eq.'QUAD') then
                    if (kdim .eq. '3D') goto 220
                    if (kdim .eq. '1D') goto 220
                else if (type(1:4).eq.'TRIA') then
                    if (kdim .eq. '3D') goto 220
                    if (kdim .eq. '1D') goto 220
                else if (type(1:3).eq.'SEG') then
                    if (kdim .eq. '3D') goto 220
                    if (kdim .eq. '2D') goto 220
                else if (type(1:3).eq.'POI') then
                    if (kdim .ne. '  ') goto 220
                else
                    call u2mesk('F', 'PREPOST4_89', 1, type)
                endif
                do 222 k = 1, nbmat
                    if (zi(jmail-1+k) .eq. ima) goto 220
222              continue
                nbmat = nbmat + 1
                zi(jmail-1+nbmat) = ima
220          continue
210      continue
200  end do
!
    call jedetr('&&UTMAVO.TRAV1')
    call jedetr('&&UTMAVO.TRAV2')
    if (nbmavo .ne. 0) call jedetr(ncninv)
    call jedema()
!
end subroutine
