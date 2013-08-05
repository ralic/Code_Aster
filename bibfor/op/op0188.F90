subroutine op0188()
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
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/cncinv.h"
#include "asterfort/cnocns.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/nocart.h"
#include "asterfort/tecart.h"
#include "asterfort/wkvect.h"
!
! ----------------------------------------------------------------------
!
! OPERATEUR RAFF_XFEM_ZONE
!
! CALCUL D'UN INDICATEUR BINAIRE (APPELEE PAR RAFF_XFEM)
!
!
    integer :: ibid, iarg, iret, i, j, ino, nuno, numa, nbnozo
    integer :: jncmp, jvalv, ncmp, jmafon, nmafon, jma, nbma, nbno, nbmali
    integer :: jlst, jlsn, jnoeu, nbmac, jadr, adrvlc, acncin
    integer :: idlima, nbmazo
    real(kind=8) :: rayon, dist
    character(len=8) :: fiss, modele, ma, k8b, chout
    character(len=16) :: typdis, k16bid
    character(len=19) :: carte, cnslt, cnsln
    character(len=24) :: mafond, listma, cnxinv, lisnoz, lismaz
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
!     ------------------------------------
!     1) INITIALISATIONS
!     ------------------------------------
!
!     NOM DU CONCEPT EN SORTIE : CHOUT
    call getres(chout, k16bid, k16bid)
!
!     NOM DU CONCEPT FISSURE
    call getvid(' ', 'FISSURE', 1, iarg, 1,&
                fiss, ibid)
!
!     RECUP DU RAYON DE LA ZONE
    call getvr8(' ', 'RAYON', 1, iarg, 1,&
                rayon, ibid)
!
!     TYPE DE SD_FISS_XFEM EN ENTREE (FISSURE/INTERFACE)
    call dismoi('F', 'TYPE_DISCONTINUITE', fiss, 'FISS_XFEM', ibid,&
                typdis, iret)
!
!     MODELE ASSOCIE A LA FISSURE/INTERFACE
    call dismoi('F', 'NOM_MODELE', fiss, 'FISS_XFEM', ibid,&
                modele, iret)
!
!     MAILLAGE ASSOCIE AU MODELE
    call dismoi('F', 'NOM_MAILLA', modele, 'MODELE', ibid,&
                ma, iret)
    call dismoi('F', 'NB_MA_MAILLA', ma, 'MAILLAGE', nbma,&
                k8b, iret)
    call dismoi('F', 'NB_NO_MAILLA', ma, 'MAILLAGE', nbno,&
                k8b, iret)
!
!     INITIALISATION DE LA CARTE AVEC LA VALEUR 0
    carte = chout
    call alcart('G', carte, ma, 'NEUT_R')
    call jeveuo(carte//'.NCMP', 'E', jncmp)
    call jeveuo(carte//'.VALV', 'E', jvalv)
    ncmp = 1
    zk8(jncmp-1+1) = 'X1'
    zr(jvalv-1+1) = 0.d0
    call nocart(carte, 1, ' ', 'NOM', 0,&
                ' ', 0, ' ', ncmp)
!
!     CREATION DE LA LISTE DES MAILLES QUI AURONT LA VALEUR 1
    listma = '&&OP0188.LISTMA'
    call wkvect(listma, 'V V I', nbma, jma)
!
!     -------------------------------------------------------------
!     2) REMPLISSAGE DE LA LISTE AVEC LES MAILLES CONTENANT LE FOND
!        OU L'INTERFACE (ON PARLERA DE 'FOND'  DANS LES 2 CAS)
!     --------------------------------------------------------------
!
    if (typdis .eq. 'FISSURE') then
        mafond = fiss//'.MAILFISS.MAFOND'
    else if (typdis.eq.'INTERFACE') then
        mafond = fiss//'.MAILFISS  .HEAV'
    else
        ASSERT(.false.)
    endif
!
    call jeveuo(mafond, 'L', jmafon)
    call jelira(mafond, 'LONMAX', nmafon, k8b)
    do 10 i = 1, nmafon
        zi(jma-1+i)=zi(jmafon-1+i)
10  end do
!
!     ------------------------------------------------------------------
!     3) REMPLISSAGE DE LA LISTE AVEC LES MAILLES DONT UN NOEUD EST
!        DANS LA ZONE
!     ------------------------------------------------------------------
!
!     ON CREE D'ABORD LA LISTE DES NOEUDS QUI SONT LA ZONE
    lisnoz = '&&OP0188.NOEU'
    call wkvect(lisnoz, 'V V I', nbno, jnoeu)
!
!     RECUP DES LEVEL SETS
    cnslt = '&&OP0188.CNSLT'
    cnsln = '&&OP0188.CNSLN'
    call cnocns(fiss//'.LNNO', 'V', cnsln)
    call jeveuo(cnsln//'.CNSV', 'L', jlsn)
    if (typdis .eq. 'FISSURE') then
        call cnocns(fiss//'.LTNO', 'V', cnslt)
        call jeveuo(cnslt//'.CNSV', 'L', jlst)
    endif
!
!     REMPLISSAGE DE LA LISTE DES NOEUDS QUI SONT LA ZONE
    nbnozo=0
    do 300 ino = 1, nbno
        if (typdis .eq. 'FISSURE') then
            dist=sqrt(zr(jlst-1+ino)**2+zr(jlsn-1+ino)**2)
        else if (typdis.eq.'INTERFACE') then
            dist=sqrt(zr(jlsn-1+ino)**2)
        endif
        if (dist .le. rayon) then
            nbnozo = nbnozo + 1
            zi(jnoeu-1+nbnozo) = ino
        endif
300  end do
!
!     EMULATION DE DEFI_GROUP/CREA_GROUP_MA/OPTION='APPUI'
    lismaz = '&&OP0188.LISMAZ'
    call wkvect(lismaz, 'V V I', nbma, idlima)
!
!     CONNECTIVITE INVERSE
    cnxinv='&&OP0188.CNXINV'
    call jeexin(cnxinv, iret)
    if (iret .eq. 0) then
!       ON AIMERAIT LA STOCKER DANS LA BASE GLOBALE AU CAS OU ON EN AIT
!       ENCORE BESOIN (POUR LA FISSURE SUIVANTE) MAIS ON A PAS LE DROIT
        call cncinv(ma, ibid, 0, 'V', cnxinv)
    endif
    call jeveuo(jexatr(cnxinv, 'LONCUM'), 'L', adrvlc)
    call jeveuo(jexnum(cnxinv, 1), 'L', acncin)
!
    do 310 i = 1, nbnozo
        nuno = zi(jnoeu+i-1)
        nbmac = zi(adrvlc+nuno+1-1) - zi(adrvlc+nuno-1)
        jadr = zi(adrvlc+nuno-1)
        do 320 j = 1, nbmac
            numa = zi(acncin+jadr-1+j-1)
            zi(idlima+numa-1) = 1
320      continue
310  end do
!
!     REMPLISSAGE DE LA LISTE A LA SUITE
    nbmazo = 0
    do 330 i = 1, nbma
        if (zi(idlima+i-1) .eq. 1) then
            nbmazo = nbmazo + 1
            zi(jma-1+nmafon+nbmazo)=i
        endif
330  end do
!
!     NB DE MAILLES DANS LA LISTE
    nbmali = nmafon + nbmazo
!
!     -------------------------------------------------------------
!     4) STOCKAGE DANS LA CARTE ET TRANSFORMATION EN CHAM_ELEM
!     --------------------------------------------------------------
!
!     STOCKAGE DANS LA CARTE
    zk8(jncmp-1+1) = 'X1'
    zr(jvalv-1+1) = 1.d0
    call nocart(carte, 3, ' ', 'NUM', nbmali,&
                k8b, zi(jma), ' ', ncmp)
    call tecart(carte)
!
    call jedema()
end subroutine
