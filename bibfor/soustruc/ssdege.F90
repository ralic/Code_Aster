subroutine ssdege(nomu)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/jecrec.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jevtbl.h"
#include "asterfort/ssdeu1.h"
#include "asterfort/ssdeu2.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: nomu
! ----------------------------------------------------------------------
!     BUT:
!        - TRAITER LES MOTS CLEFS "DEFINITION" ET "EXTERIEUR"
!          DE LA COMMANDE MACR_ELEM_STAT.
!        - CREER LES OBJETS .REFM .LICA .LICH .VARM .DESM .LINO
!
!     IN:
!        NOMU : NOM DU MACR_ELEM_STAT QUE L'ON DEFINIT.
!
    character(len=8) :: kbi81, noma, nomo, nomgd, promes
    logical :: lmess
    real(kind=8) :: time
    character(len=16) :: pheno
! ----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: iaexte, ialino,  iarefm, ibid(1), iec
    integer :: ier, ii, ino, jdesm, jvarm, n1, nbc
    integer :: nbec, nbnoto, nch, nchar, nvalap
    integer, pointer :: prnm(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    call getvid('DEFINITION', 'CHAR_MACR_ELEM', iocc=1, nbval=0, nbret=n1)
    nchar=-n1
!
    call wkvect(nomu//'.REFM', 'G V K8', 9+nchar, iarefm)
!
!     -- RECUPERARTION DES NOMS DES REFERENCES:
!     -----------------------------------------
    call getvid('DEFINITION', 'MODELE', iocc=1, scal=nomo, nbret=n1)
!
    call dismoi('NOM_MAILLA', nomo, 'MODELE', repk=noma)
    call dismoi('PHENOMENE', nomo, 'MODELE', repk=pheno)
    call dismoi('NOM_GD', pheno, 'PHENOMENE', repk=nomgd)
    call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nbec)
!
    zk8(iarefm-1+1)= nomo
    zk8(iarefm-1+2)= noma
    call getvid('DEFINITION', 'CHAM_MATER', iocc=1, scal=kbi81, nbret=n1)
    if (n1 .ne. 0) zk8(iarefm-1+3)=kbi81
    call getvid('DEFINITION', 'CARA_ELEM', iocc=1, scal=kbi81, nbret=n1)
    if (n1 .ne. 0) zk8(iarefm-1+4)=kbi81
!
    zk8(iarefm-1+6)= 'NON_RIGI'
    zk8(iarefm-1+7)= 'NON_MASS'
    zk8(iarefm-1+8)= 'NON_AMOR'
!
    call getvid('DEFINITION', 'PROJ_MESU', iocc=1, scal=promes, nbret=ier)
    if (ier .eq. 0) then
        zk8(iarefm-1+9)= ' '
    else
        zk8(iarefm-1+9)= promes
    endif
!
!     -- RECUPERARTION DU NOM DES CHARGES CINEMATIQUES:
!     -------------------------------------------------
    if (nchar .gt. 0) then
        call getvid('DEFINITION', 'CHAR_MACR_ELEM', iocc=1, nbval=nchar, vect=zk8( iarefm-1+9+1),&
                    nbret=n1)
    endif
!
!     -- CREATION DES OBJETS .LICA ET .LICH:
!     --------------------------------------
    call getvis('DEFINITION', 'NMAX_CAS', iocc=1, scal=nbc, nbret=n1)
    nbc= max(nbc,1)
    call jecrec(nomu//'.LICA', 'G V R', 'NO', 'DISPERSE', 'CONSTANT',&
                nbc)
    call jecrec(nomu//'.LICH', 'G V K8', 'NO', 'CONTIG', 'CONSTANT',&
                nbc)
    call getvis('DEFINITION', 'NMAX_CHAR', iocc=1, scal=nch, nbret=n1)
    call jeecra(nomu//'.LICH', 'LONMAX', nch)
!
!
!     -- CREATION DE L'OBJET .VARM:
!     ------------------------------
    call getvr8('DEFINITION', 'INST', iocc=1, scal=time, nbret=n1)
    call wkvect(nomu//'.VARM', 'G V R', 2, jvarm)
    zr(jvarm-1+1)=jevtbl('TAILLE_BLOC')
    zr(jvarm-1+2)=time
!
!
!     -- CREATION DE L'OBJET .DESM:
!     ------------------------------
    call wkvect(nomu//'.DESM', 'G V I', 10, jdesm)
!
!
!     -- CREATION ET REMPLISSAGE DE L'OBJET .EXTERN  (VOLATILE)
!        (QUI CONTIENT UNE LISTE PROVISOIRE DES NOEUDS EXTERNES)
!     -----------------------------------------------------------
    call ssdeu1('NOMBRE', noma, nbnoto, ibid)
    call wkvect(nomu//'.EXTERN', 'V V I', nbnoto, iaexte)
    call ssdeu1('LISTE', noma, nbnoto, zi(iaexte))
!
!
!     -- ON MET "A ZERO" LES NOEUDS EXTERNES QUI NE PORTENT
!        AUCUN DDL POUR LE MODELE.
!     --------------------------------------------------------
    call jeveuo(nomo//'.MODELE    .PRNM', 'L', vi=prnm)
    lmess=.false.
    do ii = 1, nbnoto
        ino=zi(iaexte-1+ii)
        do iec = 1, nbec
            if (prnm(nbec*(ino-1)+iec) .ne. 0) goto 11
            zi(iaexte-1+ii)=0
            lmess=.true.
        end do
 11     continue
    end do
    if (lmess) then
        call utmess('A', 'SOUSTRUC_41')
    endif
!
!
!     -- ELIMINATION DES NOEUDS EXTERNES EN DOUBLE :
!     ----------------------------------------------
    call ssdeu2(nbnoto, zi(iaexte), nvalap)
    if (nvalap .ne. nbnoto) then
        call utmess('A', 'SOUSTRUC_42')
    endif
!
!
!     -- CREATION DE L'OBJET .LINO ET RECOPIE DE .EXTERN:
!     ---------------------------------------------------
    call wkvect(nomu//'.LINO', 'G V I', nvalap, ialino)
    do 21 , ii= 1,nvalap
    zi(ialino-1+ii)= zi(iaexte-1+ii)
    21 end do
    call jeecra(nomu//'.LINO', 'LONUTI', nvalap)
!
!
!     -- MISE A JOUR DE .DESM :
!     -------------------------
    zi(jdesm-1+2)=nvalap
    zi(jdesm-1+6)=nchar
!
!
    call jedetr(nomu//'.EXTERN')
    call jedema()
end subroutine
