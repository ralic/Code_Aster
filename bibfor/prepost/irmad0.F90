subroutine irmad0(ifc, versio, nstat, chamno, nomsym)
    implicit none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/dismoi.h"
#include "asterfort/irmad1.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: versio, nstat
    character(len=*) :: chamno(*), nomsym
!--------------------------------------------------------------------
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
!     IMPRESSION D'UNE LISTE DE CHAMNO A COMPOSANTES REELLES OU
!     COMPLEXES AU FORMAT IDEAS  ( COMMANDE MACRO_MADMACS )
!     ON IMPRIME LE CHAMNO A L'AIDE D'UN DATASET 252
!C
!     ------------------------------------------------------------------
!
    character(len=1) :: type, typi
    integer :: gd, num, gdi, numi
    character(len=8) :: k8b, nomma, nomgd
    character(len=16) :: nomcmd
    character(len=19) :: chamn
    character(len=24) :: nomnu
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iad, iadesc, iaec,  iaprno
    integer :: ibid, ifc, ino, iret, itype
    integer ::  nbno, ncmpmx, nec
    character(len=8), pointer :: nomnoe(:) => null()
    integer, pointer :: numnoe(:) => null()
    integer, pointer :: nueq(:) => null()
    character(len=24), pointer :: refe(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    chamn = chamno(1)
!
    call jeveuo(chamn//'.REFE', 'L', vk24=refe)
!
!     --- NOM DU MAILLAGE
    nomma = refe(1) (1:8)
!
!     --- NOM DU PROFIL AUX NOEUDS ASSOCIE S'IL EXISTE
    nomnu = refe(2)
!
    call jelira(chamn//'.VALE', 'TYPE', cval=type)
    if (type .eq. 'R') then
        itype = 1
    else if (type .eq. 'C') then
        itype = 2
    else
        call getres(k8b, k8b, nomcmd)
        call utmess('A', 'PREPOST_97', sk=type(1:1))
        goto 999
    endif
!
    call jeveuo(chamn//'.DESC', 'L', iadesc)
    gd = zi(iadesc-1+1)
    num = zi(iadesc-1+2)
    call jenuno(jexnum('&CATA.GD.NOMGD', gd), nomgd)
!
!     --- ON VERIFIE QUE TOUS LES CHAMPS SONT IDENTIQUES
!
    do i = 1, nstat
        chamn = chamno(i)
        call jeveuo(chamn//'.DESC', 'L', iadesc)
        gdi = zi(iadesc-1+1)
        numi = zi(iadesc-1+2)
        if (gdi .ne. gd) then
            call utmess('F', 'PREPOST2_67')
        endif
        if (numi .ne. num) then
            call utmess('F', 'PREPOST2_68')
        endif
        call jelira(chamn//'.VALE', 'TYPE', cval=typi)
        if (typi .ne. type) then
            call utmess('F', 'PREPOST2_69')
        endif
    end do
!
!     --- NOMBRE D'ENTIERS CODES POUR LA GRANDEUR NOMGD
    nec = nbec(gd)
    call jeexin('&&IRMAD0.ENT_COD', iret)
    if (iret .ne. 0) call jedetr('&&IRMAD0.ENT_COD')
    call wkvect('&&IRMAD0.ENT_COD', 'V V I', nec, iaec)
    call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', ncmpmx)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', iad)
!
!     --- SI LE CHAMP EST A REPRESENTATION CONSTANTE: RIEN DE SPECIAL
!
!     --- SI LE CHAMP EST DECRIT PAR UN "PRNO":
    if (num .ge. 0) then
        call jeveuo(nomnu(1:19)//'.NUEQ', 'L', vi=nueq)
        call jenonu(jexnom(nomnu(1:19)//'.LILI', '&MAILLA'), ibid)
        call jeveuo(jexnum(nomnu(1:19)//'.PRNO', ibid), 'L', iaprno)
    endif
!
!     --- NOMBRE DE NOEUDS DU MAILLAGE: NBNO
    call dismoi('NB_NO_MAILLA', nomma, 'MAILLAGE', repi=nbno)
!
!     --- CREATION LISTES DES NOMS ET DES NUMEROS DES NOEUDS A IMPRIMER
    AS_ALLOCATE(vk8=nomnoe, size=nbno)
    AS_ALLOCATE(vi=numnoe, size=nbno)
    do ino = 1, nbno
        call jenuno(jexnum(nomma//'.NOMNOE', ino), nomnoe(ino))
        numnoe(ino) = ino
    end do
!
    if (num .ge. 0) then
        ncmpmx = 6
        call irmad1(ifc, versio, nbno, zi(iaprno), nueq,&
                    nec, zi(iaec), ncmpmx, itype, nstat,&
                    chamno, zk8(iad), nomsym,numnoe)
    else
        call getres(k8b, k8b, nomcmd)
        call utmess('E', 'PREPOST2_70')
    endif
!
! --- MENAGE
    call jedetr('&&IRMAD0.ENT_COD')
    AS_DEALLOCATE(vk8=nomnoe)
    AS_DEALLOCATE(vi=numnoe)
!
999 continue
    call jedema()
end subroutine
