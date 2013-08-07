subroutine irmad0(ifc, versio, nstat, chamno, nomsym)
    implicit none
#include "jeveux.h"
!
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
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
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
    character(len=1) ::  type, typi
    integer :: gd, num, gdi, numi
    character(len=8) :: k8b, nomma, nomgd
    character(len=16) :: nomcmd
    character(len=19) :: chamn
    character(len=24) :: nomnu
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iad, iadesc, iaec, ianueq, iaprno, iarefe
    integer :: ibid, ier, ifc, ino, iret, itype, jno
    integer :: jnu, nbno, ncmpmx, nec
!-----------------------------------------------------------------------
    call jemarq()
    chamn = chamno(1)
!
    call jeveuo(chamn//'.REFE', 'L', iarefe)
!
!     --- NOM DU MAILLAGE
    nomma = zk24(iarefe-1+1) (1:8)
!
!     --- NOM DU PROFIL AUX NOEUDS ASSOCIE S'IL EXISTE
    nomnu = zk24(iarefe-1+2)
!
    call jelira(chamn//'.VALE', 'TYPE', cval=type)
    if (type .eq. 'R') then
        itype = 1
    else if (type .eq. 'C') then
        itype = 2
    else
        call getres(k8b, k8b, nomcmd)
        call u2mesk('A', 'PREPOST_97', 1, type(1:1))
        goto 9999
    endif
!
    call jeveuo(chamn//'.DESC', 'L', iadesc)
    gd = zi(iadesc-1+1)
    num = zi(iadesc-1+2)
    call jenuno(jexnum('&CATA.GD.NOMGD', gd), nomgd)
!
!     --- ON VERIFIE QUE TOUS LES CHAMPS SONT IDENTIQUES
!
    do 10 i = 1, nstat
        chamn = chamno(i)
        call jeveuo(chamn//'.DESC', 'L', iadesc)
        gdi = zi(iadesc-1+1)
        numi = zi(iadesc-1+2)
        if (gdi .ne. gd) call u2mess('F', 'PREPOST2_67')
        if (numi .ne. num) call u2mess('F', 'PREPOST2_68')
        call jelira(chamn//'.VALE', 'TYPE', cval=typi)
        if (typi .ne. type) call u2mess('F', 'PREPOST2_69')
10  end do
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
        call jeveuo(nomnu(1:19)//'.NUEQ', 'L', ianueq)
        call jenonu(jexnom(nomnu(1:19)//'.LILI', '&MAILLA'), ibid)
        call jeveuo(jexnum(nomnu(1:19)//'.PRNO', ibid), 'L', iaprno)
    endif
!
!     --- NOMBRE DE NOEUDS DU MAILLAGE: NBNO
    call dismoi('F', 'NB_NO_MAILLA', nomma, 'MAILLAGE', nbno,&
                k8b, ier)
!
!     --- CREATION LISTES DES NOMS ET DES NUMEROS DES NOEUDS A IMPRIMER
    call wkvect('&&IRMAD0.NOMNOE', 'V V K8', nbno, jno)
    call wkvect('&&IRMAD0.NUMNOE', 'V V I', nbno, jnu)
    do 20 ino = 1, nbno
        call jenuno(jexnum(nomma//'.NOMNOE', ino), zk8(jno-1+ino))
        zi(jnu-1+ino) = ino
20  end do
!
    if (num .ge. 0) then
        ncmpmx = 6
        call irmad1(ifc, versio, nbno, zi(iaprno), zi(ianueq),&
                    nec, zi(iaec), ncmpmx, itype, nstat,&
                    chamno, zk8(iad), nomsym, zi(jnu))
    else
        call getres(k8b, k8b, nomcmd)
        call u2mess('E', 'PREPOST2_70')
    endif
!
! --- MENAGE
    call jedetr('&&IRMAD0.ENT_COD')
    call jedetr('&&IRMAD0.NOMNOE')
    call jedetr('&&IRMAD0.NUMNOE')
!
9999  continue
    call jedema()
end subroutine
