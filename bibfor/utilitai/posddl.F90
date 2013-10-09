subroutine posddl(type, resu, noeud, cmp, nunoe,&
                  nuddl)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
!
    character(len=*) :: type, resu, noeud, cmp
    integer :: nunoe, nuddl
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     DONNE LE NUMERO DU NOEUD
!           NUNOE = 0 SI LE NOEUD N'EXISTE PAS
!     DONNE LE NUMERO DU DDL ASSOCIE AU NOEUD ET A SA COMPOSANTE
!           NUDDL = 0 SI LE COUPLE (NOEUD,COMPOSANTE) N'EXISTE PAS
!     ------------------------------------------------------------------
! IN  TYPE   : TYPE DU RESU
! IN  RESU   : NOM D'UN NUME_DDL OU D'UN CHAM_NO
! IN  NOEUD  : NOM DU NOEUD
! IN  CMP    : NOM DE LA COMPOSANTE
! OUT NUNOE  : NUMERO LOCAL DU NOEUD
! OUT NUDDL  : NUMERO DU DDL ASSOCIE AU NOEUD DE COMPOSANTE CMP
!     ------------------------------------------------------------------
    integer :: ibid, gd, iec, nec, ncmpmx, icmpre, icmp, jprno, jnueq, iad
    integer :: tabec(10)
    character(len=8) ::  nomma, nomcmp, ncmp
    character(len=19) :: prno
!     ------------------------------------------------------------------
    call jemarq()
!
    if (type(1:8) .eq. 'NUME_DDL') then
        call dismoi('NOM_MAILLA', resu, 'NUME_DDL', repk=nomma)
        call dismoi('NUM_GD_SI', resu, 'NUME_DDL', repi=gd)
        prno( 1:14) = resu
        prno(15:19) = '.NUME'
!
    else if (type(1:7) .eq. 'CHAM_NO') then
        call dismoi('NOM_MAILLA', resu, 'CHAM_NO', repk=nomma)
        call dismoi('PROF_CHNO', resu, 'CHAM_NO', repk=prno)
        call dismoi('NUM_GD', resu, 'CHAM_NO', repi=gd)
!
    else
        ASSERT(.false.)
    endif
!
    call jenonu(jexnom(nomma//'.NOMNOE', noeud), nunoe)
    if (nunoe .eq. 0) goto 9999
!
    ncmp = cmp
    nuddl = 0
!
    call jenonu(jexnom(prno//'.LILI', '&MAILLA'), ibid)
    call jeveuo(jexnum(prno//'.PRNO', ibid), 'L', jprno)
    call jeveuo(prno//'.NUEQ', 'L', jnueq)
!
    nec = nbec( gd )
    ASSERT(nec .le. 10)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', iad)
    call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', ncmpmx)
    do 10 iec = 1, nec
        tabec(iec)= zi(jprno-1+(nunoe-1)*(nec+2)+2+iec )
 10 end do
!
    icmpre = 0
    do 20 icmp = 1, ncmpmx
        if (exisdg(tabec,icmp)) then
            icmpre = icmpre + 1
            nomcmp = zk8(iad-1+icmp)
            if (nomcmp .eq. ncmp) then
                nuddl = zi(jnueq+zi(jprno+(nec+2)*(nunoe-1))-1)+ icmpre-1
                goto 22
            endif
        endif
 20 end do
 22 continue
!
9999 continue
    call jedema()
end subroutine
