subroutine editgd(chinz, ncmp, gd, nedit, dg)
! aslint: disable=
    implicit none
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
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterc/indik8.h"
#include "asterfort/jacopo.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    integer :: dg(*), ncmp, gd, nedit
    character(len=*) :: chinz
! ----------------------------------------------------------------------
!     ENTREES:
!     CHINZ : NOM DE LA CARTE A METTRE A JOUR
!     NCMP  : NOMBRE DE CMP A STOCKER
!     GD  : GRANDEUR
!     NEDIT : NUMERO DE LA GRANDEUR EDITEE
!
!     SORTIES:
!     DG  : DESCRIPTEUR_GRANDEUR A METTRE A JOUR
!
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
    integer :: ior
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: i, j, iec, reste, code, ncmpmx, deb2, debgd
    integer :: nocmp, wnocmp, lshift
    character(len=8) :: nomcmp, ctype
    character(len=24) :: valk
    character(len=1) :: k1bid
    character(len=19) :: chin
!
!
!-----------------------------------------------------------------------
    integer :: iad1, iad2, ibid, ico, indgd, ncmp2
!-----------------------------------------------------------------------
    call jemarq()
    chin=chinz
    call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', ncmpmx, k1bid)
    debgd = (nedit-1)*ncmpmx
    call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', nocmp)
    call jeveuo(chin//'.NCMP', 'L', wnocmp)
!
!     -- ON COMPTE LE NOMBRE DE CMPS A NOTER REELLEMENT :
    ncmp2=0
    do 103,i=1,ncmp
    if (zk8(wnocmp-1+i)(1:1) .ne. ' ') ncmp2=ncmp2+1
    103 end do
!
    indgd = 0
    ico=0
    do 100 i = 1, ncmpmx
        nomcmp = zk8(nocmp-1+i)
        j = indik8(zk8(wnocmp),nomcmp,1,ncmp)
        if (j .ne. 0) then
            ico=ico+1
            indgd = indgd + 1
            iec = (i-1)/30 + 1
            reste = i - 30* (iec-1)
            code = lshift(1,reste)
            dg(iec) = ior(dg(iec),code)
            deb2 = debgd + indgd
            call jeveuo(chin//'.VALV', 'L', iad1)
            call jelira(chin//'.VALV', 'TYPELONG', ibid, ctype)
            call jeveuo(chin//'.VALE', 'E', iad2)
            call jacopo(1, ctype, iad1+j-1, iad2+deb2-1)
        endif
100  end do
!
    if (ico .ne. ncmp2) then
        call u2mess('F+', 'CALCULEL6_68')
        do 101 i = 1, ncmpmx
            nomcmp = zk8(nocmp-1+i)
            valk = nomcmp
            call u2mesk('F+', 'CALCULEL6_69', 1, valk)
101      continue
        call u2mess('F+', 'CALCULEL6_70')
        do 102 i = 1, ncmp
            nomcmp = zk8(wnocmp-1+i)
            valk = nomcmp
            call u2mesk('F+', 'CALCULEL6_71', 1, valk)
102      continue
        call u2mess('F', 'VIDE_1')
    endif
!
    call jedema()
end subroutine
