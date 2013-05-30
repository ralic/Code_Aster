subroutine ltnotb(litab, nomtab, nomsd)
    implicit   none
    include 'jeveux.h'
    include 'asterfort/gnoms2.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/juveca.h'
    include 'asterfort/u2mesk.h'
    character(len=*) :: litab, nomtab, nomsd
!     -----------------------------------------------------------------
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
!      DETERMINER LE NOM DE LA TABLE DE "LITAB" CORRESPONDANT
!      AU NOM SYMBOLIQUE "NOMTAB"
!
! IN  : LITAB  : NOM DE LA SD L_TABLE
! IN  : NOMTAB : NOM SYMBOLIQUE DE LA TABLE STOCKEE DANS "LITAB"
! OUT : NOMSD  : NOM DE LA TABLE STOCKEE (OU A STOCKER) DANS "LITAB"
!
!     SI NOMTAB N'EXISTE PAS ENCORE DANS LITAB ON AGRANDIT LITAB
!     -----------------------------------------------------------------
    character(len=24) :: noojb
    integer :: iret, nbtm, nbtu, jltnt, i, jltns
    character(len=8) :: k8b
    character(len=16) :: nomsym
    character(len=19) :: listab
    character(len=24) :: valk(2)
! DEB------------------------------------------------------------------
!
    call jemarq()
!
    listab = litab
    call jeexin(listab//'.LTNT', iret)
    if (iret .eq. 0) then
        valk(1) = nomtab
        valk(2) = litab
        call u2mesk('F', 'TABLE0_37', 2, valk)
    endif
!
    nomsym = nomtab
    call jelira(listab//'.LTNT', 'LONMAX', nbtm, k8b)
    call jelira(listab//'.LTNT', 'LONUTI', nbtu, k8b)
    call jeveuo(listab//'.LTNT', 'L', jltnt)
!
!
!     1) LE NOMSYM EXISTE DANS LISTAB :
!     ---------------------------------
    do 10 i = 1, nbtu
        if (zk16(jltnt+i-1) .eq. nomsym) then
            call jeveuo(listab//'.LTNS', 'L', jltns)
            nomsd = zk24(jltns+i-1)
            goto 9999
        endif
10  end do
!
!
!     2) LE NOMSYM N'EXISTE PAS DANS LISTAB :
!     ---------------------------------
    nbtu = nbtu + 1
    if (nbtu .gt. nbtm) then
        call juveca(listab//'.LTNT', nbtu+6)
        call juveca(listab//'.LTNS', nbtu+6)
    endif
    call jeecra(listab//'.LTNT', 'LONUTI', nbtu, ' ')
    call jeecra(listab//'.LTNS', 'LONUTI', nbtu, ' ')
!
    call jeveuo(listab//'.LTNT', 'E', jltnt)
    zk16(jltnt+nbtu-1) = nomsym
    noojb=listab(1:8)//'.TB000000  .TBBA'
    call gnoms2(noojb, 12, 17)
    call jeveuo(listab//'.LTNS', 'E', jltns)
    zk24(jltns+nbtu-1) = noojb(1:17)
    nomsd = zk24(jltns+nbtu-1)
!
9999  continue
!
    call jedema()
end subroutine
