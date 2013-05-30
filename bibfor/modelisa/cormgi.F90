subroutine cormgi(basez, ligrez)
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jedup1.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: basez, ligrez
    character(len=1) :: base
    character(len=19) :: ligrel
!
!**********************************************************************
!
!   OPERATION REALISEE
!   ------------------
!     CREATION DE L' OBJET .REPE DANS LA SD DE LIGREL
!
!     LIGREL.REPE : OJB V I LONG=2*NB_MAILLE(MAILLAGE)
!        V(2*(I-1)+1) --> NUMERO DU GREL CONTENANT LA MAILLE NUMERO I
!        V(2*(I-1)+2) --> NUMERO LOCALE DE CETTE MAILLE DANS LE GREL
!
!     REMARQUE : L'OBJET .REPE N'EST CREE QUE SI IL EXISTE DES MAILLES
!     DU MAILLAGE PORTANT UN ELEMENT FINI DANS LE LIGREL (.LIEL)
!
!**********************************************************************
!
    character(len=8) :: nmaila
    integer :: i, j, pt, nbmail, nbgrel, jlgrf, iret
    integer :: jrepe, jgrel, nbmgre
    logical :: exima
!
!
!
    character(len=19) :: ligtmp
    character(len=1) :: k1bid
!
    call jemarq()
    base = basez
    ligrel = ligrez
    ligtmp = '&&CORMGI.LIGREL'
    call jedetr(ligrel//'.REPE')
!
!
    call jeveuo(ligrel//'.LGRF', 'L', jlgrf)
    nmaila = zk8(jlgrf)
    call assert(nmaila.ne.' ')
!
    call jeexin(nmaila//'.CONNEX', iret)
    if (iret .eq. 0) goto 9999
!
    call jelira(nmaila//'.CONNEX', 'NMAXOC', nbmail, k1bid)
    call jelira(ligrel//'.LIEL', 'NUTIOC', nbgrel, k1bid)
    if (nbmail*nbgrel .eq. 0) goto 9999
!
!
    call wkvect(ligtmp//'.REPE', 'V V I', 2*nbmail, jrepe)
!
    exima=.false.
    do 100,i = 1,nbgrel,1
    call jelira(jexnum(ligrel//'.LIEL', i), 'LONMAX', nbmgre, k1bid)
    call jeveuo(jexnum(ligrel//'.LIEL', i), 'L', jgrel)
!
    do 110,j = 1,nbmgre - 1,1
    if (zi(jgrel+j-1) .gt. 0) then
        exima=.true.
        pt = 2* (zi(jgrel+j-1)-1) + 1
        zi(jrepe+pt-1) = i
        zi(jrepe+pt) = j
    endif
110  continue
    100 end do
!
!     -- .REPE N'EXISTE VRAIMENT QUE SI DES MAILLES DU MAILLAGE
!         SONT AFFECTEES
    if (exima) call jedup1(ligtmp//'.REPE', base, ligrel//'.REPE')
!
!
9999  continue
    call jedetr(ligtmp//'.REPE')
!
    call jedema()
end subroutine
