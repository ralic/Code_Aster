subroutine ops005()
    implicit none
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
!
!
    include 'jeveux.h'
    include 'asterc/getres.h'
    include 'asterc/getvtx.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/wkvect.h'
    character(len=1) :: kbid
    character(len=8) :: nomres, nompar
    character(len=16) :: typres, nomcmd
    character(len=19) :: nomfon
    integer :: lprol, lnova, nk, ir
    integer :: iarg
!     ------------------------------------------------------------------
    call jemarq()
!
    call getres(nomfon, typres, nomcmd)
!
    nompar = '  '
    nomres = 'TOUTRESU'
    call wkvect(nomfon//'.PROL', 'G V K24', 6, lprol)
    zk24(lprol) = 'INTERPRE'
    zk24(lprol+1) = 'INTERPRE'
    zk24(lprol+2) = nompar
    zk24(lprol+3) = nomres
    zk24(lprol+4) = 'II      '
    zk24(lprol+5) = nomfon
    call getvtx(' ', 'NOM_PARA', 1, iarg, 1,&
                kbid, nk)
    if (nk .ne. 1) nk=-nk
    call wkvect(nomfon//'.NOVA', 'G V K8', nk, lnova)
    call getvtx(' ', 'NOM_PARA', 1, iarg, nk,&
                zk8(lnova), ir)
!
    call jedema()
end subroutine
