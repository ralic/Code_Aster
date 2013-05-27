subroutine rschex(noresz, nomsym, codret)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/jelira.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/lxlgut.h'
    integer :: codret
    character(len=*) :: noresz, nomsym
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     ------------------------------------------------------------------
!      RESULTAT - NOMSYM EXISTE-T-IL ?
!
!     ENTREES:
!        NORESZ : NOM DU RESULTAT A EXAMINER
!        NOMSYM : NOM SYMBOLIQUE DU CHAMP
!     SORTIES:
!        CODRET : CODE D'EXISTENCE
!                 = 0 N'EXISTE PAS
!                 /= 0 EXISTE
!
!     ------------------------------------------------------------------
!
    integer :: iaux, nbtono, jordr, jtach
    character(len=1) :: k1bid
    character(len=19) :: noresu
!     ------------------------------------------------------------------
!
    codret = 0
!
    iaux = lxlgut(noresz)
!               1234567890123456789
    noresu = '                   '
    noresu(1:iaux) = noresz(1:iaux)
!
    call jelira(noresu//'.ORDR', 'LONUTI', nbtono, k1bid)
    call jeveuo(noresu//'.ORDR', 'L', jordr)
    call jenonu(jexnom(noresu//'.DESC', nomsym), iaux)
    call jeveuo(jexnum(noresu//'.TACH', iaux), 'L', jtach)
!
! --- ON PARCOURT TOUS LES NUMEROS D'ORDRE DE LA STRUCTURE RESULTAT
!     QUAND ON TROUVE UN CHAMP ENREGISTRE, ON SORT
!
    do 10 , iaux = 0 , nbtono - 1
    if (zk24(jtach+iaux) .ne. ' ') then
        codret = 7
        goto 9999
    endif
    10 end do
!
9999  continue
!
end subroutine
