subroutine calcfo(compl, nomfin, nomfon, nbval, vale,&
                  nopara)
    implicit   none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/fointc.h'
    include 'asterfort/fointe.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/lxlgut.h'
    include 'asterfort/wkvect.h'
    integer :: nbval
    real(kind=8) :: vale(*)
    logical :: compl
    character(len=24) :: nopara
    character(len=19) :: nomfin, nomfon
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
!
!     CREATION DU SD FONCTION A PARTIR D'UNE FORMULE (FONCTION )
!     ------------------------------------------------------------------
    integer :: ier, nbval2, lvale, lfon, ival, lprol
!     ------------------------------------------------------------------
!
    call jemarq()
!
!     --- CREATION ET REMPLISSAGE DE L'OBJET NOMFON.VALE ---
!
    if (compl) then
        nbval2 = 3*nbval
    else
        nbval2 = 2*nbval
    endif
!
    call wkvect(nomfon//'.VALE', 'G V R', nbval2, lvale)
    lfon = lvale + nbval
    do 10 ival = 0, nbval-1
        zr(lvale+ival) = vale(ival+1)
        if (compl) then
            call fointc('F', nomfin, 1, nopara, zr(lvale+ival),&
                        zr(lfon+2*ival), zr(lfon+2*ival+1), ier)
        else
            call fointe('F', nomfin, 1, nopara, zr(lvale+ival),&
                        zr( lfon+ival), ier)
        endif
10  end do
!
!     --- CREATION ET REMPLISSAGE DE L'OBJET NOMFON.PROL ---
!
    call assert(lxlgut(nomfon).le.24)
    call wkvect(nomfon//'.PROL', 'G V K24', 6, lprol)
    if (compl) then
        zk24(lprol) = 'FONCT_C         '
    else
        zk24(lprol) = 'FONCTION        '
    endif
    zk24(lprol+1) = 'LIN LIN         '
    zk24(lprol+2) = nopara
    zk24(lprol+3) = 'TOUTRESU        '
    zk24(lprol+4) = 'EE              '
    zk24(lprol+5) = nomfon
!
    call jedema()
end subroutine
