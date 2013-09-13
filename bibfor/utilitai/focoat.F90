subroutine focoat(nomfon, nbfon, nopara, noresu, interp,&
                  prolgd)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
!
    integer :: nbfon
    character(len=*) :: nomfon(*)
    character(len=16) :: nopara, noresu, interp, prolgd
!     ----------------------------------------------------------------
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
!     POUR LA COMBINAISON LINEAIRE DE FONCTION,
!     DETERMINE LES ATTRIBUTS DE LA FONCTION EN SORTIE
!     ----------------------------------------------------------------
!
    integer :: iocc, lpro
    character(len=1) :: prolg, prold
    character(len=24) :: prol
    logical :: prolge, prolde, prolgc, proldc, prolgl, proldl
    logical :: intnon, intlin, intlil, intlog, intlol
!     ----------------------------------------------------------------
!
    call jemarq()
    prol(20:24) = '.PROL'
!
    prol(1:19) = nomfon(1)
    call jeveuo(prol, 'L', lpro)
    nopara = zk24(lpro+2)
    noresu = zk24(lpro+3)
!
    intnon = .false.
    intlin = .false.
    intlil = .false.
    intlog = .false.
    intlol = .false.
    do 10 iocc = 1, nbfon
        prol(1:19) = nomfon(iocc)
        call jeveuo(prol, 'L', lpro)
        if (zk24(lpro+1)(1:3) .eq. 'NON') then
            intnon = .true.
        else if (zk24(lpro+1)(1:3) .eq. 'INT') then
            intnon = .true.
        else if (zk24(lpro+1) .eq. 'LIN LIN ') then
            intlin = .true.
        else if (zk24(lpro+1) .eq. 'LIN LOG ') then
            intlil = .true.
        else if (zk24(lpro+1) .eq. 'LOG LOG ') then
            intlog = .true.
        else if (zk24(lpro+1) .eq. 'LOG LIN ') then
            intlol = .true.
        endif
10  end do
!
    if (intnon) then
        interp = 'NON NON '
        elseif ( intlin .and. ( .not.intlil .and. .not.intlog .and.&
    .not.intlol ) ) then
        interp = 'LIN LIN '
        elseif ( intlil .and. ( .not.intlin .and. .not.intlog .and.&
    .not.intlol ) ) then
        interp = 'LIN LOG '
        elseif ( intlog .and. ( .not.intlin .and. .not.intlil .and.&
    .not.intlol ) ) then
        interp = 'LOG LOG '
        elseif ( intlol .and. ( .not.intlin .and. .not.intlil .and.&
    .not.intlog ) ) then
        interp = 'LOG LIN '
    else
        call utmess('A', 'UTILITAI_83')
        interp = 'LIN LIN '
    endif
!
    prolge = .false.
    prolde = .false.
    prolgc = .false.
    proldc = .false.
    prolgl = .false.
    proldl = .false.
    do 20 iocc = 1, nbfon
        prol(1:19) = nomfon(iocc)
        call jeveuo(prol, 'L', lpro)
        if (zk24(lpro+4)(1:1) .eq. 'E') then
            prolge = .true.
        else if (zk24(lpro+4)(1:1) .eq. 'C') then
            prolgc = .true.
        else if (zk24(lpro+4)(1:1) .eq. 'L') then
            prolgl = .true.
        endif
        if (zk24(lpro+4)(2:2) .eq. 'E') then
            prolde = .true.
        else if (zk24(lpro+4)(2:2) .eq. 'C') then
            proldc = .true.
        else if (zk24(lpro+4)(2:2) .eq. 'L') then
            proldl = .true.
        endif
20  end do
!
    if (prolge) then
        prolg = 'E'
    else if (prolgc) then
        prolg = 'C'
    else if (prolgl) then
        prolg = 'L'
    else
        prolg = 'E'
    endif
!
    if (prolde) then
        prold = 'E'
    else if (proldc) then
        prold = 'C'
    else if (proldl) then
        prold = 'L'
    else
        prold = 'E'
    endif
!
    prolgd = prolg//prold//'      '
!
    call jedema()
end subroutine
