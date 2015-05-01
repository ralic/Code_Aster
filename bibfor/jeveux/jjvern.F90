subroutine jjvern(noml32, icre, iret)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: j-pierre.lefebvre at edf.fr
! aslint: disable=
    implicit none
#include "jeveux_private.h"
#include "asterfort/jjcren.h"
#include "asterfort/utmess.h"
    character(len=32) :: noml32
    integer :: icre, iret
!     ------------------------------------------------------------------
    character(len=24) :: nomco
    character(len=32) :: nomuti, nomos, nomoc, bl32
    common /nomcje/  nomuti , nomos , nomco , nomoc , bl32
    integer :: illici, jclass(0:255)
    common /jchaje/  illici , jclass
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
!-----------------------------------------------------------------------
    integer :: jdocu, jgenr, jorig, jrnom, jtype, k, n
!
!-----------------------------------------------------------------------
    parameter  ( n = 5 )
    common /jkatje/  jgenr(n), jtype(n), jdocu(n), jorig(n), jrnom(n)
!     ------------------------------------------------------------------
    character(len=24) :: noml24
    character(len=32) :: nom32, blan32
    character(len=8) :: nume, nome, nomatr, noml8
    data             nume     , nome     , nomatr&
     &               /'$$XNUM  ','$$XNOM  ','$$XATR  '/
! DEB ------------------------------------------------------------------
    iret = 0
    blan32='                                '
    nom32=blan32
    nomuti = noml32
    noml8 = noml32(25:32)
    noml24 = noml32(1:24)
    if (noml8 .ne. '        ') then
        if (noml8 .ne. nome .and. noml8 .ne. nume .and. noml8 .ne. nomatr) then
            call utmess('F', 'JEVEUX1_56', sk=noml8)
        endif
    endif
    if (noml24 .eq. nomos(1:24) .and. nomos(25:32) .eq. '        ') then
        iret = 1
    else if (noml24 .eq. nomco(1:24)) then
        iret = 2
    else
        nom32 = noml24
! ----- RECHERCHE DU NOM DANS LES BASES PAR JJCREN
!
        call jjcren(nom32, icre, iret)
! ----- VALIDITE DES CARACTERES COMPOSANT LE NOM
!
        if (iret .ne. 0 .and. icre .ne. 0) then
            if (index ( noml24 , '$' ) .ne. 0) then
                call utmess('F', 'JEVEUX1_57', sk=noml24)
            endif
            do 10 k = 1, 32
                if (jclass(ichar( nom32(k:k) )) .eq. illici) then
                    call utmess('F', 'JEVEUX1_58', sk=nom32(k:k))
                endif
10          continue
        endif
        if (noml8 .ne. '        ') then
            if (iret .eq. 1) then
                if (genr(jgenr(iclaos)+idatos) .ne. 'N') then
                    call utmess('F', 'JEVEUX1_68', sk=noml24)
                endif
            endif
        endif
    endif
! FIN ------------------------------------------------------------------
end subroutine
