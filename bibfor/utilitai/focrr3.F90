subroutine focrr3(nomfon, resu, nopara, base, ier)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/rsadpa.h"
#include "asterc/r8vide.h"
#include "asterfort/rsnopa.h"
#include "asterfort/rsutn1.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: ier
    character(len=1) :: base
    character(len=16) :: nopara
    character(len=19) :: nomfon, resu
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
!     RECUPERATION D'UNE FONCTION DANS UNE STRUCTURE "RESULTAT"
!                                 PARAMETRE = F(VARIABLE D'ACCES)
!     ------------------------------------------------------------------
! VAR : NOMFON : NOM DE LA FONCTION
! IN  : RESU   : NOM DE LA STRUCTURE RESULTAT
! IN  : NOPARA : NOM DU PARAMETRE
! IN  : BASE   : BASE OU L'ON CREE LA FONCTION
! OUT : IER    : CODE RETOUR, = 0 : OK
!     ------------------------------------------------------------------
    integer :: nbordr, iret, kordr, lpro, lfon, lvar, iordr,  nbacc, nbpar
    integer :: iad1, iad2, nbpt
    real(kind=8) :: rundf
    character(len=8) :: type
    character(len=16) :: nomacc
    character(len=19) :: knume
    character(len=16), pointer :: acces(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
    ier = 0
    knume = '&&FOCRR3.NUME_ORDR'
    rundf = r8vide()
!
!     --- RECUPERATION DES NUME_ORDRE FOURNIS PAR L'UTILISATEUR ---
!
    call rsutn1(resu, nopara, ' ', 1, knume,&
                nbordr)
    call jeveuo(knume, 'L', kordr)
!
!     --- RECUPERATION DE LA VARIABLE D'ACCES ---
!
    call rsnopa(resu, 0, '&&FOCRR3.VAR.ACCES', nbacc, nbpar)
    call jeexin('&&FOCRR3.VAR.ACCES', iret)
    if (iret .gt. 0) then
        call jeveuo('&&FOCRR3.VAR.ACCES', 'L', vk16=acces)
        nomacc = acces(1)
    else
        call utmess('F', 'UTILITAI2_4')
    endif
    call jedetr('&&FOCRR3.VAR.ACCES')
!
!     --- CREATION DE LA FONCTION SORTIE ---
!
!     --- REMPLISSAGE DU .PROL ---
    ASSERT(lxlgut(nomfon).le.24)
    call wkvect(nomfon//'.PROL', base//' V K24', 6, lpro)
    zk24(lpro ) = 'FONCTION'
    zk24(lpro+1) = 'NON NON '
    zk24(lpro+2) = nomacc(1:8)
    zk24(lpro+3) = nopara(1:8)
    zk24(lpro+4) = 'EE      '
    zk24(lpro+5) = nomfon


!   -- calcul du nombre de points de la fonction :
    nbpt=0
    do iordr = 1, nbordr
        call rsadpa(resu, 'L', 1, nopara, zi(kordr+iordr-1),&
                    1, sjv=iad2, styp=type, istop=0)
        if (type(1:1) .ne. 'R') call utmess('F', 'UTILITAI2_6')

        if (zr(iad2).eq.rundf) cycle
        nbpt=nbpt+1
    end do


!     --- REMPLISSAGE DU .VALE ---
    call wkvect(nomfon//'.VALE', base//' V R', 2*nbpt, lvar)
    lfon = lvar + nbpt

    nbpt=0
    do 20 iordr = 1, nbordr
        call rsadpa(resu, 'L', 1, nomacc, zi(kordr+iordr-1),&
                    1, sjv=iad1, styp=type)
        if (type(1:1) .ne. 'R') then
            call utmess('F', 'UTILITAI2_5')
        endif

        call rsadpa(resu, 'L', 1, nopara, zi(kordr+iordr-1),&
                    1, sjv=iad2, styp=type, istop=0)
        if (type(1:1) .ne. 'R') then
            call utmess('F', 'UTILITAI2_6')
        endif

        if (zr(iad2).eq.rundf) cycle

        nbpt=nbpt+1
        zr(lvar+nbpt-1) = zr(iad1)
        zr(lfon+nbpt-1) = zr(iad2)

20  end do

    call jedetr(knume)

    call jedema()
end subroutine
