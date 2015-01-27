subroutine rsvpar(nomsd, iordr, nompar, ipar, rpar,&
                  kpar, ier)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsnopa.h"
    integer :: iordr, ipar, ier
    real(kind=8) :: rpar
    character(len=*) :: nomsd, nompar, kpar
! ----------------------------------------------------------------------
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
!      VERIFICATION DE L'EXISTANCE D'UN NOM DE PARAMETRE ET DE
!      SA VALEUR DANS UN RESULTAT COMPOSE
! ----------------------------------------------------------------------
! IN  : NOMSD  : NOM DE LA STRUCTURE "RESULTAT".
! IN  : IORDR  : NUMERO D'ORDRE A TRAITER.
! IN  : NOMPAR : NOM DU PARAMETRE A VERIFIER.
! IN  : IPAR   : VALEUR DU PARAMETRE ( TYPE INTEGER )
! IN  : RPAR   : VALEUR DU PARAMETRE ( TYPE REAL )
! IN  : KPAR   : VALEUR DU PARAMETRE ( TYPE CHARACTER )
! OUT : IER    : = 0    CE N'EST PAS UN PARAMETRE DU "RESULTAT".
!              : = 110  LA VALEUR DU PARAMETRE N'EST PAS CORRECTE.
!              : = 100  LA VALEUR DU PARAMETRE EST CORRECTE.
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
    integer :: ipa, nbpar, nbacc
    character(len=3) :: ctype
!
!-----------------------------------------------------------------------
    integer :: jadr
    character(len=16), pointer :: noms_para(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    ier = 0
!
    call rsnopa(nomsd, 1, '&&RSVPAR.NOMS_PARA', nbacc, nbpar)
    call jeveuo('&&RSVPAR.NOMS_PARA', 'L', vk16=noms_para)
    do ipa = 1, nbpar
        if (nompar .eq. noms_para(ipa)) then
            goto 12
        endif
    end do
    goto 999
!
12  continue
    ier = 110
    call rsadpa(nomsd, 'L', 1, nompar, iordr,&
                1, sjv=jadr, styp=ctype, istop=0)
    if (ctype(1:1) .eq. 'I') then
        if (zi(jadr) .eq. ipar) ier = 100
    else if (ctype(1:1).eq.'R') then
        if (zr(jadr) .eq. rpar) ier = 100
    else if (ctype(1:3).eq.'K80') then
        if (zk80(jadr) .eq. kpar) ier = 100
    else if (ctype(1:3).eq.'K32') then
        if (zk32(jadr) .eq. kpar) ier = 100
    else if (ctype(1:3).eq.'K24') then
        if (zk24(jadr) .eq. kpar) ier = 100
    else if (ctype(1:3).eq.'K16') then
        if (zk16(jadr) .eq. kpar) ier = 100
    else if (ctype(1:2).eq.'K8') then
        if (zk8(jadr) .eq. kpar) ier = 100
    endif
!
999 continue
    call jedetr('&&RSVPAR.NOMS_PARA')
    call jedema()
end subroutine
