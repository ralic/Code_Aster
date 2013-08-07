subroutine fointn(ipif, nomf, rvar, inume, epsi,&
                  resu, ier)
    implicit none
#include "jeveux.h"
!
#include "asterfort/focoli.h"
#include "asterfort/folocx.h"
#include "asterfort/fopro1.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    integer :: ipif, inume, ier
    real(kind=8) :: rvar, epsi, resu
    character(len=*) :: nomf
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
!     INTERPOLATION DANS LES NAPPES
!     ------------------------------------------------------------------
! IN  : IPIF   : ADRESSE DANS LE MATERIAU CODE DE LA NAPPE
!                = 0 , NAPPE
! IN  : NOMF   : NOM DE LA NAPPE SI IPIF=0
! IN  : RVAR   : VALEUR DE LA VARIABLE  "UTILISATEUR"
! IN  : INUME  : NUMERO DE LA FONCTION
! OUT : RESU   : RESULTAT
! OUT : IER    : CODE RETOUR
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: lprol, lvar, nbpt, jpro, nbpcum, i, ipt, lfon
    character(len=1) ::  coli
    character(len=16) :: prolgd
    character(len=19) :: nomfon
    character(len=24) :: interp, chprol, chvale
!     ------------------------------------------------------------------
    call jemarq()
    nomfon = nomf
    if (ipif .eq. 0) then
        chvale = nomfon//'.VALE'
        chprol = nomfon//'.PROL'
        call jeveuo(chprol, 'L', lprol)
        call fopro1(zk24(lprol), inume, prolgd, interp)
        call jeveuo(jexnum(chvale, inume), 'L', lvar)
        call jelira(jexnum(chvale, inume), 'LONMAX', nbpt)
    else
        jpro = zi(ipif+1)
        prolgd = zk24(jpro+6+ (2*inume))
        interp = zk24(jpro+6+ (2*inume-1))
        nbpcum = 0
        do 10 i = 1, inume - 1
            nbpcum = nbpcum + zi(zi(ipif+3)+i) - zi(zi(ipif+3)+i-1)
10      continue
        nbpt = zi(zi(ipif+3)+inume) - zi(zi(ipif+3)+inume-1)
        lvar = zi(ipif+2) + nbpcum
    endif
    nbpt = nbpt / 2
    lfon = lvar + nbpt
    ipt = 1
!
    call folocx(zr(lvar), nbpt, rvar, prolgd, ipt,&
                epsi, coli, ier)
    if (ier .ne. 0) goto 9999
    call focoli(ipt, coli, interp, zr(lvar), zr(lfon),&
                rvar, resu, ier)
    if (ier .ne. 0) goto 9999
!
9999  continue
!
    call jedema()
end subroutine
