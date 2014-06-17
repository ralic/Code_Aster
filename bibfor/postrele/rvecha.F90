subroutine rvecha(dim, epsi, ssch19, nbcp, nbco,&
                  nbsp, ror, rex, ma1, for,&
                  fex, n, ptadr, val)
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
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rvchlo.h"
#include "asterfort/wkvect.h"
    character(len=19) :: ssch19
    character(len=2) :: dim
    integer :: ma1(*), for(*), fex(*), n, nbcp, ptadr
    real(kind=8) :: ror(*), rex(*), val(*), epsi
!
!***********************************************************************
!
!  OPERATION REALISEE
!  ------------------
!
!     OPERATION EXTRACTION DU POST-TRAITEMENT LE LONG D' UN SGT EN UN
!     MORCEAU
!
!  ARGUMENTS EN ENTREE
!  -------------------
!
!     EPSI   : PRECISION
!     DIM    : DIMENSION DU PROBLEME ( '1D' OU '2D' OU '3D')
!     SSCH19 : NOM DELA SD SOUS_CHAMP_GD
!     NBCP   : NOMBRE DE CMP A EXTRAIRE
!     NBCO   : NOMBRE DE COUCHES CONSIDEREES
!     NBSP   : NOMBRE DE SOUS-PT CONSIDEREES
!     ROR    : TABLEAU DE ORIGINE DES SGT ELEMENTAIRES
!     REX    : TABLEAU DES POSITIONS DES EXTREMITES SUR LES FACES
!     FOR    : TABLEAU DES FACES DONNANT LES ORIGINES
!     FEX    : TABLEAU DES FACES DONNANT LES EXTREMITES
!     MA1    : TABLEAU DES 1ERE MAILLES 2D DONNANT LES SGT ELEMENTAIRES
!     MA2    : TABLEAU DES 2IEMEMAILLES 2D DONNANT LES SGT ELEMENTAIRES
!     N      : NBR DE SGTS ELEMENTAIRES A TRAITER
!
!  ARGUMENTS EN SORTIE
!  -------------------
!
!     PTADR : (IN/OUT) POINTEUR SUR LE PREMIER ELEMENT LIBRE DE VAL
!     VAL   : TABLEAU DES VALEUR DE LA CMP (POUR TOUT LE CHAMP)
!
!***********************************************************************
!
!  -----------------------------------------
!
!
!  ---------------------------------
!
!  VARIABLES LOCALES
!  -----------------
!
    character(len=24) :: ntabf, ntabr
    character(len=4) :: docu
!
    integer :: atabf, atabr,   apnbn
    integer :: m, i, adr, nbpara, nbpt
!
!==================== CORPS DE LA ROUTINE =============================
!
!-----------------------------------------------------------------------
    integer :: nbcm, nbco, nbsm, nbsp
    integer, pointer :: pnco(:) => null()
    integer, pointer :: pnsp(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    ntabf = '&&RVECHD.NUM.FACE'
    ntabr = '&&RVECHD.VAL.PAR'
!
    call wkvect(ntabr, 'V V R', 2, atabr)
    call wkvect(ntabf, 'V V I', 2, atabf)
!
    nbpt = 2
!
    call jeveuo(ssch19//'.NOMA', 'L', adr)
    call jelira(ssch19//'.VALE', 'DOCU', cval=docu)
!
!
    if (dim .eq. '3D') then
!
        nbpara = 2
!
    else
!
        nbpara = 1
!
    endif
!
    if (docu .eq. 'CHNO') then
!
        do 100, i = 1, n, 1
!
        m = ma1(i)
        zi(atabf) = for(i)
        zr(atabr) = ror(1 + (i-1)*nbpara)
!
        call rvchlo(epsi, ssch19, nbcp, 1, 1,&
                    1, 1, m, zi(atabf), 1,&
                    zr(atabr), val(ptadr + (i-1)*nbcp))
!
100      continue
!
        m = ma1(n)
        zi(atabf) = fex(n)
        zr(atabr) = rex(1 + (n-1)*nbpara)
!
        call rvchlo(epsi, ssch19, nbcp, 1, 1,&
                    1, 1, m, zi(atabf), 1,&
                    zr(atabr), val(ptadr + n*nbcp))
!
        ptadr = ptadr + (n+1)*nbcp
!
    else
!
        call jeveuo(ssch19//'.PNCO', 'L', vi=pnco)
        call jeveuo(ssch19//'.PNSP', 'L', vi=pnsp)
        call jeveuo(ssch19//'.PNBN', 'L', apnbn)
!
        do 200, i = 1, n, 1
!
        m = ma1(i)
        zi(atabf + 1-1) = for(i)
        zi(atabf + 2-1) = fex(i)
        zr(atabr + 1-1) = ror(i)
        zr(atabr + 2-1) = rex(i)
!
        nbcm = pnco(m)
        nbsm = pnsp(m)
!
        call rvchlo(epsi, ssch19, nbcp, nbco, nbsp,&
                    nbcm, nbsm, m, zi(atabf), nbpt,&
                    zr(atabr), val(ptadr))
!
!
        ptadr = ptadr + 2*nbcp*nbco*nbsp
!
200      continue
!
    endif
!
    call jedetr(ntabr)
    call jedetr(ntabf)
!
    call jedema()
end subroutine
