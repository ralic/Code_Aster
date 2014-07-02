subroutine exresl(modatt, iparg, chin)
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
! person_in_charge: jacques.pellet at edf.fr
!     ARGUMENTS:
!     ----------
#include "asterf_types.h"
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/digde2.h"
#include "asterfort/jacopo.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
    integer :: modatt, iparg
    character(len=19) :: chin
! ----------------------------------------------------------------------
!     ENTREES:
!        CHIN   : NOM DU CHAMP GLOBAL SUR LEQUEL ON FAIT L'EXTRACTION
!        IGR    : NUMERO DU GROUPE_ELEMENT (COMMON)
!        MODATT : MODE LOCAL ATTENDU
! ----------------------------------------------------------------------
    integer :: igd, nec, ncmpmx, iachin, iachlo, iichin, ianueq, lprno
    integer :: ilchlo, itypgd
    common /caii01/igd,nec,ncmpmx,iachin,iachlo,iichin,ianueq,lprno,&
     &       ilchlo,itypgd
    common /cakk02/typegd
    integer :: iaoptt, lgco, iaopmo, ilopmo, iaopno, ilopno, iaopds, iaoppa
    integer :: npario, nparin, iamloc, ilmloc, iadsgd
    character(len=8) :: typegd
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
    integer :: iachii, iachik, iachix
    common /caii04/iachii,iachik,iachix
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
!
!     FONCTIONS EXTERNES:
!     ------------------
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: desc, mode, ncmpel, iret, jparal, iel, iaux1, iaux2, iaux0, k
    integer :: jresl, debugr, lggrel
    aster_logical :: lparal
!
!
    call jemarq()
!
!     PARALLELE OR NOT ?
!     -------------------------
    call jeexin('&CALCUL.PARALLELE', iret)
    if (iret .ne. 0) then
        lparal=.true.
        call jeveuo('&CALCUL.PARALLELE', 'L', jparal)
    else
        lparal=.false.
    endif
!
    lggrel=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+4)
    debugr=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+5)
!
    desc=zi(iachii-1+11*(iichin-1)+4)
!
    ASSERT(modatt.gt.0)
    mode=zi(desc-1+2+igr)
!
    if (mode .eq. 0) then
!       -- LE RESUELEM N'EXISTE PAS SUR LE GREL :
!          ON SORT SANS METTRE ZL(ILCHLO+DEBUGR-1-1+K)=.TRUE.
!          ON AURA ALORS TOUJOURS IRET=3 AVEC TECACH
        goto 9999
    endif
!
    ASSERT(mode.eq.modatt)
    ncmpel=digde2(mode)
    ASSERT(lggrel.eq.ncmpel*nbelgr)
    call jeveuo(jexnum(chin//'.RESL', igr), 'L', jresl)
    if (lparal) then
        do 10 iel = 1, nbelgr
            if (zl(jparal-1+iel)) then
                iaux0=(iel-1)*ncmpel
                iaux1=jresl+iaux0
                iaux2=iachlo+debugr-1+iaux0
                call jacopo(ncmpel, typegd, iaux1, iaux2)
            endif
 10     continue
    else
        call jacopo(lggrel, typegd, jresl, iachlo+debugr-1)
    endif
!
!
    if (lparal) then
        do 30 iel = 1, nbelgr
            if (zl(jparal-1+iel)) then
                iaux1=ilchlo+debugr-1+(iel-1)*ncmpel
                do 20 k = 1, ncmpel
                    zl(iaux1-1+k)=.true.
 20             continue
            endif
 30     continue
    else
        do 40 k = 1, lggrel
            zl(ilchlo+debugr-1-1+k)=.true.
 40     continue
    endif
!
9999 continue
    call jedema()
end subroutine
