subroutine excart(imodat, iparg)
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
#include "asterfort/assert.h"
#include "asterfort/excar2.h"
#include "asterfort/jacopo.h"
#include "asterfort/jeexin.h"
#include "asterfort/jeveuo.h"
    integer :: imodat, iparg
! ----------------------------------------------------------------------
!     ENTREES:
!       IGR   : NUMERO DU GREL A TRAITER (COMMON)
!       IMODAT : INDICE DANS LA COLLECTION MODELOC
! ----------------------------------------------------------------------
    integer :: igd, nec, ncmpmx, iachin, iachlo, iichin, ianueq, lprno
    integer :: ilchlo, itypgd
    common /caii01/igd,nec,ncmpmx,iachin,iachlo,iichin,ianueq,lprno,&
     &       ilchlo,itypgd
    common /cakk02/typegd
    character(len=8) :: typegd
    integer :: iaoptt, lgco, iaopmo, ilopmo, iaopno, ilopno, iaopds
    integer :: iaoppa, npario, nparin, iamloc, ilmloc, iadsgd
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
    integer :: iamaco, ilmaco, iamsco, ilmsco, ialiel, illiel
    common /caii03/iamaco,ilmaco,iamsco,ilmsco,ialiel,illiel
    integer :: iachii, iachik, iachix
    common /caii04/iachii,iachik,iachix
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
    integer :: iel
    common /caii08/iel
!
    integer :: desc, modloc, dec1, dec2, lgcata, iret
    integer :: ipt, ityplo, jparal
    integer :: nbpoin, ncmp, ngrmx, debugr
    aster_logical :: lparal
!
! DEB-------------------------------------------------------------------
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
!     RECUPERATION DE LA CARTE:
!     -------------------------
    desc=zi(iachii-1+11*(iichin-1)+4)
    ngrmx=zi(desc-1+2)
    modloc=iamloc-1+zi(ilmloc-1+imodat)
    ityplo=zi(modloc-1+1)
    nbpoin=zi(modloc-1+4)
    lgcata=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+2)
    debugr=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+5)
!
!
!     1-  CAS: CART -> ELNO (OU ELGA) : "EXPAND"
!     -------------------------------------------
    if ((ityplo.eq.2) .or. (ityplo.eq.3)) then
!
        ASSERT((ityplo.ne.2) .or. (nbpoin.le.10000))
        ncmp=lgcata/nbpoin
        call excar2(ngrmx, desc, zi(modloc-1+5), ncmp, debugr)
!       ON DUPPLIQUE LES VALEURS PAR LA FIN POUR NE PAS
!       LES ECRASER :
        do 20 iel = nbelgr, 1, -1
            if (lparal) then
                if (.not.zl(jparal-1+iel)) goto 20
            endif
            do 10 ipt = nbpoin, 1, -1
                dec1=debugr-1+(iel-1)*ncmp
                dec2=debugr-1+(iel-1)*ncmp*nbpoin+ncmp*(ipt-1)
                call jacopo(ncmp, typegd, iachlo+dec1, iachlo+dec2)
                call jacopo(ncmp, 'L', ilchlo+dec1, ilchlo+dec2)
 10         continue
 20     continue
!
!     2-  CAS: CART -> ASSE :
!     -----------------------
    else if (ityplo.ge.4) then
        ASSERT(.false.)
!
!
!     3-  CAS: CART -> ELEM :
!     -----------------------
    else if (ityplo.eq.1) then
        ASSERT(nbpoin.eq.1)
        ncmp=lgcata
        call excar2(ngrmx, desc, zi(modloc-1+5), ncmp, debugr)
!
!
    endif
!
end subroutine
