subroutine extra1(nin, lchin, lpain, opt, nute,&
                  ligrel)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/excart.h"
#include "asterfort/exchml.h"
#include "asterfort/exchno.h"
#include "asterfort/exresl.h"
#include "asterfort/nbpara.h"
#include "asterfort/nopara.h"
#include "asterfort/u2mesk.h"
    integer :: nin, opt, nute
    character(len=*) :: lchin(*)
    character(len=8) :: lpain(*)
    character(len=19) :: ligrel
! ----------------------------------------------------------------------
!     BUT: PREPARER LES CHAMPS LOCAUX "IN"
!
! ----------------------------------------------------------------------
    character(len=16) :: option, nomte, nomtm, pheno, modeli
    common /cakk01/option,nomte,nomtm,pheno,modeli
!
    integer :: igd, nec, ncmpmx, iachin, iachlo, iichin, ianueq, lprno
    integer :: ilchlo, itypgd
    common /caii01/igd,nec,ncmpmx,iachin,iachlo,iichin,ianueq,lprno,&
     &       ilchlo,itypgd
    common /cakk02/typegd
    integer :: iaoptt, lgco, iaopmo, ilopmo, iaopno, ilopno, iaopds, iaoppa
    integer :: npario, nparin, iamloc, ilmloc, iadsgd
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
    integer :: iachii, iachik, iachix, debugr, lggrel
    common /caii04/iachii,iachik,iachix
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
    character(len=8) :: typegd
    character(len=19) :: chin
    character(len=4) :: type
    character(len=8) :: nompar
    integer :: k, iparg, imodat
    integer :: ipar, npin, iparin
    logical :: exich
!
!
! DEB-------------------------------------------------------------------
!
!
    npin=nbpara(opt,nute,'IN ')
    do 90 ipar = 1, npin
        nompar=nopara(opt,nute,'IN ',ipar)
        iparg=indik8(zk8(iaoppa),nompar,1,npario)
        iparin=indik8(lpain,nompar,1,nin)
        exich=((iparin.gt.0) .and. zl(iachix-1+iparin))
        if (.not.exich) then
            zi(iawloc-1+3*(iparg-1)+1)=-1
            zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+2)=0
            goto 90
        endif
!
        call assert(iparin.ne.0)
        chin=lchin(iparin)
        if (chin(1:1) .eq. ' ') call u2mesk('E', 'CALCULEL2_56', 1, nompar)
!
!
!
!       -- MISE A JOUR DES COMMON CAII01 ET CAKK02:
        iichin=iparin
        igd=zi(iachii-1+11*(iparin-1)+1)
        nec=zi(iachii-1+11*(iparin-1)+2)
        ncmpmx=zi(iachii-1+11*(iparin-1)+3)
        iachin=zi(iachii-1+11*(iparin-1)+5)
        ianueq=zi(iachii-1+11*(iparin-1)+10)
        lprno=zi(iachii-1+11*(iparin-1)+11)
        iparg=indik8(zk8(iaoppa),nompar,1,npario)
        iachlo=zi(iawloc-1+3*(iparg-1)+1)
        ilchlo=zi(iawloc-1+3*(iparg-1)+2)
        imodat=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+1)
        call assert((iachlo.lt.-2) .or. (iachlo.gt.0))
        call assert(ilchlo.ne.-1)
        type=zk8(iachik-1+2*(iparin-1)+1)(1:4)
        typegd=zk8(iachik-1+2*(iparin-1)+2)
        if (typegd .eq. 'R') then
            itypgd=1
        else if (typegd.eq.'C') then
            itypgd=2
        else if (typegd.eq.'I') then
            itypgd=3
        else if (typegd.eq.'K8') then
            itypgd=4
        else if (typegd.eq.'K16') then
            itypgd=5
        else if (typegd.eq.'K24') then
            itypgd=6
        else
            call assert(.false.)
        endif
!
!
!
!       1- MISE A .FALSE. DU CHAMP_LOC.EXIS
!       -----------------------------------------------------
        lggrel=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+4)
        debugr=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr-1)+5)
        do 30,k=1,lggrel
        zl(ilchlo-1+debugr-1+k)=.false.
30      continue
!
!
!       2- ON LANCE L'EXTRACTION:
!       -------------------------------------------
        if (type .eq. 'CART') call excart(imodat, iparg)
        if (type .eq. 'CHML') call exchml(imodat, iparg)
        if (type .eq. 'CHNO') call exchno(imodat, iparg)
        if (type .eq. 'RESL') call exresl(imodat, iparg, chin)
90  end do
!
!
!
end subroutine
