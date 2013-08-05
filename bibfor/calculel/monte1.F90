subroutine monte1(opt, te2, nout, lchout, lpaout,&
                  igr2)
    implicit none
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
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/digde2.h"
#include "asterfort/grdeur.h"
#include "asterfort/jacopo.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/modatt.h"
#include "asterfort/nbpara.h"
#include "asterfort/nopara.h"
    integer :: opt, nout, te2, igr2
    character(len=19) :: ch19
    character(len=*) :: lchout(*)
    character(len=8) :: lpaout(*)
! ----------------------------------------------------------------------
!     ENTREES:
!     IGR2   : NUMERO DU GREL DONT ON SAUVE LES CHAMPS LOCAUX
!
!     SORTIES:
!     MET A JOUR LES CHAMPS GLOBAUX DE SORTIE DE L OPTION OPT
! ----------------------------------------------------------------------
    integer :: iaoptt, lgco, iaopmo, ilopmo, iaopno, ilopno, iaopds
    integer :: iaoppa, npario, nparin, iamloc, ilmloc, iadsgd
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
    integer :: nbgr, igr, nbelgr, jcteat, lcteat, iawloc, iawlo2, iawtyp
    common /caii06/nbgr,igr,nbelgr,jcteat,lcteat,iawloc,iawlo2,iawtyp
    integer :: iachoi, iachok
    common /caii07/iachoi,iachok
    integer :: evfini, calvoi, jrepe, jptvoi, jelvoi
    common /caii19/evfini,calvoi,jrepe,jptvoi,jelvoi
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: ipar, np, mod1, jpar, gd, jparal, iret, iel, iaux1, iaux2, iaux0
    integer :: iparg, iachlo, lggrel, jcelv, jresl
    integer :: descgd, jceld, code, debugr, ncmpel, debgr2
    character(len=8) :: nompar, typsca
    logical :: lparal
!
    call jemarq()
!
!
!     PARALLELE OR NOT ?
    lparal=.false.
    call jeexin('&CALCUL.PARALLELE', iret)
    if (iret .ne. 0) then
        lparal=.true.
        call jeveuo('&CALCUL.PARALLELE', 'L', jparal)
    endif
!
!
    np=nbpara(opt,te2,'OUT')
    do 20 ipar = 1, np
        nompar=nopara(opt,te2,'OUT',ipar)
        iparg=indik8(zk8(iaoppa),nompar,1,npario)
        iachlo=zi(iawloc-1+3*(iparg-1)+1)
        if (iachlo .eq. -1) goto 20
!
        gd=grdeur(nompar)
        descgd=iadsgd+7*(gd-1)
        code=zi(descgd-1+1)
!
        mod1=modatt(opt,te2,'OUT',ipar)
        jpar=indik8(lpaout,nompar,1,nout)
        ch19=lchout(jpar)
!
!
        typsca=zk8(iawtyp-1+iparg)
        lggrel=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr2-1)+4)
        debugr=zi(iawlo2-1+5*(nbgr*(iparg-1)+igr2-1)+5)
        if (lggrel .eq. 0) goto 20
!
!
        if (code .eq. 1) then
!         -- CAS : CHAM_ELEM
            jceld=zi(iachoi-1+3*(jpar-1)+1)
            debgr2=zi(jceld-1+zi(jceld-1+4+igr2)+8)
            jcelv=zi(iachoi-1+3*(jpar-1)+2)
            call jacopo(lggrel, typsca, iachlo+debugr-1, jcelv-1+debgr2)
!
!
        else
!         -- CAS : RESUELEM
            call jeveuo(jexnum(ch19//'.RESL', igr2), 'E', jresl)
!
            if (lparal) then
!           -- POUR L'INSTANT ON N'ACCEPTE PAS EVFINI=1
                ASSERT(evfini.eq.0)
                ncmpel=digde2(mod1)
                do 10 iel = 1, nbelgr
                    if (zl(jparal-1+iel)) then
                        iaux0=(iel-1)*ncmpel
                        iaux1=iachlo+debugr-1+iaux0
                        iaux2=jresl+iaux0
                        call jacopo(ncmpel, typsca, iaux1, iaux2)
                    endif
10              continue
            else
                call jacopo(lggrel, typsca, iachlo+debugr-1, jresl)
            endif
!
            call jelibe(jexnum(ch19//'.RESL', igr2))
        endif
!
!
20  end do
!
    call jedema()
end subroutine
