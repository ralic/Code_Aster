subroutine aidtyp(impr)
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
! ----------------------------------------------------------------------
!    BUT:
!       ECRIRE SUR LE FICHIER "IMPR"
!       LES COUPLES (OPTION, TYPE_ELEMENT) POSSIBLES DANS LES CATALOGUES
!      (POUR VERIFIER LA COMPLETUDE)
! ----------------------------------------------------------------------
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    character(len=16) :: nophen, note, noop, nomodl
    character(len=80) :: ligne
    integer :: impr, nbtm, nbphen,  nbte, nbop, ianbop, ianbte, ianot2
    integer :: ianop2, iop, iphen, nbmodl, imodl, iamodl, itm, ite, ioptte
    integer :: iaopmo, nucalc
    integer, pointer :: optte(:) => null()
!
!
    call jemarq()
!
!
!
    ligne(1:40)= '========================================'
    ligne(41:80)='========================================'
!
    call jelira('&CATA.TM.NOMTM', 'NOMMAX', nbtm)
    call jelira('&CATA.PHENOMENE', 'NOMUTI', nbphen)
    call jeveuo('&CATA.TE.OPTTE', 'L', vi=optte)
    call jelira('&CATA.TE.NOMTE', 'NOMUTI', nbte)
    call jelira('&CATA.OP.NOMOPT', 'NOMUTI', nbop)
!
    call wkvect('&&AIDTYP.NBOP', 'V V I', nbop, ianbop)
    call wkvect('&&AIDTYP.NBTE', 'V V I', nbte, ianbte)
    call wkvect('&&AIDTYP.NOT2', 'V V K80', nbte, ianot2)
    call wkvect('&&AIDTYP.NOP2', 'V V K16', nbop, ianop2)
!
!
!     -- REMPLISSAGE DE .NOP2:
!     ------------------------
    do 7,iop=1,nbop
    call jenuno(jexnum('&CATA.OP.NOMOPT', iop), noop)
    zk16(ianop2-1+iop)=noop
    7 end do
!
!
!     -- REMPLISSAGE DE .NOT2:
!     ------------------------
    do 1,iphen=1,nbphen
    call jenuno(jexnum('&CATA.PHENOMENE', iphen), nophen)
    call jelira('&CATA.'//nophen, 'NUTIOC', nbmodl)
    do 2,imodl=1,nbmodl
    call jeveuo(jexnum('&CATA.'//nophen , imodl), 'L', iamodl)
    call jenuno(jexnum('&CATA.'//nophen(1:13)//'.MODL', imodl), nomodl)
    do 3,itm=1,nbtm
    ite= zi(iamodl-1+itm)
    if (ite .eq. 0) goto 3
    call jenuno(jexnum('&CATA.TE.NOMTE', ite), note)
    zk80(ianot2-1+ite)=nophen//' '//nomodl//' '//note
 3  continue
 2  continue
    1 end do
!
!     ON COMPLETE .NOT2 AVEC LES ELEMENTS N'APPARTENANT A AUCUNE
!        MODELISATION NI PHENOMENE:
    do 6, ite=1,nbte
    if (zk80(ianot2-1+ite)(1:1) .eq. ' ') then
        call jenuno(jexnum('&CATA.TE.NOMTE', ite), note)
        zk80(ianot2-1+ite)(35:50)=note
    endif
    6 end do
!
!
!     -- ECRITURE DES COUPLES (TE,OPT)
!     --------------------------------
    write(impr,'(A80)') ligne
    write(impr,*)' NOMBRE D''OPTION        : ', nbop
    write(impr,*)' NOMBRE DE TYPE_ELEMENT : ', nbte
    write(impr,'(A80)') ligne
    do 10,ite=1,nbte
    do 101,iop=1,nbop
    ioptte= optte(nbop*(ite-1)+iop)
    if (ioptte .eq. 0) goto 101
    call jeveuo(jexnum('&CATA.TE.OPTMOD', ioptte), 'L', iaopmo)
    nucalc= zi(iaopmo)
    if (nucalc .eq. 0) goto 101
    zi(ianbte-1+ite)=zi(ianbte-1+ite)+1
    zi(ianbop-1+iop)=zi(ianbop-1+iop)+1
    write(impr,1001) zk80(ianot2-1+ite)(1:50), zk16(ianop2-1+&
            iop),nucalc
101  continue
    10 end do
!
!
!     -- ECRITURE RESUME TYPE_ELEMENT:
!     --------------------------------
    write(impr,'(A80)') ligne
    write(impr,*)' RESUME TYPE_ELEMENTS : '
    do 20, ite=1,nbte
    write(impr,1001) zk80(ianot2-1+ite)(1:50),' NB_OPT_CALC: ',&
        zi(ianbte-1+ite)
    20 end do
!
!
!     -- ECRITURE RESUME OPTIONS:
!     ---------------------------
    write(impr,'(A80)') ligne
    write(impr,*)' RESUME OPTIONS : '
    do 30, iop=1,nbop
    write(impr,*)zk16(ianop2-1+iop),' NB_TYP_CALC: ', zi(ianbop-1+&
        iop)
    30 end do
    write(impr,'(A80)') ligne
!
!
! --- MENAGE
!
    call jedetr('&&AIDTYP.NBOP')
    call jedetr('&&AIDTYP.NBTE')
    call jedetr('&&AIDTYP.NOT2')
    call jedetr('&&AIDTYP.NOP2')
!
!
    1001 format (a50,1x,a16,1x,i5)
    call jedema()
end subroutine
