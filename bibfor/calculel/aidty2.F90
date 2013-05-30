subroutine aidty2(impr)
! person_in_charge: Christophe-mmn.durand at edf.fr
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!    BUT:
!       ECRIRE SUR LE FICHIER "IMPR"
!       1  LES COUPLES (OPTION, TYPE_ELEMENT) REALISES AUJOURD'HUI
!          (POUR VERIFIER LA COMPLETUDE)
! ----------------------------------------------------------------------
    include 'jeveux.h'
!
    include 'asterfort/aidtyp.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: kbid
    character(len=16) :: nomte, noop
    integer :: iaopte, nbte, nbop, ianop2, iop, ite, ioptte, iaopmo, nucalc
    integer :: impr
!
    call jemarq()
!
!     1) IMPRESSION DES LIGNES DU TYPE :
!        &&CALCUL/MECA_XT_FACE3   /CHAR_MECA_PRES_R
!     ------------------------------------------------------------------
    call jeveuo('&CATA.TE.OPTTE', 'L', iaopte)
    call jelira('&CATA.TE.NOMTE', 'NOMUTI', nbte, kbid)
    call jelira('&CATA.OP.NOMOPT', 'NOMUTI', nbop, kbid)
!
    call wkvect('&&AIDTY2.NOP2', 'V V K16', nbop, ianop2)
    do 7,iop=1,nbop
    call jenuno(jexnum('&CATA.OP.NOMOPT', iop), noop)
    zk16(ianop2-1+iop)=noop
    7 end do
!
!
!     -- ECRITURE DES COUPLES (TE,OPT)
!     --------------------------------
    do 10,ite=1,nbte
    call jenuno(jexnum('&CATA.TE.NOMTE', ite), nomte)
    do 101,iop=1,nbop
    ioptte= zi(iaopte-1+nbop*(ite-1)+iop)
    if (ioptte .eq. 0) goto 101
    call jeveuo(jexnum('&CATA.TE.OPTMOD', ioptte), 'L', iaopmo)
    nucalc= zi(iaopmo)
    if (nucalc .le. 0) goto 101
    write(impr,*)'&&CALCUL/'//nomte//'/'//zk16(ianop2-1+iop)
101  continue
    10 end do
!
!
!     2) IMPRESSION DE LIGNES PERMETTANT LE SCRIPT "USAGE_ROUTINES" :
!        A QUELLES MODELISATIONS SERVENT LES ROUTINES TE00IJ ?
!     ----------------------------------------------------------------
!  PHENOMENE  MODELISATION TYPE_ELEMENT OPTION       TE00IJ
!  MECANIQUE  D_PLAN_HMS   HM_DPQ8S     SIEQ_ELNO    330
!
    call aidtyp(impr)
!
!
    call jedema()
end subroutine
