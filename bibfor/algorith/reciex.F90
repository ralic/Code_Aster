subroutine reciex(intexc, iderex, nindex, nnoeex, ncmpex,&
                  nvasex, graexc, excmod, napexc)
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
!***********************************************************************
!    C. DUVAL
!-----------------------------------------------------------------------
!  BUT: RECUPERER LES INFORMATIONS DE TYPE EXCITATION POUR
    implicit none
!        LE CALCUL DYNAMIQUE ALEATOIRE
!
! INTEXC   /OUT/: NOM DE L INTERSPECTRE  EXCITATION
! IDEREX   /OUT/: ORDRE DE DERIVATION
! NINDEX   /OUT/: NOMBRE  D INDICES RECUPERES
! NNOEEX   /OUT/: NOMBRE DE NOEUDS DONNES EN EXCITATION
! NCMPEX   /OUT/: NOMBRE DE CMP DONNES EN EXCITATION
! NVASEX   /OUT/: NOMBRE DE VECTEURS ASSEMBLES DONNES EN EXCITATION
! GRAEXC  /OUT/ : GRANDEUR EXCITATION
! EXCMOD  /OUT/ : TYPE MODAL
! NAPEXC  /OUT/ : NOMBRE D APPUI EXCITATION (NOEUDS OU VECTASS)
!
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
!
!-----------------------------------------------------------------------
    integer :: i1, i2, ibid1, iderex, ij2, ilcmpi
    integer :: ilcmpj, ilcpex, ilfex, ilindi, ilindj, illex, ilnoex
    integer :: ilvaex, ivite, napexc, ncmpex
    integer :: ndim, nindex, nnoeex, nvasex
!-----------------------------------------------------------------------
    integer :: ibid, iret
    character(len=4) :: excmod
    character(len=8) ::  intexc
    character(len=16) :: graexc
    character(len=24) :: chnumi, chnumj, chnoei, chnoej, chcmpi, chcmpj, chvale
    character(len=24) :: chfreq
!
    logical :: lindi, exiind
    integer ::  lnumi, lnumj, mxval, num, lcmpi, lcmpj
    integer :: nbfreq, ifreq
!
    call getvid('EXCIT', 'INTE_SPEC', iocc=1, scal=intexc, nbret=ibid)
!
    call getvis('EXCIT', 'DERIVATION', iocc=1, scal=iderex, nbret=ibid)
!
    call getvis('EXCIT', 'NUME_ORDRE_I', iocc=1, nbval=0, nbret=nindex)
    if (nindex .ne. 0) then
        lindi = .true.
        nindex = -nindex
        call wkvect('&&RECIEX.INDI_I', 'V V I', nindex, ilindi)
        call wkvect('&&RECIEX.INDI_J', 'V V I', nindex, ilindj)
        call getvis('EXCIT', 'NUME_ORDRE_I', iocc=1, nbval=nindex, vect=zi(ilindi),&
                    nbret=ibid)
        call getvis('EXCIT', 'NUME_ORDRE_J', iocc=1, nbval=nindex, vect=zi(ilindj),&
                    nbret=ibid)
    else
        call getvtx('EXCIT', 'NOEUD_I', iocc=1, nbval=0, nbret=nindex)
        lindi = .false.
        nindex = -nindex
        call wkvect('&&RECIEX.INDI_I', 'V V K8', nindex, ilindi)
        call wkvect('&&RECIEX.INDI_J', 'V V K8', nindex, ilindj)
        call wkvect('&&RECIEX.CMP_I', 'V V K8', nindex, ilcmpi)
        call wkvect('&&RECIEX.CMP_J', 'V V K8', nindex, ilcmpj)
        call getvtx('EXCIT', 'NOEUD_I', iocc=1, nbval=nindex, vect=zk8(ilindi),&
                    nbret=ibid)
        call getvtx('EXCIT', 'NOEUD_J', iocc=1, nbval=nindex, vect=zk8(ilindj),&
                    nbret=ibid)
        call getvtx('EXCIT', 'NOM_CMP_I', iocc=1, nbval=nindex, vect=zk8(ilcmpi),&
                    nbret=ibid)
        call getvtx('EXCIT', 'NOM_CMP_J', iocc=1, nbval=nindex, vect=zk8(ilcmpj),&
                    nbret=ibid)
    endif
    call getvis('EXCIT', 'NUME_VITE_FLUI', iocc=1, scal=ivite, nbret=ibid)
!
    ndim = nindex * ( nindex + 1 ) / 2
    call wkvect('&&OP0131.LIADRFEX1', 'V V I', ndim, ilfex)
    call wkvect('&&OP0131.LIADRLEX1', 'V V I', ndim+2, illex)
!
    chfreq = intexc//'.FREQ'
    call jelira(chfreq, 'LONMAX', nbfreq)
    call jeveuo(chfreq, 'L', ifreq)
    zi(illex) = nbfreq
    zi(illex+ndim+1) = ifreq
    chvale = intexc//'.VALE'
!
!     VERIFICATIONS EXISTENCE PARAMETRES DE LA SD
    exiind = .false.
    if (lindi) then
        chnumi = intexc//'.NUMI'
        chnumj = intexc//'.NUMJ'
        call jeveuo(chnumi, 'L', lnumi)
        call jeveuo(chnumj, 'L', lnumj)
        call jelira(chnumi, 'LONMAX', mxval)
        do 103 i1 = 1, nindex
            do 108 i2 = i1, nindex
                ij2 = (i2*(i2-1))/2+i1
                do 111 num = 1, mxval
                    if ((zi(lnumi-1+num) .eq. zi(ilindi-1+i1)) .and.&
                        (zi(lnumj-1+num) .eq. zi(ilindj-1+i2))) then
                        exiind = .true.
                        call jeveuo(jexnum(chvale, num), 'L', zi(ilfex-1+ ij2))
                        call jelira(jexnum(chvale, num), 'LONMAX', zi( illex+ij2))
                    endif
111              continue
108          continue
103      continue
    else
        chnoei = intexc//'.NOEI'
        chnoej = intexc//'.NOEJ'
        chcmpi = intexc//'.CMPI'
        chcmpj = intexc//'.CMPJ'
        call jeveuo(chnoei, 'L', lnumi)
        call jeveuo(chnoej, 'L', lnumj)
        call jeveuo(chcmpi, 'L', lcmpi)
        call jeveuo(chcmpj, 'L', lcmpj)
        call jelira(chnoei, 'LONMAX', mxval)
        do 120 i1 = 1, nindex
            do 122 i2 = i1, nindex
                ij2 = (i2*(i2-1))/2+i1
                do 121 num = 1, mxval
                    if ((zk8(lnumi-1+num) .eq. zk8(ilindi-1+i1)) .and.&
                        (zk8(lnumj-1+num) .eq. zk8(ilindj-1+i2)) .and.&
                        (zk8(lcmpi-1+num) .eq. zk8(ilcmpi-1+i1)) .and.&
                        (zk8(lcmpj-1+num) .eq. zk8(ilcmpj-1+i2))) then
                        exiind = .true.
                        call jeveuo(jexnum(chvale, num), 'L', zi(ilfex-1+ ij2))
                        call jelira(jexnum(chvale, num), 'LONMAX', zi( illex+ij2))
                    endif
121              continue
122          continue
120      continue
    endif
!
    if (.not. exiind) then
        call u2mess('F', 'UTILITAI4_53')
    endif
!
!----TYPE MODAL ('NON' PAR DEFAUT)
!
    call getvtx('EXCIT', 'MODAL', iocc=1, scal=excmod, nbret=ibid)
    if (excmod .eq. 'OUI') napexc = nindex
!
!----GRANDEUR   (DEPL_R PAR DEFAUT)
!
    call getvtx('EXCIT', 'GRANDEUR', iocc=1, scal=graexc, nbret=ibid)
!
!---NOEUDS APPUIS
!
    call getvtx('EXCIT', 'NOEUD', iocc=1, nbval=0, nbret=nnoeex)
    nnoeex=-nnoeex
    if (nnoeex .ne. 0) then
        napexc = nnoeex
        call wkvect('&&OP0131.LISTENOEEXC', 'V V K8', nnoeex, ilnoex)
        call getvtx('EXCIT', 'NOEUD', iocc=1, nbval=nnoeex, vect=zk8(ilnoex),&
                    nbret=ibid)
    endif
!
!---CMP APPUIS
!
    call getvtx('EXCIT', 'NOM_CMP', iocc=1, nbval=0, nbret=ncmpex)
    ncmpex=-ncmpex
    if (ncmpex .ne. 0) then
        call wkvect('&&OP0131.LISTECMPEXC', 'V V K8', ncmpex, ilcpex)
        call getvtx('EXCIT', 'NOM_CMP', iocc=1, nbval=ncmpex, vect=zk8(ilcpex),&
                    nbret=ibid)
    endif
!
!---VECTEURS ASSEMBLES
!
    call getvid('EXCIT', 'CHAM_NO', iocc=1, nbval=0, nbret=nvasex)
    nvasex=-nvasex
    if (nvasex .ne. 0) then
        napexc = nvasex
        graexc = 'EFFO'
        call wkvect('&&OP0131.LVECTASSEXC', 'V V K8', nvasex, ilvaex)
        call getvid('EXCIT', 'CHAM_NO', iocc=1, nbval=nvasex, vect=zk8(ilvaex),&
                    nbret=ibid1)
    endif
!
    if (graexc .eq. 'EFFO') iderex = 0
!
    call jedetr('&&RECIEX.INDI_I')
    call jedetr('&&RECIEX.INDI_J')
    call jeexin('&&RECIEX.CMP_I', iret)
    if (iret .ne. 0) then
        call jedetr('&&RECIEX.CMP_I')
        call jedetr('&&RECIEX.CMP_J')
    endif
!
end subroutine
