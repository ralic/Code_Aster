subroutine ssdmrm(mag)
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
    implicit none
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterc/getfac.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/dismoi.h"
#include "asterfort/indiis.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/ssdmu1.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/utlisi.h"
#include "asterfort/wkvect.h"
    character(len=8) :: mag
! ----------------------------------------------------------------------
!     BUT:
!        - TRAITER LE MOT CLEF "RECO_SUPER_MAILLE"
!          DE LA COMMANDE DEFI_MAILLAGE.
!
!     IN:
!        MAG : NOM DU MAILLAGE QUE L'ON DEFINIT.
!     VAR:
!     -- MODIFICATION DE L'OBJET .NOEUD_CONF CREE DANS SSDMRC
!
! ----------------------------------------------------------------------
! INSPI SSDMRM  SSDMRG
    character(len=8) ::  crit, nomacr, mal, nosma
    real(kind=8) :: prec, di, dj
    character(len=16) :: option
    character(len=24) :: valk(2), nognoi, nognoj
    integer :: iarg
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iacoo2, iadim2, iadime, iagno, ialiii, ialiij
    integer :: ialikg, ialikm, ialini, ialinj, iamacr, iancnf, iaparr
    integer :: ibi, ibid, ico, iconf, ier, ii, inoi
    integer :: inoii, inoj, inojj, iocc, ismai, ismaj, j
    integer :: jj, kk, longi, longj, n1, n2, n3
    integer :: nbexti, nbextj, nbid, nbngno, nbnore, nbnori, nbnorj
    integer :: nbsma, nbsmar, nocc
!-----------------------------------------------------------------------
    call jemarq()
    call getfac('RECO_SUPER_MAILLE', nocc)
    if (nocc .eq. 0) goto 9999
!
!     -- ON RECUPERE CERTAINES DIMENSIONS:
!     ------------------------------------
    call jeveuo(mag//'.DIME', 'L', iadime)
    nbsma=zi(iadime-1+4)
!
    call jeveuo(mag//'.NOMACR', 'L', iamacr)
    call jeveuo(mag//'.NOEUD_CONF', 'E', iancnf)
!
    call jeveuo(mag//'.COORDO_2', 'L', iacoo2)
    call jeveuo(mag//'.DIME_2', 'L', iadim2)
    call jeveuo(mag//'.PARA_R', 'L', iaparr)
    call wkvect('&&SSDMRM.LIKM', 'V V K8', nbsma, ialikm)
    call wkvect('&&SSDMRM.LIKG', 'V V K24', nbsma, ialikg)
!
!
!     -- BOUCLE SUR LES OCCURENCES DU MOT-CLEF:
!     -----------------------------------------
    longi=0
    longj=0
    do 2, iocc=1,nocc
!
!     -- ON RECUPERE LA LISTE DES MAILLES ET LA LISTE DES GROUP_NO:
!     -------------------------------------------------------------
    call getvtx('RECO_SUPER_MAILLE', 'SUPER_MAILLE', iocc, iarg, nbsma,&
                zk8(ialikm), n1)
    call getvtx('RECO_SUPER_MAILLE', 'GROUP_NO', iocc, iarg, nbsma,&
                zk24(ialikg), n2)
    if (n1 .lt. 0) call u2mess('F', 'SOUSTRUC_64')
    if (n1 .ne. n2) call u2mess('F', 'SOUSTRUC_65')
    if (n1 .lt. 2) call u2mess('F', 'SOUSTRUC_66')
!
    nbsmar=n1
!
    call getvtx('RECO_SUPER_MAILLE', 'OPTION', iocc, iarg, 1,&
                option, n1)
    if (option(1:11) .eq. 'GEOMETRIQUE') then
        call getvr8('RECO_SUPER_MAILLE', 'PRECISION', iocc, iarg, 1,&
                    prec, n1)
        call getvtx('RECO_SUPER_MAILLE', 'CRITERE', iocc, iarg, 1,&
                    crit, n1)
    endif
!
    do 5, i=1,nbsmar
    nosma= zk8(ialikm-1+i)
    nognoi= zk24(ialikg-1+i)
    call jenonu(jexnom(mag//'.SUPMAIL', nosma), ismai)
    di=zr(iaparr-1+14*(ismai-1)+13)
    nomacr= zk8(iamacr-1+ismai)
    call dismoi('F', 'NOM_MAILLA', nomacr, 'MACR_ELEM_STAT', ibi,&
                mal, ier)
    call jeveuo(nomacr//'.LINO', 'L', ialini)
    call jelira(nomacr//'.LINO', 'LONUTI', nbexti)
    call jeveuo(jexnom(mal//'.GROUPENO', nognoi), 'L', iagno)
    call jelira(jexnom(mal//'.GROUPENO', nognoi), 'LONUTI', nbngno)
    call utlisi('INTER', zi(iagno), nbngno, zi(ialini), nbexti,&
                ibid, 0, n3)
    nbnori=-n3
    if (nbnori .gt. longi) then
        if (longi .ne. 0) call jedetr('&&SSDMRM.LIII')
!           POUR NE PAS LE DETRUIRE A CHAQUE FOIS, ON ALLOUE PLUS GRAND
        longi=(nbnori+10)*2
        call wkvect('&&SSDMRM.LIII', 'V V I', longi, ialiii)
    endif
    call utlisi('INTER', zi(iagno), nbngno, zi(ialini), nbexti,&
                zi(ialiii), nbnori, nbid)
    do 6, j=i+1,nbsmar
    nosma= zk8(ialikm-1+j)
    nognoj= zk24(ialikg-1+j)
    call jenonu(jexnom(mag//'.SUPMAIL', nosma), ismaj)
    dj=zr(iaparr-1+14*(ismaj-1)+13)
    nomacr= zk8(iamacr-1+ismaj)
    call dismoi('F', 'NOM_MAILLA', nomacr, 'MACR_ELEM_STAT', ibi,&
                mal, ier)
    call jeveuo(nomacr//'.LINO', 'L', ialinj)
    call jelira(nomacr//'.LINO', 'LONUTI', nbextj)
    call jeveuo(jexnom(mal//'.GROUPENO', nognoj), 'L', iagno)
    call jelira(jexnom(mal//'.GROUPENO', nognoj), 'LONUTI', nbngno)
    call utlisi('INTER', zi(iagno), nbngno, zi(ialinj), nbextj,&
                ibid, 0, n3)
    nbnorj=-n3
    if (nbnorj .gt. longj) then
        if (longj .ne. 0) call jedetr('&&SSDMRM.LIIJ')
        longj=(nbnorj+10)*2
        call wkvect('&&SSDMRM.LIIJ', 'V V I', longj, ialiij)
    endif
    call utlisi('INTER', zi(iagno), nbngno, zi(ialinj), nbextj,&
                zi(ialiij), nbnorj, nbid)
!
    dj=min(di,dj)
!
    if (nbnori .ne. nbnorj) then
        valk(1) = nognoi
        valk(2) = nognoj
        call u2mesk('A', 'SOUSTRUC_67', 2, valk)
    endif
    nbnore= min(nbnori,nbnorj)
!
!
    if (option(1:13) .eq. 'NOEUD_A_NOEUD') then
!           ---------------------------------
        do 61, ii=1,nbnore
        inoii=indiis(zi(ialini),zi(ialiii-1+ii),1,&
                        nbexti)
        inojj=indiis(zi(ialinj),zi(ialiij-1+ii),1,&
                        nbextj)
        inoi= zi(iadim2-1+4*(ismai-1)+3)+inoii
        inoj= zi(iadim2-1+4*(ismaj-1)+3)+inojj
        if (inoi .lt. inoj) then
            zi(iancnf-1+inoj)=inoi
        else
            zi(iancnf-1+inoi)=inoj
        endif
61      continue
!
!
    else if (option(1:7).eq.'INVERSE') then
!           -----------------------------------
        do 62, ii=1,nbnore
        if (i .eq. 1) then
            kk=nbnore+1-ii
            inoii=indiis(zi(ialini),zi(ialiii-1+kk),1,&
                            nbexti)
        else
            inoii=indiis(zi(ialini),zi(ialiii-1+ii),1,&
                            nbexti)
        endif
        inojj=indiis(zi(ialinj),zi(ialiij-1+ii),1,&
                        nbextj)
        inoi= zi(iadim2-1+4*(ismai-1)+3)+inoii
        inoj= zi(iadim2-1+4*(ismaj-1)+3)+inojj
        if (inoi .lt. inoj) then
            zi(iancnf-1+inoj)=inoi
        else
            zi(iancnf-1+inoi)=inoj
        endif
62      continue
!
!
    else if (option(1:11).eq.'GEOMETRIQUE') then
!           --------------------------------------
        ico=0
        do 63, ii=1,nbnore
        inoii=indiis(zi(ialini),zi(ialiii-1+ii),1,&
                        nbexti)
        inoi= zi(iadim2-1+4*(ismai-1)+3)+inoii
        do 631, jj=1,nbnore
        inojj=indiis(zi(ialinj),zi(ialiij-1+jj),1,&
                            nbextj)
        inoj= zi(iadim2-1+4*(ismaj-1)+3)+inojj
        call ssdmu1(dj, crit, prec, zr(iacoo2+3*( inoi-1)), zr(iacoo2+3*(inoj-1)),&
                    iconf)
        if (iconf .eq. 0) then
            if (inoi .lt. inoj) then
                zi(iancnf-1+inoj)=inoi
            else
                zi(iancnf-1+inoi)=inoj
            endif
            ico=ico+1
            goto 63
        endif
631      continue
63      continue
!
        if (nbnore .ne. ico) then
            valk(1) = nognoi
            valk(2) = nognoj
            call u2mesk('A', 'SOUSTRUC_68', 2, valk)
        endif
!
    endif
!
 6  continue
 5  continue
!
    2 end do
!
!
9999  continue
!
! --- MENAGE
!
    call jedetr('&&SSDMRM.LIKM')
    call jedetr('&&SSDMRM.LIKG')
    call jedetr('&&SSDMRM.LIII')
    call jedetr('&&SSDMRM.LIIJ')
!
    call jedema()
end subroutine
