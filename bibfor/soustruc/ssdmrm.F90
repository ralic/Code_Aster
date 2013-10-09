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
#include "asterc/getfac.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/indiis.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/ssdmu1.h"
#include "asterfort/utlisi.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
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
    character(len=8) :: crit, nomacr, mal, nosma
    real(kind=8) :: prec, di, dj
    character(len=16) :: option
    character(len=24) :: valk(2), nognoi, nognoj
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iacoo2, iadim2, iadime, iagno, ialiii, ialiij
    integer :: ialikg, ialikm, ialini, ialinj, iamacr, iancnf, iaparr
    integer :: ibid(1), ico, iconf, ii, inoi
    integer :: inoii, inoj, inojj, iocc, ismai, ismaj, j
    integer :: jj, kk, longi, longj, n1, n2, n3
    integer :: nbexti, nbextj, nbid, nbngno, nbnore, nbnori, nbnorj
    integer :: nbsma, nbsmar, nocc
!-----------------------------------------------------------------------
    call jemarq()
    call getfac('RECO_SUPER_MAILLE', nocc)
    if (nocc .eq. 0) goto 999
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
    do iocc = 1, nocc
!
!     -- ON RECUPERE LA LISTE DES MAILLES ET LA LISTE DES GROUP_NO:
!     -------------------------------------------------------------
        call getvtx('RECO_SUPER_MAILLE', 'SUPER_MAILLE', iocc=iocc, nbval=nbsma,&
                    vect=zk8(ialikm), nbret=n1)
        call getvtx('RECO_SUPER_MAILLE', 'GROUP_NO', iocc=iocc, nbval=nbsma, vect=zk24(ialikg),&
                    nbret=n2)
        if (n1 .lt. 0) then
            call utmess('F', 'SOUSTRUC_64')
        endif
        if (n1 .ne. n2) then
            call utmess('F', 'SOUSTRUC_65')
        endif
        if (n1 .lt. 2) then
            call utmess('F', 'SOUSTRUC_66')
        endif
!
        nbsmar=n1
!
        call getvtx('RECO_SUPER_MAILLE', 'OPTION', iocc=iocc, scal=option, nbret=n1)
        if (option(1:11) .eq. 'GEOMETRIQUE') then
            call getvr8('RECO_SUPER_MAILLE', 'PRECISION', iocc=iocc, scal=prec, nbret=n1)
            call getvtx('RECO_SUPER_MAILLE', 'CRITERE', iocc=iocc, scal=crit, nbret=n1)
        endif
!
        do i = 1, nbsmar
            nosma= zk8(ialikm-1+i)
            nognoi= zk24(ialikg-1+i)
            call jenonu(jexnom(mag//'.SUPMAIL', nosma), ismai)
            di=zr(iaparr-1+14*(ismai-1)+13)
            nomacr= zk8(iamacr-1+ismai)
            call dismoi('NOM_MAILLA', nomacr, 'MACR_ELEM_STAT', repk=mal)
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
            do j = i+1, nbsmar
                nosma= zk8(ialikm-1+j)
                nognoj= zk24(ialikg-1+j)
                call jenonu(jexnom(mag//'.SUPMAIL', nosma), ismaj)
                dj=zr(iaparr-1+14*(ismaj-1)+13)
                nomacr= zk8(iamacr-1+ismaj)
                call dismoi('NOM_MAILLA', nomacr, 'MACR_ELEM_STAT', repk=mal)
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
                    call utmess('A', 'SOUSTRUC_67', nk=2, valk=valk)
                endif
                nbnore= min(nbnori,nbnorj)
!
!
                if (option(1:13) .eq. 'NOEUD_A_NOEUD') then
!           ---------------------------------
                    do ii = 1, nbnore
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
                    end do
!
!
                else if (option(1:7).eq.'INVERSE') then
!           -----------------------------------
                    do ii = 1, nbnore
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
                    end do
!
!
                else if (option(1:11).eq.'GEOMETRIQUE') then
!           --------------------------------------
                    ico=0
                    do ii = 1, nbnore
                        inoii=indiis(zi(ialini),zi(ialiii-1+ii),1,&
                        nbexti)
                        inoi= zi(iadim2-1+4*(ismai-1)+3)+inoii
                        do jj = 1, nbnore
                            inojj=indiis(zi(ialinj),zi(ialiij-1+jj),1,&
                            nbextj)
                            inoj= zi(iadim2-1+4*(ismaj-1)+3)+inojj
                            call ssdmu1(dj, crit, prec, zr(iacoo2+3*( inoi-1)),&
                                        zr(iacoo2+3*(inoj-1)), iconf)
                            if (iconf .eq. 0) then
                                if (inoi .lt. inoj) then
                                    zi(iancnf-1+inoj)=inoi
                                else
                                    zi(iancnf-1+inoi)=inoj
                                endif
                                ico=ico+1
                                goto 63
                            endif
                        end do
 63                     continue
                    end do
!
                    if (nbnore .ne. ico) then
                        valk(1) = nognoi
                        valk(2) = nognoj
                        call utmess('A', 'SOUSTRUC_68', nk=2, valk=valk)
                    endif
!
                endif
!
            end do
        end do
!
    end do
!
!
999 continue
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
