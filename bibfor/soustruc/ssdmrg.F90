subroutine ssdmrg(mag)
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
#include "asterfort/getvem.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/ssdmu1.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: mag
! ----------------------------------------------------------------------
!     BUT:
!        - TRAITER LE MOTS CLEF "RECO_GLOBAL"
!          DE LA COMMANDE DEFI_MAILLAGE.
!
!     IN:
!        MAG : NOM DU MAILLAGE QUE L'ON DEFINIT.
!     VAR:
!        --MODIFICATION DE L'OBJET .NOEUD_CONF CREE DANS SSDMRC
!
    character(len=8) :: kbid, crit
    real(kind=8) :: prec, di, dj
    integer :: iarg
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iacoo2, iadim2, iadime, ialiis, ialik8, iancnf
    integer :: iaparr, iasupi, iasupj, iconf, ii, inoi, inoj
    integer :: iocc, isma, j, jj, jsma, n1, nbnoi
    integer :: nbnoj, nbsma, nbsmar, nnnoe, nocc
!-----------------------------------------------------------------------
    call jemarq()
    call getfac('RECO_GLOBAL', nocc)
    if (nocc .eq. 0) goto 9999
!
!     -- ON RECUPERE CERTAINES DIMENSIONS:
!     ------------------------------------
    call jeveuo(mag//'.DIME', 'L', iadime)
    nbsma=zi(iadime-1+4)
    nnnoe=zi(iadime-1+1)
!
    call jeveuo(mag//'.NOEUD_CONF', 'E', iancnf)
!
    call jeveuo(mag//'.COORDO_2', 'L', iacoo2)
    call jeveuo(mag//'.DIME_2', 'L', iadim2)
    call jeveuo(mag//'.PARA_R', 'L', iaparr)
    call wkvect('&&SSDMRG.LIK8', 'V V K8', nbsma, ialik8)
    call wkvect('&&SSDMRG.LIIS', 'V V I', nbsma, ialiis)
!
!
!     -- BOUCLE SUR LES OCCURENCES DU MOT-CLEF:
!     -----------------------------------------
    do 2, iocc=1,nocc
!
!     -- ON RECUPERE LA LISTE DES MAILLES A TRAITER :
!     -----------------------------------------------
    call getvtx('RECO_GLOBAL', 'TOUT', iocc=iocc, scal=kbid, nbret=n1)
    if (n1 .eq. 1) then
        nbsmar= nbsma
        do 3, i=1,nbsmar
        zi(ialiis-1+i)=i
 3      continue
    else
        call getvem(mag, 'MAILLE', 'RECO_GLOBAL', 'SUPER_MAILLE', iocc,&
                    iarg, nbsma, zk8(ialik8), n1)
        if (n1 .lt. 0) call u2mess('F', 'SOUSTRUC_63')
        nbsmar= n1
        do 4, i=1,nbsmar
        call jenonu(jexnom(mag//'.SUPMAIL', zk8(ialik8-1+i)), isma)
        zi(ialiis-1+i)=isma
 4      continue
    endif
!
    call getvr8('RECO_GLOBAL', 'PRECISION', iocc=iocc, scal=prec, nbret=n1)
    call getvtx('RECO_GLOBAL', 'CRITERE', iocc=iocc, scal=crit, nbret=n1)
!
    do 5, i=1,nbsmar
    isma=zi(ialiis-1+i)
    call jeveuo(jexnum(mag//'.SUPMAIL', isma), 'L', iasupi)
    nbnoi=zi(iadim2-1+4*(isma-1)+1)+zi(iadim2-1+4*(isma-1)+2)
    di=zr(iaparr-1+14*(isma-1)+13)
    do 6, j=i+1,nbsmar
    jsma=zi(ialiis-1+j)
    call jeveuo(jexnum(mag//'.SUPMAIL', jsma), 'L', iasupj)
    nbnoj=zi(iadim2-1+4*(jsma-1)+1)+zi(iadim2-1+4*(jsma-1)&
                +2)
    dj=zr(iaparr-1+14*(jsma-1)+13)
    dj=min(di,dj)
    do 7, ii=1,nbnoi
    inoi=zi(iasupi-1+ii)
!               -- SI C'EST UN NOEUD DE LAGRANGE, ON SAUTE :
    if (inoi .gt. nnnoe) goto 7
    do 8, jj=1,nbnoj
    inoj=zi(iasupj-1+jj)
    if (inoj .gt. nnnoe) goto 8
    call ssdmu1(dj, crit, prec, zr(iacoo2+3*(inoi-1)), zr(iacoo2+3*(inoj-1)),&
                iconf)
    if (iconf .eq. 0) then
        if (inoi .lt. inoj) then
            zi(iancnf-1+inoj)=inoi
        else
            zi(iancnf-1+inoi)=inoj
        endif
    endif
 8  continue
 7  continue
 6  continue
 5  continue
!
    2 end do
!
!
9999  continue
! --- MENAGE
    call jedetr('&&SSDMRG.LIK8')
    call jedetr('&&SSDMRG.LIIS')
!
    call jedema()
end subroutine
