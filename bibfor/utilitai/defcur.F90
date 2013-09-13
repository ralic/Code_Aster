subroutine defcur(vecr1, veck1, nb, vecr2, nv,&
                  nommai, nm, prolgd, interp)
    implicit none
! ----------------------------------------------------------------------
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
!     LECTURE DE LA DEFINITION D'UNE FONCTION (ABSCISSE CURVILIGNE)
!     STOCKAGE DANS UN OBJET DE TYPE FONCTION
! ----------------------------------------------------------------------
!     IN  : VECR1  : VECTEUR DE LONG. NB, CONTIENT LES VALEURS DE LA
!                    FONCTION DEFINIE AUX NOEUDS.
!     IN  : VECK1  : VECTEUR DE LONG. NB, CONTIENT LES NOMS DES NOEUDS.
!     OUT : VECR2  : VECTEUR DE LONG. NV, CONTIENT LES VALEURS DE LA
!                    FONCTION.
!     IN  : MONMAI : NOM DU MAILLAGE.
!     IN  : NM     : NOMBRE DE MAILLES.
#include "jeveux.h"
#include "asterfort/i2extf.h"
#include "asterfort/i2sens.h"
#include "asterfort/i2tgrm.h"
#include "asterfort/i2vois.h"
#include "asterfort/jecrec.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/prfcur.h"
#include "asterfort/utmess.h"
#include "asterfort/vefcur.h"
#include "asterfort/wkvect.h"
!
    integer :: ptch, pnoe, nb, nv
    real(kind=8) :: vecr1(nb), vecr2(nv)
    character(len=2) :: prolgd
    character(len=8) :: nommai, interp, veck1(nb)
    character(len=24) :: cooabs, nomnoe, connex, typmai
    character(len=8) :: typm
    character(len=24) :: conseg, typseg
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, iach, iacnex, iagm, iav1, iav2, iexi
    integer :: ii, im, ima1, ima2, ind, ing, ino
    integer :: iseg2, isens, itym, itypm, jgcnx, ji, jj
    integer :: jp, kk, kseg, labs, lnoe, lvali, mi
    integer :: nbchm, nbnoma, nbpoi1, nbrma, nbrma1, nbrma2
    integer :: nbrse1, nbrse2, nbrseg, nbseg2, nm, numno
!
!-----------------------------------------------------------------------
    call jemarq()
    nbrma = nm
!
!     --- CONSTRUCTION DES OBJETS DU CONCEPT MAILLAGE ---
!
    nomnoe = nommai//'.NOMNOE'
    cooabs = nommai//'.ABS_CURV  .VALE'
    connex = nommai//'.CONNEX'
    typmai = nommai//'.TYPMAIL'
!
!     --- VERIFICATION DE L EXISTENCE DE L ABSCISSE CURVILIGNE ---
!
    call jeexin(cooabs, iexi)
    if (iexi .eq. 0) then
        call utmess('F', 'UTILITAI_46')
    endif
!     --- CREATION D OBJETS TEMPORAIRES ---
!
    call wkvect('&&DEFOCU.TEMP      ', 'V V I', nbrma, iagm)
    do 60 ii = 1, nbrma
        zi(iagm+ii-1) = ii
60  end do
    nbrma2 = 2*nbrma
    nbrma1 = nbrma + 1
    call wkvect('&&DEFOCU.TEMP.VOIS1', 'V V I', nbrma, iav1)
    call wkvect('&&DEFOCU.TEMP.VOIS2', 'V V I', nbrma, iav2)
    call wkvect('&&DEFOCU.TEMP.CHM  ', 'V V I', nbrma1, ptch)
    call wkvect('&&DEFOCU.TEMP.IACHM', 'V V I', nbrma2, iach)
    call wkvect('&&DEFOCU.TEMP.LNOE', 'V V I', nbrma1, lnoe)
    call wkvect('&&DEFOCU.TEMP.PNOE', 'V V I', nv, pnoe)
    call wkvect('&&DEFOCU.TEMP.IPOI1', 'V V I', nbrma, ima1)
    call wkvect('&&DEFOCU.TEMP.ISEG2', 'V V I', nbrma, ima2)
!
!     TRI DES MAILLES POI1 ET SEG2
    nbseg2=0
    nbpoi1=0
    kseg=0
    do 12 im = 1, nbrma
        call jeveuo(typmai, 'L', itypm)
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(itypm+im-1)), typm)
        if (typm .eq. 'SEG2') then
            kseg=zi(itypm+im-1)
            nbseg2=nbseg2+1
            zi(ima2+nbseg2-1)=im
        else if (typm .eq. 'POI1') then
            nbpoi1=nbpoi1+1
            zi(ima1+nbpoi1-1)=im
        else
            call utmess('F', 'MODELISA_2')
        endif
12  end do
    conseg='&&DEFOCU.CONNEX'
    typseg='&&DEFOCU.TYPMAI'
    call wkvect(typseg, 'V V I', nbrma, itym)
    do 13 im = 1, nbrma
        zi(itym-1+im)=kseg
13  end do
!     IL FAUT CREER UNE TABLE DE CONNECTIVITE POUR LES SEG2
!
    nbnoma=2*nbseg2
    nbrseg=nbseg2
    nbrse1=nbseg2+1
    nbrse2=nbseg2*2
    call jecrec(conseg, 'V V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbseg2)
    call jeecra(conseg, 'LONT', nbnoma)
    do 14 iseg2 = 1, nbseg2
        im=zi(ima2+iseg2-1)
        call jelira(jexnum(connex, im ), 'LONMAX', nbnoma)
        call jeveuo(jexnum(connex, im ), 'L', iacnex)
        call jeecra(jexnum(conseg, iseg2), 'LONMAX', nbnoma)
        call jeveuo(jexnum(conseg, iseg2), 'E', jgcnx)
        do 3 ino = 1, nbnoma
            numno=zi(iacnex-1+ino)
            zi(jgcnx+ino-1)=numno
 3      continue
14  end do
!
    call i2vois(conseg, typseg, zi(iagm), nbrseg, zi(iav1),&
                zi(iav2))
    call i2tgrm(zi(iav1), zi(iav2), nbrseg, zi(iach), zi(ptch),&
                nbchm)
    call i2sens(zi(iach), nbrse2, zi(iagm), nbrseg, conseg,&
                typseg)
!
!     --- CREATION D UNE LISTE ORDONNEE DE NOEUDS ---
    do 10 i = 1, nbrseg
        isens = 1
        mi = zi(iach+i-1)
        if (mi .lt. 0) then
            mi = -mi
            isens = -1
        endif
        call i2extf(mi, 1, conseg, typseg, ing,&
                    ind)
        if (isens .eq. 1) then
            zi(lnoe+i-1) = ing
            zi(lnoe+i) = ind
        else
            zi(lnoe+i) = ing
            zi(lnoe+i-1) = ind
        endif
10  end do
!
!
!     --- VERIFICATION DE LA DEFINITION DE LA FONCTION ---
!
    call vefcur(zi(lnoe), nbrse1, veck1, zi(pnoe), nb,&
                nomnoe)
!
!
    call jeveuo(cooabs, 'L', labs)
    call wkvect('&&DEFOCU.TEMP.VALE', 'V V R8', nv, lvali)
!
    do 30 i = 1, nbrseg
        zr(lvali+2*(i-1)) = zr(labs+3*(i-1))
30  end do
!
    zr(lvali+2*nbrseg) = zr(labs+3*(nbrseg-1)+1)
!
    do 40 i = 1, nb
        kk = 2*(zi(pnoe+i-1)-1)+1
        zr(lvali+kk) = vecr1(i)
40  end do
!
    do 80 i = 1, nb
        jp = zi(pnoe+i-1)
        ji = i
        do 70 jj = i, nb
            if (zi(pnoe+jj-1) .lt. jp) then
                ji = jj
                jp = zi(pnoe+jj-1)
            endif
70      continue
        zi(pnoe+ji-1) = zi(pnoe+i-1)
        zi(pnoe+i-1) = jp
80  end do
!
!     ------------- INTERPOLATION DE LA FONCTION -------------
!     --- PROLONGEMENT DE LA FONCTION A GAUCHE ET A DROITE ---
!
    call prfcur(zi(pnoe), nb, zr(lvali), nv, interp,&
                prolgd)
!
!     --- REMPLISSAGE DE L OBJET .VALE ---
!
    do 50 i = 1, nbrse1
        vecr2(i) = zr(lvali+2*(i-1))
        vecr2(nbrse1+i) = zr(lvali+2*(i-1)+1)
50  end do
!
!     --- MENAGE ---
    call jedetr('&&DEFOCU.TEMP      ')
    call jedetr('&&DEFOCU.TEMP.VOIS1')
    call jedetr('&&DEFOCU.TEMP.VOIS2')
    call jedetr('&&DEFOCU.TEMP.CHM  ')
    call jedetr('&&DEFOCU.TEMP.IACHM')
    call jedetr('&&DEFOCU.TEMP.LNOE')
    call jedetr('&&DEFOCU.TEMP.PNOE')
    call jedetr('&&DEFOCU.TEMP.IPOI1')
    call jedetr('&&DEFOCU.TEMP.ISEG2')
    call jedetr('&&DEFOCU.TEMP.VALE')
    call jedetr('&&DEFOCU.CONNEX')
    call jedetr('&&DEFOCU.TYPMAI')
!
    call jedema()
end subroutine
