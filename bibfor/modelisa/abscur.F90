subroutine abscur(nomu, it)
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
    implicit none
#include "jeveux.h"
#include "asterfort/alcart.h"
#include "asterfort/i2extf.h"
#include "asterfort/i2sens.h"
#include "asterfort/i2tgrm.h"
#include "asterfort/i2vois.h"
#include "asterfort/jecrec.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/nocart.h"
#include "asterfort/sdmail.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: nomu
!-----------------------------------------------------------------------
!     CALCUL D'UNE ABSCISSE CURVILIGNE POUR UN GROUPE DE MAILLES
!     " TOUTES LES MAILLES DOIVENT ETRE DU TYPE 'POI1' OU 'SEG2' "
!
!     ARGUMENTS EN ENTREE
!     ------------------
!
!     NOMU   : NOM DU MAILLAGE
!     IT     : =1 CALCUL SUR L'ENSEMBLE DU MAILLAGE
!
!     EN SORTIE
!     ---------
!
!     CREATION D'UNE CARTE: (APPEL A ALCART)
!
    character(len=8) :: typm
    character(len=24) :: conseg, typseg
    character(len=24) :: nommai, nomnoe, cooval, coodsc, cooref, grpnoe
    character(len=24) :: gpptnn, grpmai, gpptnm, connex, titre, typmai, adapma
    integer :: ptch, adrm
!
!
!-----------------------------------------------------------------------
    integer :: iab1, iab2, iach, iacnex, iadr2, iagm, iancmp
    integer :: iav1, iav2, iavalv, icoo1, icoo2, icor2, ij
    integer :: im, ima, ima1, ima2, ind, ing, ino
    integer :: ipoi1, iseg2, isens, it, itym, itypm, ival
    integer :: jgcnx, kseg, lplace, mi, n, n1, n2
    integer :: nbchm, nbnoma, nbpoi1, nbrma, nbrma1, nbrma2, nbseg2
    integer :: numno
    real(kind=8) :: s, stot, x1, x2, y1, y2, z1
    real(kind=8) :: z2
!-----------------------------------------------------------------------
    call jemarq()
!
    call sdmail(nomu, nommai, nomnoe, cooval, coodsc,&
                cooref, grpnoe, gpptnn, grpmai, gpptnm,&
                connex, titre, typmai, adapma)
!
    if (it .eq. 1) then
        nommai = nomu//'.NOMMAI'
        call jelira(nommai, 'NOMUTI', nbrma)
        call wkvect('&&ABSCURV.TEMP', 'V V I', nbrma, iagm)
        do 11 ij = 1, nbrma
            zi(iagm+ij-1) = ij
11      continue
    else
        call utmess('F', 'MODELISA_1')
    endif
!
    nbrma2 = 2*nbrma
    nbrma1 = nbrma+1
!
!     CREATION D'OBJETS TEMPORAIRES
!
    call wkvect('&&ABSCUR.TEMP.VOIS1', 'V V I', nbrma, iav1)
    call wkvect('&&ABSCUR.TEMP.VOIS2', 'V V I', nbrma, iav2)
    call wkvect('&&ABSCUR.TEMP.CHM  ', 'V V I', nbrma1, ptch)
    call wkvect('&&ABSCUR.TEMP.IACHM', 'V V I', nbrma2, iach)
    call wkvect('&&ABSCUR.TEMP.PLACE', 'V V L', nbrma, lplace)
    call wkvect('&&ABSCUR.TEMP.IPOI1', 'V V I', nbrma, ima1)
    call wkvect('&&ABSCUR.TEMP.ISEG2', 'V V I', nbrma, ima2)
    call wkvect('&&ABSCUR.TEMP.AB1  ', 'V V R', nbrma, iab1)
    call wkvect('&&ABSCUR.TEMP.AB2  ', 'V V R', nbrma, iab2)
    call wkvect('&&ABSCUR.TEMP.COR2 ', 'V V I', nbrma, icor2)
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
    conseg='&&ABSCUR.CONNEX'
    typseg='&&ABSCUR.TYPMAI'
    call wkvect(typseg, 'V V I', nbrma, itym)
    do 13 im = 1, nbrma
        zi(itym-1+im)=kseg
13  end do
!     IL FAUT CREER UNE TABLE DE CONNECTIVITE POUR LES SEG2
!
! -     OBJET CONSEG    = FAMILLE CONTIGUE DE VECTEURS N*IS
!                         POINTEUR DE NOM       = NOMMAI
!                         POINTEUR DE LONGUEUR  = CONSEG.$$LONC
!                         LONGUEUR TOTALE       = NBNOMA
!
    nbnoma=2*nbseg2
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
!     IL FAUT VERIFIER L'INCLUSION DES POI1
    do 15 ipoi1 = 1, nbpoi1
        im=zi(ima1+ipoi1-1)
        call jeveuo(jexnum(connex, im), 'L', adrm)
        n = zi(adrm)
        do 16 iseg2 = 1, nbseg2
            call jeveuo(jexnum(conseg, iseg2), 'L', iadr2)
            n1 = zi(iadr2)
            n2 = zi(iadr2 + 1)
            if (n1 .eq. n) then
                zi(icor2+ipoi1-1)= iseg2
                goto 15
            else if (n2.eq.n) then
                zi(icor2+ipoi1-1)=-iseg2
                goto 15
            endif
16      continue
        call utmess('F', 'MODELISA_3')
15  continue
!
!
    call i2vois(conseg, typseg, zi(iagm), nbseg2, zi(iav1),&
                zi(iav2))
    call i2tgrm(zi(iav1), zi(iav2), nbseg2, zi(iach), zi(ptch),&
                nbchm)
!
    if (nbchm .gt. 1) then
        call utmess('F', 'MODELISA_4')
    endif
!
    call i2sens(zi(iach), 2*nbseg2, zi(iagm), nbseg2, conseg,&
                typseg)
!
!     CREATION D'UNE CARTE
!
    call alcart('G', nomu//'.ABS_CURV  ', nomu, 'ABSC_R')
    call jeveuo(nomu//'.ABS_CURV  .NCMP', 'E', iancmp)
    call jeveuo(nomu//'.ABS_CURV  .VALV', 'E', iavalv)
    zk8(iancmp) = 'ABSC1'
    zk8(iancmp+1) = 'ABSC2'
    stot = 0.d0
!
!     CALCUL DE L'ABSCISSE CURVILIGNE
!
    do 10 iseg2 = 1, nbseg2
        isens = 1
        mi = zi(iach+iseg2-1)
        if (mi .lt. 0) then
            mi = - mi
            isens = -1
        endif
        ima=zi(ima2+mi-1)
        call i2extf(mi, 1, conseg, typseg, ing,&
                    ind)
        call jeveuo(cooval, 'L', ival)
        if (isens .eq. 1) then
            icoo1 = 3*(ing-1)
            icoo2 = 3*(ind-1)
        else
            icoo1 = 3*(ind-1)
            icoo2 = 3*(ing-1)
        endif
        x1 = zr(ival + icoo1)
        y1 = zr(ival + icoo1 + 1)
        z1 = zr(ival + icoo1 + 2)
        x2 = zr(ival + icoo2)
        y2 = zr(ival + icoo2 + 1)
        z2 = zr(ival + icoo2 + 2)
        s = sqrt((x2-x1)**2+(y2-y1)**2+(z2-z1)**2)
        zr(iavalv ) = stot
        zr(iab1+mi-1)= stot
        stot = stot+s
        zr(iavalv+1) = stot
        zr(iab2+mi-1)= stot
        call nocart(nomu//'.ABS_CURV  ', 3, ' ', 'NUM', 1,&
                    ' ', ima, ' ', 2)
10  end do
!     CAS DES POI1
    do 20 ipoi1 = 1, nbpoi1
        ima=zi(ima1+ipoi1-1)
        mi=zi(icor2+ipoi1-1)
        if (mi .gt. 0) then
            s=zr(iab1+mi-1)
        else
            s=zr(iab2-mi-1)
        endif
        zr(iavalv ) = s
        zr(iavalv+1) = s
        call nocart(nomu//'.ABS_CURV  ', 3, ' ', 'NUM', 1,&
                    ' ', ima, ' ', 2)
20  end do
!
! --- MENAGE
!
    call jedetr('&&ABSCUR.CONNEX')
    call jedetr('&&ABSCUR.TEMP.AB1  ')
    call jedetr('&&ABSCUR.TEMP.AB2  ')
    call jedetr('&&ABSCUR.TEMP.CHM  ')
    call jedetr('&&ABSCUR.TEMP.COR2 ')
    call jedetr('&&ABSCUR.TEMP.IACHM')
    call jedetr('&&ABSCUR.TEMP.IPOI1')
    call jedetr('&&ABSCUR.TEMP.ISEG2')
    call jedetr('&&ABSCUR.TEMP.PLACE')
    call jedetr('&&ABSCUR.TEMP.VOIS1')
    call jedetr('&&ABSCUR.TEMP.VOIS2')
    call jedetr('&&ABSCUR.TYPMAI')
    call jedetr('&&ABSCURV.TEMP')
    call jedetr(nomu//'.ABS_CURV  .NCMP')
    call jedetr(nomu//'.ABS_CURV  .VALV')
!
    call jedema()
end subroutine
