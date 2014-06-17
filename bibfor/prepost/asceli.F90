subroutine asceli(mailla)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/copich.h"
#include "asterfort/cpclma.h"
#include "asterfort/detrs2.h"
#include "asterfort/grpdbl.h"
#include "asterfort/iunifi.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedup1.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/pacoa1.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: mailla
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     OPERATEUR: "MODI_MAILLAGE" , MOTCLE FACTEUR "PLAQ_TUBE"
!     ELIMINE LES NOEUDS EN DOUBLE:
!             SURFACE BORD1 AVEC BORD2
!
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
    integer :: inuma, nbpt, jpoin, ino, inov, nbmat, ifm, jpoin2
    integer :: nbno, jgrn1, jgrn2, jvi1, jvi2, iret, igr, k, jind2
    integer ::  nbnot, nbgrno,  i, jind, nbno2,  kk
    character(len=8) :: tmp, nomno
    character(len=24) :: grpnoe, connex, liso1, liso2, nogrn1, nogrn2, nomgrn
    integer, pointer :: dime(:) => null()
    real(kind=8), pointer :: coor(:) => null()
    real(kind=8), pointer :: cor(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
!
    ifm = iunifi('MESSAGE')
!
    grpnoe = mailla//'.GROUPENO'
    connex = mailla//'.CONNEX'
!
    nogrn1 = 'BORD1'
    nogrn2 = 'BORD2'
    call jeexin(jexnom(grpnoe, nogrn1), iret)
    if (iret .eq. 0) then
        call utmess('F', 'PREPOST_1', sk=nogrn1)
    endif
    call jeexin(jexnom(grpnoe, nogrn2), iret)
    if (iret .eq. 0) then
        call utmess('F', 'PREPOST_1', sk=nogrn2)
    endif
    call jelira(jexnom(grpnoe, nogrn1), 'LONUTI', nbno)
    call jeveuo(jexnom(grpnoe, nogrn1), 'L', jgrn1)
    call jeveuo(jexnom(grpnoe, nogrn2), 'L', jgrn2)
    liso1 = '&&ASCELI.NOEUD_1'
    liso2 = '&&ASCELI.NOEUD_2'
    call pacoa1(zi(jgrn1), zi(jgrn2), nbno, mailla, liso1,&
                liso2)
    call jeveuo(liso1, 'L', jvi1)
    call jeveuo(liso2, 'L', jvi2)
    call jeveuo(mailla//'.DIME', 'E', vi=dime)
    nbnot = dime(1)
    nbmat = dime(3)
!
    write(ifm,*) 'COUTURE - NOMBRE DE NOEUDS ELIMINES: ',nbno
    write(ifm,*) 'COUTURE - NOMBRE DE NOEUDS TOTAL   : ',nbnot-nbno
!
!     OBJET .GROUPENO : ON CHANGE LES NUMEROS DE NOEUDS DES GROUPENO
!     --------------------------------------------------------------
    call jelira(grpnoe, 'NUTIOC', nbgrno)
    do 70 igr = 1, nbgrno
        call jenuno(jexnum(grpnoe, igr), nomgrn)
        call jelira(jexnom(grpnoe, nomgrn), 'LONUTI', nbpt)
        call jeveuo(jexnom(grpnoe, nomgrn), 'E', jpoin)
        do 80 ino = 1, nbpt
            do 90 inov = 1, nbno
                if (zi(jvi2+inov-1) .eq. zi(jpoin+ino-1)) then
                    zi(jpoin+ino-1) = zi(jvi1+inov-1)
                    goto 80
                endif
90          continue
80      continue
70  end do
!
!     -- ON SUPPRIME LES EVENTUELS DOUBLONS CREES DANS .GROUPENO :
    call grpdbl(mailla, 'GROUPENO')
!
!
!     OBJET .CONNEX : ON CHANGE LES CONNECTIVITES
!     --------------------------------------------------------
    do 200 inuma = 1, nbmat
        call jelira(jexnum(connex, inuma), 'LONMAX', nbpt)
        call jeveuo(jexnum(connex, inuma), 'E', jpoin)
        do 202 ino = 1, nbpt
            do 204 inov = 1, nbno
                if (zi(jvi2+inov-1) .eq. zi(jpoin+ino-1)) then
                    zi(jpoin+ino-1) = zi(jvi1+inov-1)
                    goto 202
                endif
204          continue
202      continue
200  end do
!
!     OBJETS .NOMNOE, .COORDO.VALE : ON ACTUALISE CES OBJETS EN
!     SUPPRIMANT LES NOEUDS DOUBLES SUITE AU RECOLLEMENT DES BORDS
!     ------------------------------------------------------------
    tmp='TMP'
    nbno2=nbnot-nbno
    dime(1)=nbno2
!
    call jedup1(mailla//'.NOMNOE', 'V', tmp//'.NOMNOE')
    call jedup1(mailla//'.CONNEX', 'V', tmp//'.CONNEX')
    call cpclma(mailla, tmp, 'GROUPENO', 'V')
!
    call copich('V', mailla//'.COORDO', tmp//'.COORDO')
!
    call jedetr(mailla//'.NOMNOE')
    call jecreo(mailla//'.NOMNOE', 'G N K8')
    call jeecra(mailla//'.NOMNOE', 'NOMMAX', nbno2)
    call jedetr(mailla//'.COORDO    .VALE')
    call jecreo(mailla//'.COORDO    .VALE', 'G V R')
    call jeecra(mailla//'.COORDO    .VALE', 'LONMAX', 3*nbno2)
    call jeveuo(mailla//'.COORDO    .VALE', 'E', vr=coor)
    call jeveuo(tmp//'.COORDO    .VALE', 'L', vr=cor)
!
!     TABLEAUX DE TRAVAIL:
!     - ZI(JIND) INDIQUE LES NOEUDS A SUPPRIMER PAR UNE VALEUR 1
!     - ZI(JIND2) PERMET DE REACTUALISER LES NUMEROS DES NOEUDS:
!       ZI(JIND2+I-1)=J SIGNIFIE QUE LE NOEUD I DE LA NOUVELLE
!       SD MAILLAGE PORTERA LE NUMERO I-J
    call wkvect('&&ASCELI_IND_NOEU_DEL', 'V V I', nbnot, jind)
    call wkvect('&&ASCELI_IND_NOEU_CONN', 'V V I', nbnot, jind2)
    do 206 i = 1, nbnot
        zi(jind+i-1)=0
        zi(jind2+i-1)=0
206  end do
    do 207 i = 1, nbno
        zi(jind+zi(jvi2+i-1)-1)=1
207  end do
    k=0
    kk=0
    do 205 i = 1, nbnot
        if (zi(jind+i-1) .eq. 0) then
            k=k+1
            call jenuno(jexnum(tmp//'.NOMNOE', i), nomno)
            call jecroc(jexnom(mailla//'.NOMNOE', nomno))
            coor(1+3*(k-1)) =cor(1+3*(i-1))
            coor(1+3*(k-1)+1)=cor(1+3*(i-1)+1)
            coor(1+3*(k-1)+2)=cor(1+3*(i-1)+2)
            zi(jind2+i-1)=kk
        else
            kk=kk+1
        endif
205  end do
    ASSERT(k.eq.nbno2)
!
!
!     ON ACTUALISE LES OBJETS .GROUPENO ET .CONNEX
!     CAR LES NOMS DES NOEUDS ONT CHANGE
!     ----------------------------------
    do 210 inuma = 1, nbmat
        call jelira(jexnum(tmp//'.CONNEX', inuma), 'LONMAX', nbpt)
        call jeveuo(jexnum(connex, inuma), 'E', jpoin)
        call jeveuo(jexnum(tmp//'.CONNEX', inuma), 'L', jpoin2)
        do 212 ino = 1, nbpt
            zi(jpoin+ino-1)= zi(jpoin2+ino-1)-zi(jind2+zi(jpoin2+ino-&
            1)-1)
212      continue
210  end do
!
    do 270 igr = 1, nbgrno
        call jenuno(jexnum(grpnoe, igr), nomgrn)
        call jelira(jexnom(grpnoe, nomgrn), 'LONUTI', nbpt)
        call jeveuo(jexnom(grpnoe, nomgrn), 'E', jpoin)
        call jeveuo(jexnom(tmp//'.GROUPENO', nomgrn), 'L', jpoin2)
        do 280 ino = 1, nbpt
            zi(jpoin+ino-1)= zi(jpoin2+ino-1)-zi(jind2+zi(jpoin2+ino-&
            1)-1)
280      continue
270  continue
!
!
    call detrs2('CHAM_NO', tmp//'.COORDO')
    call jedetr(tmp//'.NOMNOE')
    call jedetr(tmp//'.CONNEX')
    call jedetr(tmp//'.GROUPENO')
    call jedetr('&&ASCELI_IND_NOEU_DEL')
    call jedetr('&&ASCELI.NOEUD_1')
    call jedetr('&&ASCELI.NOEUD_2')
!
    call jedema()
end subroutine
