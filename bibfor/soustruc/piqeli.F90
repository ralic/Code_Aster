subroutine piqeli(mailla)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/cpclma.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/pacoa1.h"
#include "asterfort/pacoa3.h"
#include "asterfort/wkvect.h"
    character(len=8) :: mailla
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
!     OPERATEUR: "DEFI_GROUP" , MOTCLE FACTEUR "EQUE_PIQUA"
!     ELIMINE LES NOEUDS EN DOUBLE:
!             SURFACE S_LAT1  AVEC S_LAT2
!             SURFACE S_FOND1 AVEC S_FOND2
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
    integer :: nbma, jmail, ima, inuma, nbpt, jpoin, ino, inov, nbno, jgrn1
    integer :: jgrn2, jvi1, jvi2, iret, nbgma, igr
    integer :: nbno3, nbno4, nbnor, jgrnr, ino1, ino2, i, nb1, nb2
    integer :: numno(10), inc, nbnor2, jgrnr2, nbgno, nbno2, jgg, jvg
!
    character(len=8) ::  nomgrm, nogrn1, nogrn2
    character(len=8) :: nogrn3, nogrn4, nogrnr, nomgno
    integer :: in, nbnoms
    parameter    (nbnoms=8)
    character(len=8) :: noms1(nbnoms), noms2(nbnoms)
    character(len=24) :: grpmai, grpnoe, connex, liso1, liso2, liso3, grpnov
    logical :: recosf
    real(kind=8) :: dmin0
!
    data noms1 / 'S_LAT1','S_LAT1_C','S_LAT1_T','S_FOND1',&
     &             'S_LAT1_C','S_LAT1_C','S_LAT1_C','S_LAT1_C' /
    data noms2 / 'S_LAT2','S_LAT2_C','S_LAT2_T','S_FOND2',&
     &             'S_FOND1','S_FOND2','S_FOND1','S_FOND2' /
!     ------------------------------------------------------------------
!
    call jemarq()
!
    grpmai = mailla//'.GROUPEMA       '
    grpnoe = mailla//'.GROUPENO       '
    connex = mailla//'.CONNEX         '
    grpnov = '&&PIQELI'//'.GROUPENO       '
    liso1 = '&&PIQELI.NOEUD_1'
    liso2 = '&&PIQELI.NOEUD_2'
    liso3 = '&&PIQELI.NOEUD_3'
!
    call jelira(mailla//'.GROUPEMA', 'NOMUTI', nbgma)
!
!
! --- CORRECTION DES GROUPES DE NOEUDS S_FOND1 ET S_FOND2 APRES
!     RECOLLEMENT DES AUTRES GROUPES DE NOEUDS
!     PREPARATION DES DONNEES
!
    nogrn3 = noms1(4)
    nogrn4 = noms2(4)
    recosf = .false.
    call jeexin(jexnom(grpnoe, nogrn3), iret)
    if (iret .eq. 0) recosf = .false.
    call jeexin(jexnom(grpnoe, nogrn4), iret)
    if (iret .eq. 0) recosf = .false.
    call jelira(jexnom(grpnoe, nogrn3), 'LONUTI', nbno3)
    call jelira(jexnom(grpnoe, nogrn4), 'LONUTI', nbno4)
    if (nbno3 .le. nbno4) then
        recosf = .true.
        nogrnr = nogrn4
        nbnor = nbno4
        call jeveuo(jexnom(grpnoe, nogrnr), 'L', jgrnr)
    endif
    if (nbno4 .le. nbno3) then
        recosf = .true.
        nogrnr = nogrn3
        nbnor = nbno3
        call jeveuo(jexnom(grpnoe, nogrnr), 'L', jgrnr)
    endif
!
! --- ELIMINATION SUR LES GROUPES DE MAILLES
!
    do 300 in = 1, 4
        nogrn1 = noms1(in)
        nogrn2 = noms2(in)
        call jeexin(jexnom(grpnoe, nogrn1), iret)
        if (iret .eq. 0) goto 300
        call jeexin(jexnom(grpnoe, nogrn2), iret)
        if (iret .eq. 0) goto 300
        call jelira(jexnom(grpnoe, nogrn1), 'LONUTI', nbno)
        call jeveuo(jexnom(grpnoe, nogrn1), 'L', jgrn1)
        call jeveuo(jexnom(grpnoe, nogrn2), 'L', jgrn2)
        call pacoa1(zi(jgrn1), zi(jgrn2), nbno, mailla, liso1,&
                    liso2)
        call jeveuo(liso1, 'L', jvi1)
        call jeveuo(liso2, 'L', jvi2)
!
        do 100 igr = 1, nbgma
            call jenuno(jexnum(grpmai, igr), nomgrm)
            call jelira(jexnom(grpmai, nomgrm), 'LONUTI', nbma)
            call jeveuo(jexnom(grpmai, nomgrm), 'E', jmail)
            do 200 ima = 1, nbma
                inuma = zi(jmail+ima-1)
                call jelira(jexnum(connex, inuma), 'LONMAX', nbpt)
                call jeveuo(jexnum(connex, inuma), 'E', jpoin)
                do 202 ino = 1, nbpt
                    do 204 inov = 1, nbno
                        if (zi(jvi2+inov-1) .eq. zi(jpoin+ino-1)) then
                            zi(jpoin+ino-1) = zi(jvi1+inov-1)
                        endif
204                  continue
202              continue
200          continue
100      end do
!
! --- CORRECTION DES GROUPES DE NOEUDS S_FOND1 ET S_FOND2 APRES
!     RECOLLEMENT DES AUTRES GROUPES DE NOEUDS
!
        if (recosf) then
            inc = 0
            do 400 ino = 1, nbnor
                do 401 ino1 = 1, nbno
                    if (zi(jgrnr+ino-1) .eq. zi(jvi1+ino1-1)) then
                        do 402 i = 1, nbnor
                            if (zi(jgrnr+i-1) .eq. zi(jvi2+ino1-1)) then
                                inc = inc + 1
                                ASSERT(inc.le.10)
                                numno(inc) = zi(jvi2+ino1-1)
                            endif
402                      continue
                    endif
401              continue
400          continue
!
!
            if (inc .gt. 0) then
                ino2 = 0
                nbnor2 = nbnor - inc
                call wkvect(liso3, 'V V I', nbnor2, jgrnr2)
                do 404 ino = 1, nbnor
                    do 405 ino1 = 1, inc
                        if (numno(ino1) .eq. zi(jgrnr+ino-1)) goto 404
405                  continue
                    ino2 = ino2 + 1
                    zi(jgrnr2+ino2-1) = zi(jgrnr+ino-1)
404              continue
!
                call jelira(grpnoe, 'NOMUTI', nbgno)
                call cpclma(mailla, '&&PIQELI', 'GROUPENO', 'V')
                call jedetr(grpnoe)
                call jecrec(grpnoe, 'G V I', 'NOM', 'DISPERSE', 'VARIABLE',&
                            nbgno)
                do 408 i = 1, nbgno
                    call jenuno(jexnum(grpnov, i), nomgno)
                    call jelira(jexnum(grpnov, i), 'LONUTI', nbno2)
                    call jeveuo(jexnum(grpnov, i), 'L', jvg)
!
                    if (nomgno .ne. nogrnr) then
                        call jecroc(jexnom(grpnoe, nomgno))
                        call jeecra(jexnom(grpnoe, nomgno), 'LONMAX', max(1,nbno2))
                        call jeecra(jexnom(grpnoe, nomgno), 'LONUTI', nbno2)
                        call jeveuo(jexnom(grpnoe, nomgno), 'E', jgg)
                        do 406 ino = 0, nbno2-1
                            zi(jgg+ino) = zi(jvg+ino)
406                      continue
                    else
                        call jecroc(jexnom(grpnoe, nogrnr))
                        call jeecra(jexnom(grpnoe, nogrnr), 'LONMAX', max(1,nbnor2))
                        call jeecra(jexnom(grpnoe, nogrnr), 'LONUTI', nbnor2)
                        call jeveuo(jexnom(grpnoe, nogrnr), 'E', jgrnr)
                        do 407 ino = 1, nbnor2
                            zi(jgrnr+ino-1) = zi(jgrnr2+ino-1)
407                      continue
                    endif
408              continue
            endif
        endif
!
300  end do
!
! --- ELIMINATION SUR LES GROUPES DE MAILLES
!
    do 800 in = 5, 8
        nogrn1 = noms1(in)
        nogrn2 = noms2(in)
        call jeexin(jexnom(grpnoe, nogrn1), iret)
        if (iret .eq. 0) goto 800
        call jeexin(jexnom(grpnoe, nogrn2), iret)
        if (iret .eq. 0) goto 800
        call jelira(jexnom(grpnoe, nogrn1), 'LONUTI', nb1)
        call jelira(jexnom(grpnoe, nogrn2), 'LONUTI', nb2)
        call jeveuo(jexnom(grpnoe, nogrn1), 'L', jgrn1)
        call jeveuo(jexnom(grpnoe, nogrn2), 'L', jgrn2)
        dmin0 = 0.01d0
        call pacoa3(zi(jgrn1), zi(jgrn2), nb1, nb2, dmin0,&
                    mailla, liso1, liso2, nbno)
        call jeveuo(liso1, 'L', jvi1)
        call jeveuo(liso2, 'L', jvi2)
!
        do 600 igr = 1, nbgma
            call jenuno(jexnum(grpmai, igr), nomgrm)
            call jelira(jexnom(grpmai, nomgrm), 'LONUTI', nbma)
            call jeveuo(jexnom(grpmai, nomgrm), 'E', jmail)
            do 500 ima = 1, nbma
                inuma = zi(jmail+ima-1)
                call jelira(jexnum(connex, inuma), 'LONMAX', nbpt)
                call jeveuo(jexnum(connex, inuma), 'E', jpoin)
                do 502 ino = 1, nbpt
                    do 504 inov = 1, nbno
                        if (zi(jvi2+inov-1) .eq. zi(jpoin+ino-1)) then
                            zi(jpoin+ino-1) = zi(jvi1+inov-1)
                        endif
504                  continue
502              continue
500          continue
600      end do
!
800  end do
!
    call jedetr(liso1)
    call jedetr(liso2)
    call jedetr(liso3)
    call jedetr(grpnov)
!
    call jedema()
!
end subroutine
