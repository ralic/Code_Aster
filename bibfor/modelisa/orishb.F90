subroutine orishb(noma)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/getvem.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/oris15.h"
#include "asterfort/oris20.h"
#include "asterfort/orish6.h"
#include "asterfort/orish8.h"
#include "asterfort/pacoor.h"
#include "asterfort/wkvect.h"
    character(len=8) :: noma
!.======================================================================
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ORISHB  --  LE BUT EST DE REORIENTER, SI C'EST NECESSAIRE,
!                 LES MAILLES DU GROUPES DE MAILLES DONNES POUR
!                 QUE LES ELEMETNS SHB8 FONCTIONNENT
!      ETAPE 1 : LA FACE DU BAS (1234) DOIT AVOIR UNE NORMALE
!                TOURNEE VERS L'INTéRIEUR
!   ARGUMENT        E/S  TYPE         ROLE
!    NOMA         IN/OUT   K*      NOM DU MAILLAGE
!
!.========================= DEBUT DES DECLARATIONS ====================
! -----  VARIABLES LOCALES
    integer :: iocc, ng, jjj, ngr, igr, niv, ifm, ima, numa, ino, jgro, jdes
    integer :: nbmail, nbno, nutyma, idtyma, jcoor, nuno(20)
    real(kind=8) :: coor(60), ps
    character(len=8) :: k8b, typel
    character(len=16) :: motfac
    character(len=24) :: grmama, connex, gmat
    integer :: iarg
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
    call jemarq()
    call infniv(ifm, niv)
!
    grmama = noma//'.GROUPEMA'
    connex = noma//'.CONNEX'
    call jeveuo(noma//'.TYPMAIL', 'L', idtyma)
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
    motfac='ORIE_SHB'
    iocc=1
    call getvem(noma, 'GROUP_MA', motfac, 'GROUP_MA', iocc,&
                iarg, 0, k8b, ng)
    if (ng .ne. 0) then
        ng = -ng
        call wkvect('&&ORISHB.WORK', 'V V K24', ng, jjj)
        call getvem(noma, 'GROUP_MA', motfac, 'GROUP_MA', iocc,&
                    iarg, ng, zk24(jjj), ngr)
!
!      POUR CHAQUE GROUPE DE MAILLES
        do 60 igr = 1, ngr
!
            gmat = zk24(jjj+igr-1)
!
            call jelira(jexnom(grmama, gmat), 'LONUTI', nbmail)
            call jeveuo(jexnom(grmama, gmat), 'L', jgro)
!
!      POUR CHAQUE MAILLE DU GROUPE
!
            do 50 ima = 1, nbmail
                numa = zi(jgro-1+ima)
                nutyma = zi(idtyma+numa-1)
                call jenuno(jexnum('&CATA.TM.NOMTM', nutyma), typel)
!
                if (typel(1:5) .eq. 'HEXA8') then
!
                    call jelira(jexnum(connex, numa), 'LONMAX', nbno)
!
! RECUP DES COORDONNES DES NOEUDS DE LA MAILLE
                    call pacoor(noma, numa, nbno, coor)
!
                    call orish8(coor, ps)
!
! RENUMEROTATION SEULEMENT SI PS < 0
!
                    if (ps .lt. 0.d0) then
!
                        call jeveuo(jexnum(connex, numa), 'E', jdes)
!
                        do 10 ino = 1, nbno
                            nuno(ino) = zi(jdes+ino-1)
10                      continue
!
! RENUMERATION ELEMENT 1234 DEVIENT 1432 ET 5678 DEVIENT 5876
!
                        zi(jdes+1-1) = nuno(1)
                        zi(jdes+2-1) = nuno(4)
                        zi(jdes+3-1) = nuno(3)
                        zi(jdes+4-1) = nuno(2)
                        zi(jdes+5-1) = nuno(5)
                        zi(jdes+6-1) = nuno(8)
                        zi(jdes+7-1) = nuno(7)
                        zi(jdes+8-1) = nuno(6)
!
                    endif
                endif
                if (typel(1:6) .eq. 'PENTA6') then
!
                    call jelira(jexnum(connex, numa), 'LONMAX', nbno)
!
! RECUP DES COORDONNES DES NOEUDS DE LA MAILLE
                    call pacoor(noma, numa, nbno, coor)
!
                    call orish6(coor, ps)
!
! RENUMEROTATION SEULEMENT SI PS < 0
!
                    if (ps .lt. 0.d0) then
!
                        call jeveuo(jexnum(connex, numa), 'E', jdes)
!
                        do 20 ino = 1, nbno
                            nuno(ino) = zi(jdes+ino-1)
20                      continue
!
! RENUMERATION ELEMENT 123 DEVIENT 132 ET 456 DEVIENT 465
!
                        zi(jdes+1-1) = nuno(1)
                        zi(jdes+2-1) = nuno(3)
                        zi(jdes+3-1) = nuno(2)
                        zi(jdes+4-1) = nuno(4)
                        zi(jdes+5-1) = nuno(6)
                        zi(jdes+6-1) = nuno(5)
                    endif
                endif
                if (typel(1:7) .eq. 'PENTA15') then
!
                    call jelira(jexnum(connex, numa), 'LONMAX', nbno)
!
! RECUP DES COORDONNES DES NOEUDS DE LA MAILLE
                    call pacoor(noma, numa, nbno, coor)
!
                    call oris15(coor, ps)
!
! RENUMEROTATION SEULEMENT SI PS < 0
!
                    if (ps .lt. 0.d0) then
!
                        call jeveuo(jexnum(connex, numa), 'E', jdes)
                        do 30 ino = 1, nbno
                            nuno(ino) = zi(jdes+ino-1)
30                      continue
!
! RENUMERATION ELEMENT 123456 DEVIENT 165432 ET 789 DEVIENT 798
! ET 10,11,12,13,14,15 devient 10,15,14,13,12,11
!
                        zi(jdes+1-1) = nuno(1)
                        zi(jdes+2-1) = nuno(3)
                        zi(jdes+3-1) = nuno(2)
                        zi(jdes+4-1) = nuno(4)
                        zi(jdes+5-1) = nuno(6)
                        zi(jdes+6-1) = nuno(5)
                        zi(jdes+7-1) = nuno(9)
                        zi(jdes+8-1) = nuno(8)
                        zi(jdes+9-1) = nuno(7)
                        zi(jdes+10-1) = nuno(10)
                        zi(jdes+11-1) = nuno(12)
                        zi(jdes+12-1) = nuno(11)
                        zi(jdes+13-1) = nuno(15)
                        zi(jdes+14-1) = nuno(14)
                        zi(jdes+15-1) = nuno(13)
                    endif
                endif
                if (typel(1:6) .eq. 'HEXA20') then
!
                    call jelira(jexnum(connex, numa), 'LONMAX', nbno)
!
! RECUP DES COORDONNES DES NOEUDS DE LA MAILLE
                    call pacoor(noma, numa, nbno, coor)
!
                    call oris20(coor, ps)
!
! RENUMEROTATION SEULEMENT SI PS < 0
!
                    if (ps .lt. 0.d0) then
!
                        call jeveuo(jexnum(connex, numa), 'E', jdes)
!
                        do 40 ino = 1, nbno
                            nuno(ino) = zi(jdes+ino-1)
40                      continue
!
! RENUMERATION ELEMENT 12345678 DEVIENT 18765432 ET
! 9,10,11,12 DEVIENT 9,12,11,10 ET 13,14,15,16,17,18,19,20
! DEVIENT 13,20,19,18,17,16,15,14
!
                        zi(jdes+1-1) = nuno(1)
                        zi(jdes+2-1) = nuno(4)
                        zi(jdes+3-1) = nuno(3)
                        zi(jdes+4-1) = nuno(2)
                        zi(jdes+5-1) = nuno(5)
                        zi(jdes+6-1) = nuno(8)
                        zi(jdes+7-1) = nuno(7)
                        zi(jdes+8-1) = nuno(6)
                        zi(jdes+9-1) = nuno(12)
                        zi(jdes+10-1) = nuno(11)
                        zi(jdes+11-1) = nuno(10)
                        zi(jdes+12-1) = nuno(9)
                        zi(jdes+13-1) = nuno(13)
                        zi(jdes+14-1) = nuno(16)
                        zi(jdes+15-1) = nuno(15)
                        zi(jdes+16-1) = nuno(14)
                        zi(jdes+17-1) = nuno(20)
                        zi(jdes+18-1) = nuno(19)
                        zi(jdes+19-1) = nuno(18)
                        zi(jdes+20-1) = nuno(17)
                    endif
                endif
50          continue
60      continue
    endif
!
    call jedema()
end subroutine
