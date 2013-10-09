subroutine lrvema(nomail, mfich, nochmd)
    implicit none
!
#include "jeveux.h"
#include "asterfort/as_mfdfin.h"
#include "asterfort/as_mfdncn.h"
#include "asterfort/as_mficlo.h"
#include "asterfort/as_mfiope.h"
#include "asterfort/as_mmhnme.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/ulisog.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: mfich
    character(len=8) :: nomail
    character(len=64) :: nochmd
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!-----------------------------------------------------------------------
!   BUT : ROUTINE DE LIRE RESU / LIRE_CHAMP QUI VERIFIE LA COHERENCE
!       ENTRE LE MAILLAGE FOURNI ET LES DONNEES DU FICHIER MED
!
! IN  :
!   NOMAIL  K8   NOM DU MAILLAGE ASTER
!   MFICH   I    NUMERO DU FICHIER MED
!   NOCHMD  K64  NOM D'UN CHAMP REPOSANT SUR LE MAILLAGE MED A VERIFIER
!-----------------------------------------------------------------------
! person_in_charge: nicolas.sellenet at edf.fr
!
    integer :: iret, nmatyp, ncmp, jcmp, junit
    integer :: nbma, jnbtyp, jmatyp, nbtym, nbtv, codret
    integer :: i, j, idfimd, iaux, jnbty2
    integer :: vali(2), lnomam, jtymas
    integer :: edlect
    parameter (edlect=0)
    integer :: ntymax
    parameter (ntymax = 69)
    integer :: edconn
    parameter (edconn=1)
    integer :: edmail
    parameter (edmail=0)
    integer :: ednoda
    parameter (ednoda=0)
!
    integer :: nummed(ntymax)
    character(len=1) :: k1b
    character(len=8) :: saux08, nomast(ntymax)
    character(len=64) :: nomamd
    character(len=200) :: nofimd
    character(len=255) :: kfic
    logical :: lfirst
!
    data nomast  /'POI1    ','SEG2    ','SEG22   ','SEG3    ',&
     &              'SEG33   ','SEG4    ',&
     &                         'TRIA3   ','TRIA33  ','TRIA6   ',&
     &              'TRIA66  ','TRIA7   ','QUAD4   ','QUAD44  ',&
     &              'QUAD8   ','QUAD88  ','QUAD9   ','QUAD99  ',&
     &              'TETRA4  ','TETRA10 ','PENTA6  ','PENTA15 ',&
     &              'PENTA18 ','PYRAM5  ','PYRAM13 ','HEXA8   ',&
     &              'HEXA20  ','HEXA27  ','TR3QU4  ','QU4TR3  ',&
     &              'TR6TR3  ','TR3TR6  ','TR6QU4  ','QU4TR6  ',&
     &              'TR6QU8  ','QU8TR6  ','TR6QU9  ','QU9TR6  ',&
     &              'QU8TR3  ','TR3QU8  ','QU8QU4  ','QU4QU8  ',&
     &              'QU8QU9  ','QU9QU8  ','QU9QU4  ','QU4QU9  ',&
     &              'QU9TR3  ','TR3QU9  ','SEG32   ','SEG23   ',&
     &              'QU4QU4  ','TR3TR3  ','HE8HE8  ','PE6PE6  ',&
     &              'TE4TE4  ','QU8QU8  ','TR6TR6  ','SE2TR3  ',&
     &              'SE2TR6  ','SE2QU4  ','SE2QU8  ','SE2QU9  ',&
     &              'SE3TR3  ','SE3TR6  ','SE3QU4  ','SE3QU8  ',&
     &              'SE3QU9  ','H20H20  ','P15P15  ','T10T10  '/
!
    data nummed  /1,         102,       0,         103,&
     &              0,         0,&
     &                         203,       0,         206,&
     &              0,         207,       204,       0,&
     &              208,       0,         209,       0,&
     &              304,       310,       306,       315,&
     &              0,         305,       313,       308,&
     &              320,       327,       0,         0,&
     &              0,         0,         0,         0,&
     &              0,         0,         0,         0,&
     &              0,         0,         0,         0,&
     &              0,         0,         0,         0,&
     &              0,         0,         0,         0,&
     &              0,         0,         0,         0,&
     &              0,         0,         0,         0,&
     &              0,         0,         0,         0,&
     &              0,         0,         0,         0,&
     &              0,         0,         0,         0/
!-----------------------------------------------------------------------
!
    call jemarq()
!
!     ON VERIFIE QUE LE MAILLAGE FOURNI ET CELUI
!     CONTENU DANS LE FICHIER MED ONT
!     - MEME TYPE DE MAILLE
!     - MEME NOMBRE DE MAILLE PAR TYPE
!     =================================
!
    call ulisog(mfich, kfic, k1b)
    if (kfic(1:1) .eq. ' ') then
        call codent(mfich, 'G', saux08)
        nofimd = 'fort.'//saux08
    else
        nofimd = kfic(1:200)
    endif
!
    nomamd=' '
    call as_mfiope(idfimd, nofimd, edlect, iaux)
    if (iaux .ne. 0) then
        lnomam = lxlgut(saux08)
        call utmess('F', 'MED_78', sk=saux08(1:lnomam))
    endif
!
    call as_mfdncn(idfimd, nochmd, ncmp, codret)
    if (codret .ne. 0) then
        call utmess('F', 'MED_32', sk=nochmd)
    endif
    call wkvect('&&LRVEMA.CNAME', 'V V K16', ncmp, jcmp)
    call wkvect('&&LRVEMA.CUNIT', 'V V K16', ncmp, junit)
!
    call as_mfdfin(idfimd, nochmd, nomamd, nbtv, zk16(junit),&
                   zk16(jcmp), codret)
!
    call wkvect('&&LRVERIMO_NBETYP1', 'V V I', ntymax, jnbtyp)
    call wkvect('&&LRVERIMO_NBETYP2', 'V V I', ntymax, jnbty2)
    do 10 i = 1, ntymax
        zi(jnbtyp+i-1)=0
        if (nummed(i) .ne. 0) then
            call as_mmhnme(idfimd, nomamd, edconn, edmail, nummed(i),&
                           ednoda, nmatyp, iret)
            zi(jnbtyp+i-1)=nmatyp
        endif
 10 end do
!
    call as_mficlo(idfimd, iret)
    call dismoi('NB_MA_MAILLA', nomail, 'MAILLAGE', repi=nbma)
    call wkvect('&&LRVERIMO_NBMA_TYP', 'V V I', nbma, jmatyp)
    do 20 i = 1, nbma
        zi(jmatyp+i-1)=0
 20 end do
!
    call jeveuo(nomail//'.TYPMAIL', 'L', jtymas)
    do 30 i = 1, nbma
        zi(jmatyp+i-1)=nummed(zi(jtymas+i-1))
 30 end do
!
    do 50 i = 1, ntymax
        nbtym=0
        zi(jnbty2+i-1)=nbtym
        if (nummed(i) .ne. 0) then
            do 60 j = 1, nbma
                if (zi(jmatyp+j-1) .eq. nummed(i)) then
                    nbtym=nbtym+1
                endif
 60         continue
        endif
        zi(jnbty2+i-1)=nbtym
 50 end do
!
    lfirst=.true.
    do 70 i = 1, ntymax
        if (nummed(i) .ne. 0) then
            if (zi(jnbtyp+i-1) .ne. zi(jnbty2+i-1) .and. lfirst) then
                lfirst=.false.
                call utmess('A+', 'MED_54')
                if (zi(jnbtyp+i-1) .lt. zi(jnbty2+i-1)) then
                    vali(1)=zi(jnbtyp+i-1)
                    vali(2)=zi(jnbty2+i-1)
                    call utmess('A', 'MED_59', sk=nomast(i), ni=2, vali=vali)
                else
                    vali(1)=zi(jnbtyp+i-1)
                    vali(2)=zi(jnbty2+i-1)
                    call utmess('A', 'MED_61', sk=nomast(i), ni=2, vali=vali)
                endif
!
            endif
        endif
 70 end do
!
    call jedetr('&&LRVERIMO_NBETYP1')
    call jedetr('&&LRVERIMO_NBETYP2')
    call jedetr('&&LRVERIMO_NBMA_TYP')
    call jedetr('&&LRVEMA.CNAME')
    call jedetr('&&LRVEMA.CUNIT')
!
    call jedema()
!
end subroutine
