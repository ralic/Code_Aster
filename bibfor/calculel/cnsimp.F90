subroutine cnsimp(cnsz, unite)
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    character(len=*) :: cnsz
    integer :: unite
! ---------------------------------------------------------------------
! BUT: IMPRIMER UN CHAM_NO_S
! ---------------------------------------------------------------------
!     ARGUMENTS:
! CNSZ   IN/JXIN  K19 : SD CHAM_NO_S A IMPRIMER
! UNITE  IN       I   : NUMERO DE L'UNITE LOGIQUE D'IMPRESSION
!
! REMARQUE :
!  - POUR L'INSTANT ON IMPRIME AU FORMAT "RESULTAT" LES CHAMPS DE R8
!-----------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer :: jcnsk, jcnsd, jcnsv, jcnsl, jcnsc
    integer :: nbno, ibid, k, ino, ncmp, ncmpu, jlval, ik, licmpu(997)
    character(len=8) :: ma, nomgd, nomno
    character(len=3) :: tsca
    character(len=19) :: cns
    character(len=40) :: fmt1, fmt2
    logical :: exicmp
!     ------------------------------------------------------------------
    call jemarq()
!
!
    cns = cnsz
!
    call jeveuo(cns//'.CNSK', 'L', jcnsk)
    call jeveuo(cns//'.CNSD', 'L', jcnsd)
    call jeveuo(cns//'.CNSC', 'L', jcnsc)
    call jeveuo(cns//'.CNSV', 'L', jcnsv)
    call jeveuo(cns//'.CNSL', 'L', jcnsl)
!
    ma = zk8(jcnsk-1+1)
    nomgd = zk8(jcnsk-1+2)
    nbno = zi(jcnsd-1+1)
    ncmp = zi(jcnsd-1+2)
!
!
!     1- CALCUL DE NCMPU  : NB CMPS UTILISEES DANS LE CHAMP
!            ET DE LICMPU : NUMEROS DES CMPS UTILISEES
!     ------------------------------------------------------------
    ncmpu = 0
    do 30,k = 1,ncmp
    do 10,ino = 1,nbno
    if (zl(jcnsl-1+ (ino-1)*ncmp+k)) goto 20
10  continue
    goto 30
20  continue
    ncmpu = ncmpu + 1
    ASSERT(ncmpu.le.997)
    licmpu(ncmpu) = k
    30 end do
!
!     -- LE CHAMP EST VIDE : ON SORT
    if (ncmpu .eq. 0) goto 9999
!
    call dismoi('F', 'TYPE_SCA', nomgd, 'GRANDEUR', ibid,&
                tsca, ibid)
    ASSERT((tsca.eq.'R') .or. (tsca.eq.'K8') .or. (tsca.eq.'I') .or. (tsca.eq.'C'))
!
!
!     1- ALLOCATION D'UN TABLEAU DE K16 QUI CONTIENDRA LES VALEURS
!         D'UNE LIGNE A ECRIRE
!     ------------------------------------------------------------
    if (tsca .ne. 'C') then
        call wkvect('&&CNSIMP.LVALEURS', 'V V K16', ncmpu, jlval)
    else
        call wkvect('&&CNSIMP.LVALEURS', 'V V K16', 2*ncmpu, jlval)
    endif
!
!
!     2- FORMAT DES LIGNES :
!     ----------------------
    if (tsca .ne. 'C') then
        fmt1 = '(A12,XXX(''|'',A12))'
        fmt2 = '(A12,XXX(''|'',A12))'
    else
        fmt1 = '(A12,XXX(''|'',A25))'
        fmt2 = '(A12,XXX(''|'',A12,'' '',A12))'
    endif
    call codent(ncmpu, 'D', fmt1(6:8))
    call codent(ncmpu, 'D', fmt2(6:8))
!
!
!     3- ECRITURE DE L'ENTETE DU CHAMP :
!     ---------------------------------------
    write (unite,*) ' '
    write (unite,*) ' GRANDEUR: ',nomgd
    write (unite,*) ' '
    write (unite,fmt1) 'NOEUD', (zk8(jcnsc-1+licmpu(ik)),ik=1,ncmpu)
!
!
!     4- ECRITURE DES VALEURS :
!     ---------------------------------------
    do 70,ino = 1,nbno
    call jenuno(jexnum(ma//'.NOMNOE', ino), nomno)
!
!       -- ON N'ECRIT UN NOEUD QUE S'IL EXISTE AU MOINS 1 CMP :
    exicmp = .false.
    do 40,ik = 1,ncmpu
    k = licmpu(ik)
    if (zl(jcnsl-1+ (ino-1)*ncmp+k)) then
        exicmp = .true.
        goto 50
    endif
40  continue
50  continue
    if (.not.exicmp) goto 70
!
!
!
!       -- ON MET LES VALEURS NON AFFECTEES A " " :
    do 60,ik = 1,ncmpu
    k = licmpu(ik)
    if (zl(jcnsl-1+ (ino-1)*ncmp+k)) then
        if (tsca .eq. 'R') then
            write (zk16(jlval-1+ik),'(E12.5,A4)') zr(jcnsv-1+&
                    (ino-1)*ncmp+k),' '
        else if (tsca.eq.'K8') then
            write (zk16(jlval-1+ik),'(A8,A8)') zk8(jcnsv-1+&
                    (ino-1)*ncmp+k),' '
        else if (tsca.eq.'C') then
            write (zk16(jlval-1+2*(ik-1)+1),'(E12.5,A4)')&
                    dble(zc(jcnsv-1+(ino-1)*ncmp+k)),' '
            write (zk16(jlval-1+2*(ik-1)+2),'(E12.5,A4)')&
                    dimag(zc(jcnsv-1+(ino-1)*ncmp+k)),' '
        else if (tsca.eq.'I') then
            write (zk16(jlval-1+ik),'(I12,A4)') zi(jcnsv-1+&
                    (ino-1)*ncmp+k),' '
        endif
    else
        if (tsca .ne. 'C') then
            write (zk16(jlval-1+ik),'(A16)') ' '
        else
            write (zk16(jlval-1+2*(ik-1)+1),'(A16)') ' '
            write (zk16(jlval-1+2*(ik-1)+2),'(A16)') ' '
        endif
    endif
60  continue
    if (tsca .ne. 'C') then
        write (unite,fmt2) nomno, (zk16(jlval-1+ik),ik=1,ncmpu)
    else
        write (unite,fmt2) nomno, (zk16(jlval-1+2*(ik-1)+1),&
            zk16(jlval-1+2*(ik-1)+2),ik=1,ncmpu)
    endif
!
    70 end do
!
9999  continue
!
    call jedetr('&&CNSIMP.LVALEURS')
    call jedema()
end subroutine
