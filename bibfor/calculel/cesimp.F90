subroutine cesimp(cesz, unite, nbmat, nummai)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
!
    character(len=*) :: cesz
    integer :: unite, nbmat, nummai(*)
! ---------------------------------------------------------------------
! BUT: IMPRIMER UN CHAM_ELEM_S
! ---------------------------------------------------------------------
!     ARGUMENTS:
! CESZ   IN/JXIN  K19 : SD CHAM_ELEM_S A IMPRIMER
! UNITE  IN       I   : NUMERO DE L'UNITE LOGIQUE D'IMPRESSION
! NBMAT  IN       I   : /0 : ON IMPRIME TOUTES LES MAILLES
! NBMAT  IN       I   : >0 : ON N'IMPRIME QUE LES MAILLES DE NUMMAI(*)
!                            DE 1 A NBMAT
! NUMMAI IN      V(I) : NUMEROS DES MAILLES A IMPRIMER (SI NBMAT >0)
!
! REMARQUE :
!  - POUR L'INSTANT ON IMPRIME AU FORMAT "RESULTAT" LES CHAMPS DE R8
!-----------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer :: jcesd, jcesv, jcesl, iad, jconx2
    integer :: nbma, k, ima, ncmp, jlval, ipt, isp, nbpt, nbsp, ino, iret
    integer :: ik, ncmpu, licmpu(997), nbmai, im
    character(len=8) :: ma, nomgd, nomma, poin, spoin, typces
    character(len=3) :: tsca
    character(len=19) :: ces
    character(len=60) :: fmt, fmt1
    aster_logical :: exicmp
    character(len=8), pointer :: cesk(:) => null()
    character(len=8), pointer :: cesc(:) => null()
    integer, pointer :: connex(:) => null()
!     ------------------------------------------------------------------
    call jemarq()
!
    ces = cesz
!
    call jeveuo(ces//'.CESK', 'L', vk8=cesk)
    call jeveuo(ces//'.CESD', 'L', jcesd)
    call jeveuo(ces//'.CESC', 'L', vk8=cesc)
    call jeveuo(ces//'.CESV', 'L', jcesv)
    call jeveuo(ces//'.CESL', 'L', jcesl)
!
    ma = cesk(1)
    nomgd = cesk(2)
    typces = cesk(3)
    nbma = zi(jcesd-1+1)
    ncmp = zi(jcesd-1+2)
!
    call jeexin(ma//'.CONNEX', iret)
    ASSERT(iret.ne.0)
    call jeveuo(ma//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', jconx2)
!
!
!     1- CALCUL DE NCMPU  : NB CMPS UTILISEES DANS LE CHAMP
!            ET DE LICMPU : NUMEROS DES CMPS UTILISEES
!     ------------------------------------------------------------
    ncmpu = 0
    do k = 1, ncmp
        do ima = 1, nbma
            nbpt = zi(jcesd-1+5+4* (ima-1)+1)
            nbsp = zi(jcesd-1+5+4* (ima-1)+2)
            do ipt = 1, nbpt
                do isp = 1, nbsp
                    call cesexi('C', jcesd, jcesl, ima, ipt,&
                                isp, k, iad)
                    if (iad .gt. 0) goto 40
                end do
            end do
        end do
        goto 50
 40     continue
        ncmpu = ncmpu + 1
        licmpu(ncmpu) = k
 50     continue
    end do
!
!
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
    if (tsca .eq. 'I' .or. tsca .eq. 'K8') then
        fmt1= '(3(''|'',A12),XXX(''|'',A12))'
        fmt = '(3(''|'',A12),XXX(''|'',A12))'
    else if (tsca.eq.'R'.or.tsca.eq.'K16') then
        fmt1= '(3(''|'',A12),XXX(''|'',A16))'
        fmt = '(3(''|'',A12),XXX(''|'',A16))'
    else if (tsca.eq.'C') then
        fmt1= '(3(''|'',A12),XXX(''|'',A33))'
        fmt = '(3(''|'',A12),XXX(''|'',A16,'' '',A16))'
    else
!       ON ATTEND UN TYPE PARMI : I/R/K8/K16
        ASSERT(.false.)
    endif
!
!
!     1- ALLOCATION D'UN TABLEAU DE K16 QUI CONTIENDRA LES VALEURS
!        D'UNE LIGNE A ECRIRE
!     ------------------------------------------------------------
    if (tsca .ne. 'C') then
        call wkvect('&&CESIMP.LVALEURS', 'V V K16', max(ncmpu, 1), jlval)
    else
        call wkvect('&&CESIMP.LVALEURS', 'V V K16', 2*max(ncmpu, 1), jlval)
    endif
!
!
!     2- FORMAT DES LIGNES :
!     ----------------------
    ASSERT(ncmpu.le.997)
    call codent(ncmpu, 'D', fmt1(13:15))
    call codent(ncmpu, 'D', fmt(13:15))
!
!
!     3- ECRITURE DE L'ENTETE DU CHAMP :
!     ---------------------------------------
    write (unite,*) ' '
    write (unite,*) ' GRANDEUR: ',nomgd
    write (unite,*) ' '
    write (unite,fmt1) 'MAILLE','POINT','SOUS_POINT',&
     &  (cesc(licmpu(ik)),ik=1,ncmpu)
!
!
!     4- ECRITURE DES VALEURS :
!     ---------------------------------------
    if (nbmat .eq. 0) then
        nbmai = nbma
    else
        nbmai = nbmat
    endif
    do im = 1, nbmai
        ima = im
        if (nbmat .ne. 0) ima = nummai(im)
        call jenuno(jexnum(ma//'.NOMMAI', ima), nomma)
        nbpt = zi(jcesd-1+5+4* (ima-1)+1)
        nbsp = zi(jcesd-1+5+4* (ima-1)+2)
        do ipt = 1, nbpt
            do isp = 1, nbsp
!
!       -- ON N'ECRIT UN SOUS_POINT QUE S'IL EXISTE AU MOINS 1 CMP :
                exicmp = .false.
                do ik = 1, ncmpu
                    k = licmpu(ik)
                    call cesexi('C', jcesd, jcesl, ima, ipt,&
                                isp, k, iad)
                    if (iad .gt. 0) then
                        exicmp = .true.
                        goto 70
                    endif
                end do
 70             continue
                if (.not.exicmp) goto 90
!
!
!
                do ik = 1, ncmpu
                    k = licmpu(ik)
                    call cesexi('C', jcesd, jcesl, ima, ipt,&
                                isp, k, iad)
                    if (iad .gt. 0) then
!
                        if (tsca .eq. 'R') then
                            write (zk16(jlval-1+ik),'(1PE16.9)') zr(&
                            jcesv-1+iad)
!
                        else if (tsca.eq.'C') then
                            write (zk16(jlval-1+2*(ik-1)+1),'(1PE16.9)')&
     &                  dble(zc(jcesv-1+iad))
                            write (zk16(jlval-1+2*(ik-1)+2),'(1PE16.9)')&
     &                  dimag(zc(jcesv-1+iad))
!
                        else if (tsca.eq.'I') then
                            write (zk16(jlval-1+ik),'(I12,A4)') zi(&
                            jcesv-1+iad),' '
!
                        else if (tsca.eq.'K8') then
                            write (zk16(jlval-1+ik),'(A8,A8)') zk8(&
                            jcesv-1+iad),' '
!
                        else if (tsca.eq.'K16') then
                            write (zk16(jlval-1+ik),'(A16)') zk16(&
                            jcesv-1+iad)
                        endif
!
                    else
!               -- ON MET LES VALEURS NON AFFECTEES A " " :
                        write (zk16(jlval-1+ik),'(A16)') ' '
                    endif
                end do
!
!
                if (typces .eq. 'ELNO') then
                    ino = connex(zi(jconx2+ima-1)+ipt-1)
                    call jenuno(jexnum(ma//'.NOMNOE', ino), poin)
                else
                    write (poin,'(I8)') ipt
                endif
!
                write (spoin,'(I8)') isp
                if (tsca .ne. 'C') then
                    write (unite,fmt) nomma,poin,spoin, (zk16(jlval-1+&
                    ik),ik=1,ncmpu)
                else
                    write (unite,fmt) nomma,poin,spoin, (zk16(jlval-1+&
                    ik),ik=1,2*ncmpu)
                endif
!
 90             continue
            end do
        end do
    end do
!
    call jedetr('&&CESIMP.LVALEURS')
    call jedema()
end subroutine
