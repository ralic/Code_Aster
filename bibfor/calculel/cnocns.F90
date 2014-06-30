subroutine cnocns(cnoz, basez, cnsz)
! person_in_charge: jacques.pellet at edf.fr
! A_UTIL
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
#include "jeveux.h"
#include "asterc/cheksd.h"
#include "asterfort/assert.h"
#include "asterfort/cmpcha.h"
#include "asterfort/cnscre.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"

    character(len=*) :: cnoz, cnsz, basez
! ------------------------------------------------------------------
! BUT : TRANSFORMER UN CHAM_NO (CNOZ) EN CHAM_NO_S (CNSZ)
! ------------------------------------------------------------------
!     ARGUMENTS:
! CNOZ    IN/JXIN  K19 : SD CHAM_NO A TRANSFORMER
! BASEZ   IN       K1  : BASE DE CREATION POUR CNSZ : G/V/L
! CNSZ    IN/JXOUT K19 : SD CHAM_NO_S A CREER
!     ------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=1) :: base
    character(len=3) :: tsca
    character(len=8) :: ma, nomgd
    character(len=19) :: cno, cns, profcn
    integer :: nec, gd, ncmpmx, nbno,  jrefe, jvale, kcmp, ierr
    integer :: iadg, icmp, jprno,  ino, ncmp, ncmp1, jcnsl, jcnsv
    integer :: ival, ico, ieq, icmp1
    logical(kind=1) :: sdveri
    integer, pointer :: corr1(:) => null()
    integer, pointer :: desc(:) => null()
    integer, pointer :: nueq(:) => null()
    integer, pointer :: corr2(:) => null()
    character(len=8), pointer :: nom_cmp(:) => null()
!     ------------------------------------------------------------------

    call jemarq()
    cno = cnoz
    cns = cnsz
    base = basez

!   -- verification de la SD cno ? (debug) :
    sdveri=.false.
    if (sdveri) then
        call cheksd(cno,'sd_cham_no',ierr)
        ASSERT(ierr.eq.0)
    endif


!     -- SI CNS EXISTE DEJA, ON LE DETRUIT :
    call detrsd('CHAM_NO_S', cns)

    call dismoi('NOM_MAILLA', cno, 'CHAM_NO', repk=ma)
    call dismoi('NOM_GD', cno, 'CHAM_NO', repk=nomgd)

    call dismoi('NB_NO_MAILLA', ma, 'MAILLAGE', repi=nbno)

    call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nec)
    call dismoi('NUM_GD', nomgd, 'GRANDEUR', repi=gd)
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)

    call jeveuo(cno//'.REFE', 'L', jrefe)
    call jeveuo(cno//'.VALE', 'L', jvale)
    call jeveuo(cno//'.DESC', 'L', vi=desc)

!------------------------------------------------------------------
!     1- ON ALLOUE CNS :
!     ------------------

!     1.1 CALCUL DE NCMP1 ET ZK8(JNOCMP) :
!         NCMP1: NOMBRE DE CMPS PORTEES PAR CNO
!         ZK8(JNOCMP): LISTES DES CMPS PORTEES PAR CNO
!     -------------------------------------------
    call cmpcha(cno, '&&CNOCNS.NOM_CMP', '&&CNOCNS.CORR1', '&&CNOCNS.CORR2', ncmp1,&
                ncmpmx)
    call jeveuo('&&CNOCNS.NOM_CMP', 'L', vk8=nom_cmp)
    call jeveuo('&&CNOCNS.CORR1', 'L', vi=corr1)
    call jeveuo('&&CNOCNS.CORR2', 'L', vi=corr2)


!     1.2 ALLOCATION DE CNS :
!     -------------------------------------------
    call cnscre(ma, nomgd, ncmp1, nom_cmp, base,&
                cns)


!------------------------------------------------------------------
!     2- REMPLISSAGE DE CNS.CNSL ET CNS.CNSV :
!     -------------------------------------------
    call jeveuo(cns//'.CNSL', 'E', jcnsl)
    call jeveuo(cns//'.CNSV', 'E', jcnsv)

!     -- CAS DES CHAM_NO A REPRESENTATION CONSTANTE :
    if (desc(2) .lt. 0) then
        profcn = ' '
!     -- CAS DES CHAM_NO A PROF_CHNO:
    else
        call dismoi('PROF_CHNO', cno, 'CHAM_NO', repk=profcn)
    endif

!     2.1 CAS DES CHAM_NO A REPRESENTATION CONSTANTE :
!     ---------------------------------------------------
    if (profcn .eq. ' ') then
        do ino = 1, nbno
            do icmp1 = 1, ncmp1
                zl(jcnsl-1+ (ino-1)*ncmp1+icmp1) = .true.
                ieq = (ino-1)*ncmp1 + icmp1

                if (tsca .eq. 'R') then
                    zr(jcnsv-1+ieq) = zr(jvale-1+ieq)
                else if (tsca.eq.'I') then
                    zi(jcnsv-1+ieq) = zi(jvale-1+ieq)
                else if (tsca.eq.'C') then
                    zc(jcnsv-1+ieq) = zc(jvale-1+ieq)
                else if (tsca.eq.'L') then
                    zl(jcnsv-1+ieq) = zl(jvale-1+ieq)
                else if (tsca.eq.'K8') then
                    zk8(jcnsv-1+ieq) = zk8(jvale-1+ieq)
                else
                    ASSERT(.false.)
                endif
            end do
        end do


!     2.2 CAS DES CHAM_NO A PROF-CHNO
!     ---------------------------------------------------
    else
        call dismoi('PROF_CHNO', cno, 'CHAM_NO', repk=profcn)
        call jeveuo(jexnum(profcn//'.PRNO', 1), 'L', jprno)
        call jeveuo(profcn//'.NUEQ', 'L', vi=nueq)
        do ino = 1, nbno

!         NCMP : NOMBRE DE CMPS SUR LE NOEUD INO
!         IVAL : ADRESSE DU DEBUT DU NOEUD INO DANS .NUEQ
!         IADG : DEBUT DU DESCRIPTEUR GRANDEUR DU NOEUD INO
            ncmp = zi(jprno-1+ (ino-1)* (nec+2)+2)
            if (ncmp .eq. 0) goto 80
            ival = zi(jprno-1+ (ino-1)* (nec+2)+1)
            iadg = jprno - 1 + (ino-1)* (nec+2) + 3

            ico = 0
            do kcmp = 1, ncmp1
                icmp=corr2(kcmp)
                if (exisdg(zi(iadg),icmp)) then
                    ico = ico + 1
                    ieq = nueq(ival-1+ico)
                    icmp1 = corr1(icmp)
!             ASSERT(ICMP1.EQ.KCMP)  COUTEUX ?

                    zl(jcnsl-1+ (ino-1)*ncmp1+icmp1) = .true.

                    if (tsca .eq. 'R') then
                        zr(jcnsv-1+ (ino-1)*ncmp1+icmp1) = zr(jvale-1+ ieq)
                    else if (tsca.eq.'I') then
                        zi(jcnsv-1+ (ino-1)*ncmp1+icmp1) = zi(jvale-1+ ieq)
                    else if (tsca.eq.'C') then
                        zc(jcnsv-1+ (ino-1)*ncmp1+icmp1) = zc(jvale-1+ ieq)
                    else if (tsca.eq.'L') then
                        zl(jcnsv-1+ (ino-1)*ncmp1+icmp1) = zl(jvale-1+ ieq)
                    else if (tsca.eq.'K8') then
                        zk8(jcnsv-1+ (ino-1)*ncmp1+icmp1) = zk8( jvale-1+ieq)
                    else
                        ASSERT(.false.)
                    endif
                endif
            end do
 80         continue
        end do
    endif

!   -- verification de la SD cns :
    if (sdveri)  then
        call cheksd(cns,'sd_cham_no_s',ierr)
        ASSERT(ierr.eq.0)
    endif

!------------------------------------------------------------------

    call jedetr('&&CNOCNS.TMP_NUCMP')
    call jedetr('&&CNOCNS.NOM_CMP')
    call jedetr('&&CNOCNS.CORR1')
    call jedetr('&&CNOCNS.CORR2')
    call jedema()
end subroutine
