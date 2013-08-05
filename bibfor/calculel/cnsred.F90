subroutine cnsred(cns1z, nbno, lino, nbcmp, licmp,&
                  base, cns2z)
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
! A_UTIL
    implicit none
#include "jeveux.h"
!
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/cnscre.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    character(len=*) :: cns1z, cns2z, base
    integer :: nbno, nbcmp, lino(nbno)
    character(len=*) :: licmp(nbcmp)
! ---------------------------------------------------------------------
! BUT: REDUIRE UN CHAM_NO_S SUR UN ENSEMBLE DE NOEUDS ET DE COMPOSANTES
! ---------------------------------------------------------------------
!     ARGUMENTS:
! CNS1Z   IN/JXIN  K19 : SD CHAM_NO_S A REDUIRE
! CNS2Z   IN/JXOUT K19 : SD CHAM_NO_S REDUITE
! BASE    IN       K1  : BASE DE CREATION POUR CNS2Z : G/V/L
! NBNO    IN        I   : NOMBRE DE NOEUDS DE LINO
!                         SI NBNO=0 : ON NE REDUIT PAS SUR LES NOEUDS
! LINO    IN        L_I : LISTE DES NUMEROS DE NOEUDS SUR LESQUELS
!                         ON VEUT REDUIRE LE CHAM_NO_S
! NBCMP   IN        I   : NOMBRE DE CMPS DE LICMP
!                         SI NBCMP=0 : ON NE REDUIT PAS SUR LES CMPS
! LICMP   IN        L_K8: LISTE DES NOMS DES CMPS SUR LESQUELLES
!                         ON VEUT REDUIRE LE CHAM_NO_S
!         SI NBCMP > 0 LES CMP DU CHAM_NO_S REDUIT SONT DANS L'ORDRE
!         DE LICMP. SINON ELLES SONT DANS L'ORDRE DE CNS1Z
!
! REMARQUES :
!  - POUR REDUIRE SUR LES NOEUDS (ET GARDER TOUTES LES CMPS)
!        NBCMP=0 LICMP= KBID
!  - POUR REDUIRE SUR LES CMPS (ET GARDER TOUS LES NOEUDS)
!        NBNO=0 LINO= IBID
!  - SI NBCMP=0 ET NBNO=0 : CNS2Z=CNS1Z
!
!  - ON PEUT APPELER CETTE ROUTINE AVEC CNS2Z=CNS1Z
!    LA SD INITIALE (CNS1Z) EST ALORS PERDUE.
!
!-----------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer :: jcn1k, jcn1d, jcn1v, jcn1l, jcn1c, nbnom, ncmp2, jexno, kno
    integer :: icmp2
    integer :: jcn2d, jcn2v, jcn2l, jcn2c
    integer :: ibid
    integer :: ncmpmx, ncmp1, icmp1
    integer :: ino
    character(len=1) :: kbid
    character(len=8) :: ma, nomgd, nocmp
    character(len=3) :: tsca
    character(len=19) :: cns1, cns2
!     ------------------------------------------------------------------
    call jemarq()
!
!
    cns1 = cns1z
    cns2 = cns2z
!
!     -- POUR NE PAS ECRASER LA SOURCE SI CNS2=CNS1 :
    if (cns2 .eq. cns1) then
        call copisd('CHAM_NO_S', 'V', cns1, '&&CNSRED.CNS1')
        cns1 = '&&CNSRED.CNS1'
    endif
!
!
    call jeveuo(cns1//'.CNSK', 'L', jcn1k)
    call jeveuo(cns1//'.CNSD', 'L', jcn1d)
    call jeveuo(cns1//'.CNSC', 'L', jcn1c)
    call jeveuo(cns1//'.CNSV', 'L', jcn1v)
    call jeveuo(cns1//'.CNSL', 'L', jcn1l)
!
    ma = zk8(jcn1k-1+1)
    nomgd = zk8(jcn1k-1+2)
    nbnom = zi(jcn1d-1+1)
    ncmp1 = zi(jcn1d-1+2)
!
    call dismoi('F', 'TYPE_SCA', nomgd, 'GRANDEUR', ibid,&
                tsca, ibid)
    call dismoi('F', 'NB_CMP_MAX', nomgd, 'GRANDEUR', ncmpmx,&
                kbid, ibid)
!
    ASSERT(nbcmp.ge.0)
    if (nbcmp .gt. 0) then
        ncmp2 = nbcmp
    else
        ncmp2 = ncmp1
    endif
!
!
!     1- CREATION DE CNS2 :
!     ---------------------------------------
    if (nbcmp .gt. 0) then
        call cnscre(ma, nomgd, ncmp2, licmp, base,&
                    cns2)
    else
        call cnscre(ma, nomgd, ncmp2, zk8(jcn1c), base,&
                    cns2)
    endif
    call jeveuo(cns2//'.CNSD', 'L', jcn2d)
    call jeveuo(cns2//'.CNSC', 'L', jcn2c)
    call jeveuo(cns2//'.CNSV', 'E', jcn2v)
    call jeveuo(cns2//'.CNSL', 'E', jcn2l)
!
!
!     2- ON TRANSFORME LINO EN LISTE DE BOOLEENS:
!     ------------------------------------------
    call wkvect('&&CNSRED.EXINO', 'V V L', nbnom, jexno)
    do 10,kno = 1,nbnom
    zl(jexno-1+kno) = .false.
    10 end do
!
    ASSERT(nbno.ge.0)
    if (nbno .eq. 0) then
        do 20,kno = 1,nbnom
        zl(jexno-1+kno) = .true.
20      continue
!
    else
        do 30,kno = 1,nbno
        zl(jexno-1+lino(kno)) = .true.
30      continue
    endif
!
!     3- REMPLISSAGE DES OBJETS .CNSL ET .CNSV :
!     ------------------------------------------
!
    do 50,icmp2 = 1,ncmp2
    nocmp = zk8(jcn2c-1+icmp2)
    icmp1 = indik8(zk8(jcn1c),nocmp,1,ncmp1)
    if (icmp1 .eq. 0) goto 50
!
!
    do 40,ino = 1,nbnom
    if (zl(jexno-1+ino)) then
        if (zl(jcn1l-1+ (ino-1)*ncmp1+icmp1)) then
            zl(jcn2l-1+ (ino-1)*ncmp2+icmp2) = .true.
!
            if (tsca .eq. 'R') then
                zr(jcn2v-1+ (ino-1)*ncmp2+icmp2) = zr( jcn1v-1+ (ino-1)*ncmp1+icmp1 )
            else if (tsca.eq.'C') then
                zc(jcn2v-1+ (ino-1)*ncmp2+icmp2) = zc( jcn1v-1+ (ino-1)*ncmp1+icmp1 )
            else if (tsca.eq.'I') then
                zi(jcn2v-1+ (ino-1)*ncmp2+icmp2) = zi( jcn1v-1+ (ino-1)*ncmp1+icmp1 )
            else if (tsca.eq.'L') then
                zl(jcn2v-1+ (ino-1)*ncmp2+icmp2) = zl( jcn1v-1+ (ino-1)*ncmp1+icmp1 )
            else if (tsca.eq.'K8') then
                zk8(jcn2v-1+ (ino-1)*ncmp2+icmp2) = zk8( jcn1v- 1+ (ino-1 )*ncmp1+icmp1 )
            else
                ASSERT(.false.)
            endif
!
        endif
    endif
!
40  continue
    50 end do
!
!
!     5- MENAGE :
!     -----------
    call jedetr('&&CNSRED.EXINO')
    if (cns1 .eq. '&&CNSRED.CNS1') call detrsd('CHAM_NO_S', cns1)
!
    call jedema()
end subroutine
