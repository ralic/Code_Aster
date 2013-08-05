subroutine chsut1(chs1, nomgd2, ncmp, lcmp1, lcmp2,&
                  base, chs2)
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
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mesk.h"
#include "asterfort/verigd.h"
    integer :: ncmp
    character(len=*) :: chs1, nomgd2, base, chs2
    character(len=8) :: lcmp1(ncmp), lcmp2(ncmp)
! ---------------------------------------------------------------------
! BUT: CHANGER LA GRANDEUR ET LE NOM DES CMPS D'UN CHAMP_S
! ---------------------------------------------------------------------
!     ARGUMENTS:
! CHS1   IN/JXIN  K19 : SD CHAMP_S A MODIFIER
! CHS2   IN/JXOUT K19 : SD CHAMP_S MODIFIEE
! BASE   IN       K1  : /G/V/L
! NCMP   IN       I   : NOMBRE DE CMPS DE LCMP1 ET LCMP2
!                       IL FAUT QUE LCMP1 CONTIENNE TOUTES LES CMPS
!                       DE CHS1
! NOMGD2  IN      K8   : NOM DE LA GRANDEUR "APRES"
! LCMP1   IN      L_K8 : LISTE DES CMPS "AVANT"
! LCMP2   IN      L_K8 : LISTE DES CMPS "APRES"
!
! REMARQUE : CHS2 PEUT ETRE IDENTIQUE A CHS1 (CHAMP_S MODIFIE)
!-----------------------------------------------------------------------
    character(len=24) :: valk(3)
!     ------------------------------------------------------------------
    character(len=19) :: chsa, chsb, chsp
    integer :: i1, i2, jcs1k, jcs1d, jcs1c, jcs2k, jcs2c, k, kk
    integer :: iret, ibid, ncmpch
!
    character(len=8) :: nocmp, nomgd1, tsca1, tsca2
!
    chsa = chs1
    chsb = chs2
    chsp = '&&CHUT1.CHAMP_S_IN'
!
    call exisd('CHAM_NO_S', chsa, i1)
    call exisd('CHAM_ELEM_S', chsa, i2)
    if (i1*i2 .ne. 0) call u2mesk('A', 'CALCULEL2_2', 1, chsa)
    if (i1+i2 .eq. 0) call u2mesk('A', 'CALCULEL2_3', 1, chsa)
!
!
!     1.  ON RECOPIE LE CHAMP "IN" ET ON RECUPERE LES ADRESSES JEVEUX :
!     -----------------------------------------------------------------
    if (i1 .gt. 0) then
!      -- CAS D'UN CHAM_NO_S :
        call copisd('CHAM_NO_S', 'V', chsa, chsp)
        call copisd('CHAM_NO_S', base, chsp, chsb)
        call jeveuo(chsp//'.CNSK', 'L', jcs1k)
        call jeveuo(chsp//'.CNSD', 'L', jcs1d)
        call jeveuo(chsp//'.CNSC', 'L', jcs1c)
        call jeveuo(chsb//'.CNSK', 'E', jcs2k)
        call jeveuo(chsb//'.CNSC', 'E', jcs2c)
!
    else
!      -- CAS D'UN CHAM_ELEM_S :
        call copisd('CHAM_ELEM_S', 'V', chsa, chsp)
        call copisd('CHAM_ELEM_S', base, chsp, chsb)
        call jeveuo(chsp//'.CESK', 'L', jcs1k)
        call jeveuo(chsp//'.CESD', 'L', jcs1d)
        call jeveuo(chsp//'.CESC', 'L', jcs1c)
        call jeveuo(chsb//'.CESK', 'E', jcs2k)
        call jeveuo(chsb//'.CESC', 'E', jcs2c)
    endif
!
!
!     2. QUELQUES VERIFICATIONS :
!     ----------------------------
!
!     2.1 : LES TYPES SCALAIRES DE NOMGD1 ET NOMGD2 SONT LES MEMES:
    nomgd1 = zk8(jcs1k-1+2)
    call dismoi('F', 'TYPE_SCA', nomgd1, 'GRANDEUR', ibid,&
                tsca1, ibid)
    call dismoi('F', 'TYPE_SCA', nomgd2, 'GRANDEUR', ibid,&
                tsca2, ibid)
    if (tsca1 .ne. tsca2) then
        valk(1)=tsca1
        valk(2)=tsca2
        call u2mesk('F', 'CALCULEL4_4', 2, valk)
    endif
!
!     2.2 : NOMGD1 ET LCMP1 SONT COHERENTS :
    call verigd(nomgd1, lcmp1, ncmp, iret)
    ASSERT(iret.le.0)
!
!     2.3 : NOMGD2 ET LCMP2 SONT COHERENTS :
    call verigd(nomgd2, lcmp2, ncmp, iret)
    ASSERT(iret.le.0)
!
!
!      3. MODIFICATION DE CHS2 :
!      -------------------------
    zk8(jcs2k-1+2) = nomgd2
    ncmpch = zi(jcs1d-1+2)
    do 10,k = 1,ncmpch
    nocmp = zk8(jcs1c-1+k)
    kk = indik8(lcmp1,nocmp,1,ncmp)
!       SI KK.EQ.0 : ON NE SAIT PAS RENOMMER LA CMP
    ASSERT(kk.ne.0)
    zk8(jcs2c-1+k) = lcmp2(kk)
    10 end do
!
!
!
!     5. MENAGE :
!     -----------
    if (i1 .gt. 0) then
        call detrsd('CHAM_NO_S', chsp)
    else
        call detrsd('CHAM_ELEM_S', chsp)
    endif
!
end subroutine
