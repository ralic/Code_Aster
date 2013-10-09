subroutine xconno(mox, chfis, base, opt, param,&
                  chglo)
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cescel.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnscre.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
!
    character(len=*) :: opt, param
    character(len=1) :: base
    character(len=19) :: chglo
    character(len=11) :: chfis
    character(len=8) :: mox
!
!----------------------------------------------------------------------
!  BUT: CONCATENER LES CHAMPS NODAUX DES SD FISS_XFEM
!       DANS UN CHAMP GLOBAL ELNO AFFECTE AU MODELE
!
!----------------------------------------------------------------------
!
!     ARGUMENTS/
!  MOX     IN    K19 : MODELE XFEM
!  CHFIS   IN    K19 : SUFFIXE DU NOM DU CHAMP NODAL A CONCATENER
!  CHGLO   OUT   K19 : CHAMP GLOBAL RESULTANT
!  BASE    IN    K1  : BASE DE CREATION POUR CHGLO : G/V/L
!
!
!
!
    integer :: nfis, ifis, jj, ino, ii, kk, iret
    integer :: ima, icmp, nbnom, jmacnx, jlcnx
    integer :: ibid, jg, nmaenr, i, jxc
    integer :: jnfis, jcnsd, jcnsc, jcnsv, jcnsk, jcnsl, jcnsv2, jcnsl2
    integer :: ncmp1, jmofis, jcesd, jcesv, jcesl, iad, jnbsp, nncp
    integer :: jcesd2, jcesv2, jcesl2, jtypma, jtmdim, itypma, ndime, ndim
    character(len=3) :: tsca
    character(len=19) :: ces, cns, ligrel, cns2, ces2
    character(len=24) :: grp(3)
    logical :: lstno
    character(len=8) :: ma, nomgd, nomfis, licmp(2)
!     ------------------------------------------------------------------
!
    call jemarq()
    ces = '&&XCONNO.CES'
    cns = '&&XCONNO.CNS'
    ligrel = mox(1:8)//'.MODELE'
    lstno = chfis.eq.'.STNO'
!
!     1.RECUPERATION D'INFORMATIONS DANS MOX
!
    call jeveuo(mox//'.NFIS', 'L', jnfis)
    nfis = zi(jnfis)
!
    call jeveuo(mox//'.FISS', 'L', jmofis)
    nomfis = zk8(jmofis)
!
    call cnocns(nomfis//chfis, 'V', cns)
!
    call jeveuo(cns//'.CNSK', 'L', jcnsk)
    call jeveuo(cns//'.CNSD', 'L', jcnsd)
    call jeveuo(cns//'.CNSC', 'L', jcnsc)
    call jeveuo(cns//'.CNSV', 'L', jcnsv)
    call jeveuo(cns//'.CNSL', 'L', jcnsl)
!
    ma = zk8(jcnsk-1+1)
    nomgd = zk8(jcnsk-1+2)
!      NBNOM = ZI(JCNSD-1+1)
    ncmp1 = zi(jcnsd-1+2)
!
    call jeveuo(ma//'.CONNEX', 'L', jmacnx)
    call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', jlcnx)
    call dismoi('DIM_GEOM', ma, 'MAILLAGE', repi=ndim)
    call jeveuo(ma//'.TYPMAIL', 'L', jtypma)
    call jeveuo('&CATA.TM.TMDIM', 'L', jtmdim)
!
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
!
! --- RECUPERATION DU NOMBRE DE SOUS POINT (NBRE DE FISSURES VUES)
!
    call jeveuo('&&XTYELE.NBSP', 'L', jnbsp)
!
! --- CREATION DE LA SD ELNO
!
    call cescre('V', ces, 'ELNO', ma, nomgd,&
                ncmp1, zk8(jcnsc), [ibid], zi(jnbsp), [-ncmp1])
!
    call jeveuo(ces//'.CESD', 'L', jcesd)
    call jeveuo(ces//'.CESV', 'E', jcesv)
    call jeveuo(ces//'.CESL', 'E', jcesl)
!
! --- ON CREE LA SD MODELE.NOXFEM EN MEME TEMPS QUE MODELE.STNO
!
    if (lstno) then
        cns2 = '&&XCONNO.CNS2'
        licmp(1) = 'X1'
        licmp(2) = 'X2'
        call cnscre(ma, 'NEUT_I', 2, licmp, 'V',&
                    cns2)
        call jeveuo(cns2//'.CNSV', 'E', jcnsv2)
        call jeveuo(cns2//'.CNSL', 'E', jcnsl2)
! --- ON CREE AUSSI UN CHAMP ELEM QUI CONTIENT LE NOM DES FISS
        ces2 = '&&XCONNO.CES2'
        call cescre('V', ces2, 'ELEM', ma, 'NEUT_K8',&
                    1, ['Z1'], [ibid], zi( jnbsp), [-1])
        call jeveuo(ces2//'.CESD', 'L', jcesd2)
        call jeveuo(ces2//'.CESV', 'E', jcesv2)
        call jeveuo(ces2//'.CESL', 'E', jcesl2)
    endif
!
    do 20 ifis = 1, nfis
!
        call jeveuo(mox//'.FISS', 'L', jmofis)
        nomfis = zk8(jmofis-1 + ifis)
        call cnocns(nomfis//chfis, 'V', cns)
!
        grp(1)=nomfis//'.MAILFISS.HEAV'
        grp(2)=nomfis//'.MAILFISS.CTIP'
        grp(3)=nomfis//'.MAILFISS.HECT'
!
        call jeveuo(cns//'.CNSV', 'L', jcnsv)
        call jeveuo(cns//'.CNSL', 'L', jcnsl)
!
        do 1000 ii = 1, 3
!         COPIER LE CHAMP 'CHFIS'
!         POUR LES MAILLES '.HEAV','.CTIP' ET '.HECT'
            call jeexin(grp(ii), iret)
            if (iret .ne. 0) then
                call jeveuo(grp(ii), 'L', jg)
                call jelira(grp(ii), 'LONMAX', nmaenr)
                do 120 i = 1, nmaenr
                    ima = zi(jg-1+i)
                    nbnom = zi(jlcnx+ima)-zi(jlcnx-1+ima)
                    itypma=zi(jtypma-1+ima)
                    ndime= zi(jtmdim-1+itypma)
                    do 1210 jj = 1, nbnom
                        ino = zi(jmacnx + zi(jlcnx-1+ima)-2+jj)
                        do 1220 icmp = 1, ncmp1
!
!                 POUR CHAQUE TYPE 'R', I', 'L', 'K8', SI LE CHAM_NO
!                 A DEJE ETE REMPLI, ON INCREMENTE LE SOUS POINT
                            do 100 kk = 1, zi(jnbsp-1+ima)
                                call cesexi('S', jcesd, jcesl, ima, jj,&
                                            kk, icmp, iad)
                                if (iad .lt. 0) goto 110
100                         continue
                            ASSERT(.false.)
110                         continue
                            if (tsca .eq. 'R') then
                                zl(jcesl-1-iad) = .true.
                                zr(jcesv-1-iad) = zr(jcnsv-1+(ino-1)* ncmp1+icmp)
                            else if (tsca.eq.'I') then
                                zl(jcesl-1-iad) = .true.
                                zi(jcesv-1-iad) = zi(jcnsv-1+(ino-1)* ncmp1+icmp)
                                if (lstno .and. ndim .eq. ndime) then
                                    if ((.not.zl(jcnsl2-1+(ino-1)*2+1) ) .and.&
                                        zi(jcesv-1-iad) .gt. 0) then
                                        zl(jcnsl2-1+(ino-1)*2+1) =&
                                        .true.
                                        zl(jcnsl2-1+(ino-1)*2+2) =&
                                        .true.
                                        zi(jcnsv2-1+(ino-1)*2+1) =&
                                        ima
                                        zi(jcnsv2-1+(ino-1)*2+2) = jj
                                    endif
                                endif
                            else if (tsca.eq.'L') then
                                zl(jcesl-1-iad) = .true.
                                zl(jcesv-1-iad) = zl(jcnsv-1+(ino-1)* ncmp1+icmp)
                            else if (tsca.eq.'K8') then
                                zl(jcesl-1-iad) = .true.
                                zk8(jcesv-1-iad)= zk8(jcnsv-1+(ino-1)*&
                                ncmp1+icmp)
                            else
                                ASSERT(.false.)
                            endif
!
1220                     continue
1210                 continue
                    if (lstno) then
                        do 130 kk = 1, zi(jnbsp-1+ima)
                            call cesexi('S', jcesd2, jcesl2, ima, 1,&
                                        kk, 1, iad)
                            if (iad .lt. 0) then
                                zl(jcesl2-1-iad) = .true.
                                zk8(jcesv2-1-iad) = nomfis
                                goto 120
                            endif
130                     continue
                    endif
120             continue
            endif
1000     continue
!
        call detrsd('CHAM_NO_S', cns)
 20 end do
!
! --- CONVERSION CHAM_ELEM_S -> CHAM_ELEM
!
    call cescel(ces, ligrel, opt, param, 'OUI',&
                nncp, base, chglo, 'F', ibid)
    call detrsd('CHAM_ELEM_S', ces)
!
! --- CONVERSION CHAM_ELEM_S -> CHAM_ELEM POUR MODELE.MAFIS
!
    if (lstno) then
        call jeveuo(mox//'.XFEM_CONT', 'L', jxc)
        if (zi(jxc) .gt. 0) then
            call cescel(ces2, ligrel, 'GEOM_FAC', 'NOMFIS', 'NON',&
                        nncp, base, mox//'.XMAFIS', 'F', ibid)
        endif
    endif
!
    call jedema()
end subroutine
