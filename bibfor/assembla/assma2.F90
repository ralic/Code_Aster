subroutine assma2(lmasym, tt, nu14, ncmp, matel,&
                  c1, jvalm, jtmp2, lgtmp2)
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
! person_in_charge: jacques.pellet at edf.fr
!
! aslint: disable=
    implicit none
!-----------------------------------------------------------------------
! BUT : ASSEMBLER LES MACRO-ELEMENTS DANS UNE MATR_ASSE
!-----------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/ascopr.h"
#include "asterfort/asretm.h"
#include "asterfort/assert.h"
#include "asterfort/cordd2.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
#include "asterfort/ssvalm.h"
!
    character(len=16) :: optio
!-----------------------------------------------------------------------
    real(kind=8) :: c1
    logical :: lmasym, lmesym
    character(len=2) :: tt
    character(len=19) :: matel
    character(len=8) :: mo, ma, nogdco, nogdsi, nomacr
    character(len=14) :: nu14, num2
    integer :: nbecmx
    parameter(nbecmx=10)
    integer :: icodla(nbecmx), icodge(nbecmx)
    integer :: i1, i2, iaconx, iad1, iad11, iad2, iad21
    integer :: jsupma, jnmacr, jnueq, jnulo1, jprno, jposd1
    integer :: iec, ima, inold, nbterm, jprn1, jprn2
    integer :: jresl, jsmdi, jsmhc, jsssa, jvalm(2), k1
    integer :: k2, n1, nugd, iancmp, lgncmp, icmp
    integer :: nbsma, nbssa, ncmp, nbvel, nddl1, nddl2, jtmp2, lgtmp2
    integer :: nec, nm, nmxcmp, nnoe, i, jec
    integer :: lshift
!-----------------------------------------------------------------------
!     FONCTIONS FORMULES :
!-----------------------------------------------------------------------
!
#define zzprno(ili,nunoel,l) zi(jprn1-1+zi(jprn2+ili-1)+ \
    (nunoel-1)*(nec+2)+l-1)
#define numlo1(kno,k) zi(jnulo1-1+2*(kno-1)+k)
#define posdd1(kno,kddl) zi(jposd1-1+nmxcmp*(kno-1)+kddl)
!-----------------------------------------------------------------------
!
!
    call jemarq()
!
    call dismoi('NB_SS_ACTI', matel, 'MATR_ELEM', repi=nbssa)
    if (nbssa .eq. 0) goto 100
!
    lmesym=.true.
    do i = 1, nbecmx
        icodla(i)=0
        icodge(i)=0
    end do
!
    call dismoi('NOM_MODELE', nu14, 'NUME_DDL', repk=mo)
    call dismoi('NOM_MAILLA', mo, 'MODELE', repk=ma)
    call dismoi('NB_NO_MAILLA', mo, 'MODELE', repi=nm)
    call dismoi('NB_SM_MAILLA', mo, 'MODELE', repi=nbsma)
    call jeveuo(ma//'.NOMACR', 'L', jnmacr)
    call jeveuo(mo//'.MODELE    .SSSA', 'L', jsssa)
!
    call jeveuo(nu14//'.SMOS.SMDI', 'L', jsmdi)
    call jeveuo(nu14//'.SMOS.SMHC', 'L', jsmhc)
    call jeveuo(nu14//'.NUME.NUEQ', 'L', jnueq)
    call jeveuo(nu14//'.NUME.PRNO', 'L', jprn1)
    call jeveuo(jexatr(nu14//'.NUME.PRNO', 'LONCUM'), 'L', jprn2)
!
    call dismoi('SUR_OPTION', matel, 'MATR_ELEM', repk=optio)
    call dismoi('NOM_GD', nu14, 'NUME_DDL', repk=nogdco)
    call dismoi('NOM_GD_SI', nogdco, 'GRANDEUR', repk=nogdsi)
    call dismoi('NB_CMP_MAX', nogdsi, 'GRANDEUR', repi=nmxcmp)
    call dismoi('NUM_GD_SI', nogdsi, 'GRANDEUR', repi=nugd)
    nec=nbec(nugd)
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nogdsi), 'L', iancmp)
    call jelira(jexnom('&CATA.GD.NOMCMP', nogdsi), 'LONMAX', lgncmp)
    icmp=indik8(zk8(iancmp),'LAGR',1,lgncmp)
    if (icmp .gt. 0) then
        jec=(icmp-1)/30+1
        icodla(jec)=lshift(1,icmp-(jec-1)*30)
    endif
!
    call jeveuo('&&ASSMAM.NUMLO1', 'E', jnulo1)
    call jeveuo('&&ASSMAM.POSDD1', 'E', jposd1)
!
!
!
    call ssvalm('DEBUT', optio, mo, ma, 0,&
                jresl, nbvel)
!
    do ima = 1, nbsma
!             -- BOUCLE SUR LES MACRO-ELEMENTS :
!             ----------------------------------
        if (zi(jsssa-1+ima) .eq. 0) goto 90
!
        call jeveuo(jexnum(ma//'.SUPMAIL', ima), 'L', jsupma)
        call jelira(jexnum(ma//'.SUPMAIL', ima), 'LONMAX', nnoe)
!
        nbterm=0
!
        call ssvalm(' ', optio, mo, ma, ima,&
                    jresl, nbvel)
!
        nomacr=zk8(jnmacr-1+ima)
        call dismoi('NOM_NUME_DDL', nomacr, 'MACR_ELEM_STAT', repk=num2)
        call jeveuo(nomacr//'.CONX', 'L', iaconx)
        call jeveuo(jexnum(num2//'.NUME.PRNO', 1), 'L', jprno)
!
        do k1 = 1, nnoe
            n1=zi(jsupma-1+k1)
            if (n1 .gt. nm) then
                do iec = 1, nbecmx
                    icodge(iec)=icodla(iec)
                end do
!
            else
                inold=zi(iaconx-1+3*(k1-1)+2)
                do iec = 1, nec
                    icodge(iec)=zi(jprno-1+(nec+2)*(inold-1)+2+iec)
                end do
            endif
!
            iad1=zzprno(1,n1,1)
            call cordd2(jprn1, jprn2, 1, icodge, nec,&
                        ncmp, n1, nddl1, zi(jposd1-1+nmxcmp*(k1-1)+1))
            zi(jnulo1-1+2*(k1-1)+1)=iad1
            zi(jnulo1-1+2*(k1-1)+2)=nddl1
            do i1 = 1, nddl1
                do k2 = 1, k1-1
                    iad2=numlo1(k2,1)
                    nddl2=numlo1(k2,2)
                    do i2 = 1, nddl2
                        iad11=zi(jnueq-1+iad1+posdd1(k1,i1)-1)
                        iad21=zi(jnueq-1+iad2+posdd1(k2,i2)-1)
                        call asretm(lmasym, jtmp2, lgtmp2, nbterm, jsmhc,&
                                    jsmdi, iad11, iad21)
                    end do
                end do
                k2=k1
                iad2=numlo1(k2,1)
                nddl2=numlo1(k2,2)
                do i2 = 1, i1
                    iad11=zi(jnueq-1+iad1+posdd1(k1,i1)-1)
                    iad21=zi(jnueq-1+iad2+posdd1(k2,i2)-1)
                    call asretm(lmasym, jtmp2, lgtmp2, nbterm, jsmhc,&
                                jsmdi, iad11, iad21)
                end do
            end do
        end do
!
!
!             ---- POUR FINIR, ON RECOPIE EFFECTIVEMENT LES TERMES:
        call ascopr(lmasym, lmesym, 'R'//tt(2:2), jtmp2, nbterm,&
                    jresl, c1, jvalm)
 90     continue
    end do
    call ssvalm('FIN', optio, mo, ma, ima,&
                jresl, nbvel)
!
!
100 continue
    call jedema()
end subroutine
