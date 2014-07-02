subroutine pteddl(typesd, num, nbcmp, lnocmp, neq,&
                  ivec)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: nbcmp, neq, ivec(neq, *)
    character(len=*) :: typesd, num
    character(len=8) :: lnocmp(*)
! ----------------------------------------------------------------------
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
!
! IN : TYPESD : /'NUME_DDL' /'CHAM_NO'
! IN : NUM    : NOM D'UN NUME_DDL[_GENE] OU D'UN PROF_CHNO
! IN : NBCMP  : NOMBRE DE CMP DE LA LISTE LNOCMP
! IN : LNOCMP : LISTE DE NOMS DE CMP
! IN : NEQ    : NOMBRE D'EQUATIONS DE NUM
! OUT: IVEC   : TABLEAU DE POINTEURS DE DDLS DEJA ALLOUE.
!      IVEC(IEQ,ICMP) =
!                   1 SI LE IEQ-EME CMP DE NUM A POUR NOM: LNOCMP(ICMP)
!                   0 SINON
! ----------------------------------------------------------------------
    integer :: ibid, i, j, tabec(10), ncmpmx
    integer :: nec, gd, iad, iec
    integer :: nlili, jprno, nbno, ival, ncmp, icompt
    integer :: icmp, ieq, nucmp, nleq, numno, ino
    integer :: ieql, jnugl, imatd, iexi
    character(len=8) :: nomma
    character(len=19) :: nomnu, prno
    character(len=24) :: nolili
    aster_logical :: matd, lnuge
    integer, pointer :: nume_cmp(:) => null()
    integer, pointer :: desc(:) => null()
    integer, pointer :: nueq(:) => null()
    integer, pointer :: deeq(:) => null()
    integer, pointer :: vnbno(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
!
    do i = 1, neq
        do j = 1, nbcmp
            ivec(i,j)=0
        end do
    end do
!
    nomnu(1:14)=num
    nomnu(15:19)='.NUME'
    call jeexin(nomnu(1:14)//'.NUML.NUGL', imatd)
    if (imatd .ne. 0) then
        call jeveuo(nomnu(1:14)//'.NUML.NUGL', 'L', jnugl)
        matd=.true.
    else
        jnugl=0
        matd=.false.
    endif
!
!
!     -- LNUGE : .TRUE.  : NUME EST UN NUME_DDL_GENE
!                .FALSE. : NUME EST UN NUME_DDL
    call jeexin(nomnu//'.DESC', iexi)
    lnuge=(iexi.gt.0)
!
!
!     -- CAS NUME_DDL :
!     ------------------
    if (.not.lnuge) then
        if (typesd(1:8) .eq. 'NUME_DDL') then
            call dismoi('NOM_MAILLA', num, 'NUME_DDL', repk=nomma)
            call dismoi('NUM_GD_SI', num, 'NUME_DDL', repi=gd)
            prno(1:14)=num
            prno(15:19)='.NUME'
        else if (typesd(1:7).eq.'CHAM_NO') then
            call dismoi('NOM_MAILLA', num, 'CHAM_NO', repk=nomma)
            call dismoi('PROF_CHNO', num, 'CHAM_NO', repk=prno)
            call dismoi('NUM_GD', num, 'CHAM_NO', repi=gd)
        else
            ASSERT(.false.)
        endif
        nec=nbec(gd)
        ASSERT(nec.le.10)
!
        call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', iad)
        call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', ncmpmx)
        AS_ALLOCATE(vi=nume_cmp, size=ncmpmx)
        do i = 0, ncmpmx-1
            nume_cmp(1+i)=indik8(lnocmp,zk8(iad+i),1,nbcmp)
        end do
!
        call jeveuo(prno//'.NUEQ', 'L', vi=nueq)
!
        call jelira(prno//'.PRNO', 'NMAXOC', nlili)
        do i = 1, nlili
            call jenuno(jexnum(prno//'.LILI', i), nolili)
            call jelira(jexnum(prno//'.PRNO', i), 'LONMAX', ibid)
            if (ibid .eq. 0) goto 70
            call jeveuo(jexnum(prno//'.PRNO', i), 'L', jprno)
            if (ibid .eq. 1 .and. zi(jprno) .eq. 0) goto 70
!
!          --RECHERCHE DU NOMBRE DE NOEUDS : NBNO
            if (nolili(1:8) .eq. '&MAILLA ') then
                call jelira(nomma//'.NOMNOE', 'NOMMAX', nbno)
            else
                call jeveuo(nolili(1:19)//'.NBNO', 'L', vi=vnbno)
                nbno=vnbno(1)
            endif
            do ino = 1, nbno
!           NCMP : NOMBRE DE CMPS SUR LE NOEUD INO
!           IVAL : ADRESSE DU DEBUT DU NOEUD INO DANS .NUEQ
                ival=zi(jprno-1+(ino-1)*(nec+2)+1)
                ncmp=zi(jprno-1+(ino-1)*(nec+2)+2)
                if (ncmp .eq. 0) goto 60
                do iec = 1, nec
                    tabec(iec)=zi(jprno-1+(ino-1)*(nec+2)+2+iec)
                end do
                if (ncmp .eq. 0) goto 60
!
                icompt=0
                do icmp = 1, ncmpmx
                    if (exisdg(tabec,icmp)) then
                        icompt=icompt+1
                        ieq=nueq(ival-1+icompt)
                        nucmp=nume_cmp(icmp)
                        if (.not.matd) then
                            ieql=ieq
                        else
                            ieql=zi(jnugl+ieq-1)
                        endif
                        if (nucmp .gt. 0) ivec(ieql,nucmp)=1
                    endif
                end do
 60             continue
            end do
 70         continue
        end do
        AS_DEALLOCATE(vi=nume_cmp)
!
!
!     -- CAS NUME_DDL_GENE :
!     ----------------------
    else
        call jeveuo(nomnu//'.DESC', 'L', vi=desc)
        ASSERT(desc(1).eq.2)
        if (matd) ASSERT(.false.)
        call jeveuo(nomnu//'.DEEQ', 'L', vi=deeq)
        call jelira(nomnu//'.DEEQ', 'LONMAX', nleq)
        nleq=nleq/2
!       VERIFICATION DE LA COMPATIBILITE DU NB D EQUATIONS
        ASSERT(nleq.eq.neq)
        do ieq = 1, neq
            numno=deeq(1+2*ieq-1)
            do j = 1, nbcmp
                if (lnocmp(j) .eq. 'LAGR' .and. numno .lt. 0) ivec(ieq,j)= 1
                if (lnocmp(j) .eq. 'GENE' .and. numno .gt. 0) ivec(ieq,j)= 1
            end do
        end do
    endif
!
    call jedema()
end subroutine
