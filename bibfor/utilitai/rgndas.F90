subroutine rgndas(nu, ieq, nomno, nomcmp, tyddl,&
                  ligrel, infobl)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
!
    integer :: ieq
    character(len=*) :: nu, nomno, nomcmp, tyddl, ligrel, infobl
! person_in_charge: jacques.pellet at edf.fr
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
!     RETROUVER LE NOM DU NOEUD ET LE CMP CORRESPONDANT A UN NUMERO
!     D'EQUATION DANS UN SYSTEME ASSEMBLE.
! ----------------------------------------------------------------------
! IN  : NU     : NOM D'UN NUME_DDL OU D'UN NUME_DDL_GENE
! IN  : IEQ    : NUMERO D'UNE EQUATION DANS UN SYSTEME ASSEMBLE
! OUT : NOMNO  : NOM DU NOEUD ASSOCIE A IEQ
! OUT : NOMCMP : NOM DE LA CMP ASSOCIE A IEQ
! OUT : TYDDL :   / 'A' : DDL PHYSIQUE (NOEUD, CMP)
!                 / 'B' : LAGRANGE ASOCIE A UN DDL IMPOSE
!                 / 'C' : LAGRANGE ASSOCIE A UNE RELATION LINEAIRE
!                 / 'D' : DDL GENERALISE
! OUT : LIGREL : NOM DU LIGREL SI LE NOEUD EST 'B' OU 'C' (BLANC SINON)
! OUT : INFOBL : INFORMATIONS COMPLEMENTAIRES
! ----------------------------------------------------------------------
    integer :: gd, nec, jprno, ico
    integer :: nlili, i, ilo, nbno, ino, ideb, ncmp
    integer :: icmp, iieq, nuno, nucmp, ncmpmx, iadg1, jrefe
    integer :: inocmp, nuddl, neq, nusst, nulia, jdeeq, jorig, iexi
    character(len=8) :: noma, nomno2, nomcm2, modgen, kn1, kn2
    character(len=19) :: nume
    aster_logical :: trouve, lnuge
    integer, pointer :: desc(:) => null()
    integer, pointer :: nueq(:) => null()
!
! DEB-------------------------------------------------------------------
!
    call jemarq()
    nume(1:14)=nu
    nume(15:19)='.NUME'
    ligrel=' '
    infobl=' '
    nomno=' '
    nomcmp=' '
    tyddl='?'
!
!
!     -- LNUGE : .TRUE.  : NUME EST UN NUME_DDL_GENE
!                .FALSE. : NUME EST UN NUME_DDL
    call jeexin(nume//'.DESC', iexi)
    lnuge=(iexi.gt.0)
!
!
!     -- 1. CAS NUME_DDL :
!     ---------------------
    if (.not.lnuge) then
        call dismoi('NOM_MAILLA', nu, 'NUME_DDL', repk=noma)
        call dismoi('NUM_GD_SI', nu, 'NUME_DDL', repi=gd)
        call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', ncmpmx)
        call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', inocmp)
        call jeveuo(nume//'.DEEQ', 'L', jdeeq)
        call jeveuo(nume//'.NUEQ', 'L', vi=nueq)
!
!       -- 1.1 CAS FACILE : DDL PHYSIQUE :
        nuno=zi(jdeeq-1+2*(ieq-1)+1)
        nuddl=zi(jdeeq-1+2*(ieq-1)+2)
        if (nuno .gt. 0 .and. nuddl .gt. 0) then
            tyddl='A'
            call jenuno(jexnum(noma//'.NOMNOE', nuno), nomno)
            nomcmp=zk8(inocmp-1+nuddl)
            goto 70
!
        endif
!
!       -- 1.2 CAS MOINS FACILE : DDL PORTE PAR NOEUD TARDIF :
!          CALCUL DE TROUVE, NUNO, NUCMP :
        call jelira(nume//'.PRNO', 'NMAXOC', nlili)
        ASSERT(nlili.gt.1)
        nec=nbec(gd)
        trouve=.false.
        do i = 2, nlili
            call jenuno(jexnum(nume//'.LILI', i), ligrel)
            call jelira(jexnum(nume//'.PRNO', i), 'LONMAX', ilo)
            if (ilo .le. 0) goto 30
            call jeveuo(jexnum(nume//'.PRNO', i), 'L', jprno)
            nbno=ilo/(nec+2)
            do ino = 1, nbno
                ideb=zi(jprno-1+(ino-1)*(nec+2)+1)
                ncmp=zi(jprno-1+(ino-1)*(nec+2)+2)
                do icmp = 1, ncmp
                    iieq=nueq(ideb-1+icmp)
                    if (ieq .eq. iieq) then
                        trouve=.true.
                        nuno=ino
                        nucmp=icmp
                        goto 40
!
                    endif
                end do
            end do
 30         continue
        end do
 40     continue
        ASSERT(trouve)
        ASSERT(nuno.gt.0)
!
!
!       -- CALCUL DE NOMCMP :
        iadg1=jprno-1+(nuno-1)*(nec+2)+3
        ico=0
        do icmp = 1, ncmpmx
            if (exisdg(zi(iadg1),icmp)) then
                ico=ico+1
                if (ico .eq. nucmp) goto 60
            endif
        end do
 60     continue
        ASSERT(icmp.le.ncmpmx)
        nomcmp=zk8(inocmp-1+icmp)
        ASSERT(nomcmp.eq.'LAGR')
!
!
!       -- ON REMPLIT INFOBL:
        nuno=zi(jdeeq-1+2*(ieq-1)+1)
        nuddl=zi(jdeeq-1+2*(ieq-1)+2)
!         -- SI NUNO = 0  C'EST UNE LIAISON_DDL :
        if (nuno .eq. 0) then
            tyddl='C'
            infobl='NOEUD DE LIAISON_DDL'
        else
            tyddl='B'
            call jenuno(jexnum(noma//'.NOMNOE', nuno), nomno2)
            nomcm2=zk8(inocmp-1+(-nuddl))
            infobl='NOEUD: '//nomno2//' CMP: '//nomcm2
        endif
!
!
!     -- 2. CAS NUME_DDL_GENE :
!     --------------------------
    else
        tyddl='D'
        ligrel=' '
        infobl=' '
        call jeveuo(nume//'.DESC', 'L', vi=desc)
        ASSERT(desc(1).eq.2)
        call jeveuo(nume//'.DEEQ', 'L', jdeeq)
        call jelira(nume//'.DEEQ', 'LONMAX', neq)
        neq=neq/2
        nuno=zi(jdeeq+2*ieq-1)
        nucmp=zi(jdeeq+2*ieq-2)
        if (nuno .gt. 0) then
            call jeveuo(jexnum(nume//'.ORIG', 1), 'L', jorig)
            nusst=zi(jorig+nuno-1)
            call jeexin(nume//'.REFE', iexi)
            if (iexi .gt. 0) then
                call jeveuo(nume//'.REFE', 'L', jrefe)
            else
                call jeveuo(nume//'.REFN', 'L', jrefe)
            endif
            modgen=zk24(jrefe)
            call jeexin(modgen//'      .MODG.SSNO', iexi)
            if (iexi .gt. 0) then
                call jenuno(jexnum(modgen//'      .MODG.SSNO', nusst), nomno)
            else
                nomno='UNFOUND'
            endif
            nomcmp(1:3)='GEN'
            write (nomcmp(4:8),'(I5)')nucmp
        else
            nuno=-nuno
            call jeveuo(jexnum(nume//'.ORIG', 2), 'L', jorig)
            nulia=zi(jorig+nuno+1)
            nomno(1:3)='TAR'
            write (nomno(4:8),'(I5)')nuno
            nomcmp(1:3)='LAG'
            write (nomcmp(4:8),'(I5)')nucmp
            write (nomcmp(4:8),'(I5)')nucmp
            write (kn1(1:7),'(I7)')ieq
            write (kn2(1:4),'(I4)')nulia
            infobl='EQUATION:'//kn1//'   LIAISON:'//kn2
        endif
    endif
!
 70 continue
    call jedema()
end subroutine
