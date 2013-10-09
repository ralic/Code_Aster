subroutine rbph02(mailla, numddl, chamno, nomgd, neq,&
                  nbnoeu, objve1, ncmp, objve2, objve3,&
                  objve4)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/posddl.h"
#include "asterfort/reliem.h"
#include "asterfort/wkvect.h"
!
    integer :: nbnoeu, neq
    character(len=8) :: mailla
    character(len=14) :: numddl
    character(len=*) :: chamno
    character(len=24) :: objve1, objve2, objve3, objve4
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     OPERATEUR REST_BASE_PHYS
!               TRAITEMENT DES MOTS CLES "NOEUD" ET "GROUP_NO"
!  IN MAILLA : NOM D'UN MAILLAGE
!  IN CHAMNO : NOM D'UN CHAM_NO  (OU ' ')
!  IN NUMDDL : NOM D'UN NUME_DDL (OU ' ')
!     REMARQUE : IL FAUT NUMDLL OU CHAMNO != ' '
! ----------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer :: jprno, nec, tabec(10), i, idec, inuddl, iad, iec, ino
    integer :: ncmpmx, jnoeu, ncmp, icmp, nunoe, jneq, jcmp, j, nbcmp
    character(len=8) :: motcls(4), typmcl(4), nomgd, nomnoe, nomcmp
    character(len=19) :: prno
!     ------------------------------------------------------------------
!
    motcls(1) = 'GROUP_NO'
    motcls(2) = 'NOEUD'
    motcls(3) = 'GROUP_MA'
    motcls(4) = 'MAILLE'
    typmcl(1) = 'GROUP_NO'
    typmcl(2) = 'NOEUD'
    typmcl(3) = 'GROUP_MA'
    typmcl(4) = 'MAILLE'
!
    call reliem(' ', mailla, 'NU_NOEUD', ' ', 1,&
                4, motcls, typmcl, objve1, nbnoeu)
    call jeveuo(objve1, 'L', jnoeu)
!
    if (numddl .ne. ' ') then
        call dismoi('PROF_CHNO', numddl, 'NUME_DDL', repk=prno)
    else
        ASSERT(chamno.ne.' ')
        call dismoi('PROF_CHNO', chamno, 'CHAM_NO', repk=prno)
    endif
!
    call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nec)
    call dismoi('NB_CMP_MAX', nomgd, 'GRANDEUR', repi=ncmpmx)
    ASSERT(nec .le. 10)
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', iad)
    call jeveuo(jexnum(prno//'.PRNO', 1), 'L', jprno)
!
    call wkvect(objve2, 'V V K8', ncmpmx, jcmp)
!
    neq = 0
    ncmp = 0
    do i = 1, nbnoeu
        ino = zi(jnoeu+i-1)
        do iec = 1, nec
            tabec(iec)= zi(jprno-1+(ino-1)*(nec+2)+2+iec )
        end do
        nbcmp = 0
        do icmp = 1, ncmpmx
            if (exisdg(tabec,icmp)) then
                nbcmp = nbcmp + 1
                do j = 1, ncmp
                    if (zk8(jcmp+j-1) .eq. zk8(iad-1+icmp)) goto 14
                end do
                ncmp = ncmp + 1
                zk8(jcmp-1+ncmp) = zk8(iad-1+icmp)
            endif
 14         continue
        end do
        neq = neq + nbcmp
    end do
!
    call wkvect(objve3, 'V V I', nbnoeu*ncmp, jneq)
    call wkvect(objve4, 'V V I', neq, inuddl)
!
    idec = 0
    do i = 1, nbnoeu
        ino = zi(jnoeu+i-1)
        call jenuno(jexnum(mailla//'.NOMNOE', ino), nomnoe)
        do iec = 1, nec
            tabec(iec)= zi(jprno-1+(ino-1)*(nec+2)+2+iec )
        end do
        do icmp = 1, ncmpmx
            if (exisdg(tabec,icmp)) then
                idec = idec + 1
                nomcmp = zk8(iad-1+icmp)
                if (numddl .ne. ' ') then
                    call posddl('NUME_DDL', numddl, nomnoe, nomcmp, nunoe,&
                                zi(inuddl+idec-1))
                else
                    call posddl('CHAM_NO', chamno, nomnoe, nomcmp, nunoe,&
                                zi(inuddl+idec-1))
                endif
                do j = 1, ncmp
                    if (zk8(jcmp+j-1) .eq. zk8(iad-1+icmp)) then
                        zi(jneq-1+(i-1)*ncmp+j) = 1
                        goto 24
                    endif
                end do
            endif
 24         continue
        end do
    end do
!
end subroutine
