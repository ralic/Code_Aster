subroutine imppiv(nu, ieq)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
#include "asterfort/utmess.h"
!
    integer :: ieq
    character(len=*) :: nu
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
!     IMPRIMER (FICHIER 'MESSAGE') LE NOM DES NOEUDS ET DES DDLS
!     IMPLIQUES DANS UNE EQUATION DE SYSTEME LINEAIRE DE TYPE
!     LAGRANGE / LIAISON_DDL
! ----------------------------------------------------------------------
! IN  : NU     : NOM D'UN NUME_DDL OU D'UN PROF_CHNO
! IN  : IEQ    : NUMERO D'UNE EQUATION DANS UN SYSTEME ASSEMBLE
! ----------------------------------------------------------------------
    integer :: gd, nec, jprno, jnueq, ifm
    integer :: nlili, i, ilo, nbno, ino, ideb, ncmp, icmp, iieq, nuli
    integer :: nuno
    integer :: nbmas, k, kk, jnuno, kno
    character(len=8) :: noma, nomeq, nomno
    character(len=19) :: prno, ligrel
    logical :: trouve
!
! DEB-------------------------------------------------------------------
!
    call jemarq()
    ifm=iunifi('MESSAGE')
!
    call dismoi('NOM_MAILLA', nu, 'NUME_DDL', repk=noma)
    call dismoi('NUM_GD_SI', nu, 'NUME_DDL', repi=gd)
    prno( 1:14) = nu
    prno(15:19) = '.NUME'
    nec = nbec(gd)
!
    call jeveuo(prno//'.NUEQ', 'L', jnueq)
!
    call jelira(prno//'.PRNO', 'NMAXOC', nlili)
    trouve = .false.
    do i = 1, nlili
        call jenuno(jexnum(prno//'.LILI', i), ligrel)
        call jelira(jexnum(prno//'.PRNO', i), 'LONMAX', ilo)
        if (ilo .eq. 0) goto 10
        call jeveuo(jexnum(prno//'.PRNO', i), 'L', jprno)
        nbno = ilo / ( nec + 2 )
        do ino = 1, nbno
            ideb = zi(jprno-1+(ino-1)*(nec+2)+1)
            ncmp = zi(jprno-1+(ino-1)*(nec+2)+2)
            do icmp = 1, ncmp
                iieq = zi(jnueq-1+ideb-1+icmp)
                if (ieq .eq. iieq) then
                    trouve = .true.
                    nuli = i
                    nuno = ino
                    goto 9998
                endif
            end do
        end do
 10     continue
    end do
!
9998 continue
!
    if (.not.trouve) then
        call codent(ieq, 'D', nomeq)
        call utmess('F', 'UTILITAI2_31', sk=nomeq)
    endif
!
    ASSERT(nuli .ne. 1)
!
!     ON PARCOURT LES MAILLES SUPPLEMENTAIRES DU LIGREL TROUVE
!     POUR IMPRIMER LES CONNECTIVITES DE CES MAILLES :
    call jenuno(jexnum(prno//'.LILI', nuli), ligrel)
    call jelira(ligrel//'.NEMA', 'NMAXOC', nbmas)
    write(ifm,*) ' '
    write(ifm,*) 'IMPRESSION DE LA LISTE DES NOEUDS IMPLIQUES'
    write(ifm,*) 'DANS LA RELATION LINEAIRE SURABONDANTE:'
!
    do k = 1, nbmas
        call jelira(jexnum(ligrel//'.NEMA', k), 'LONMAX', nbno)
!       -- L'OBJET .NEMA CONTIENT LE TYPE_MAILLE AU BOUT :
        if (nbno .eq. 0) goto 777
        nbno=nbno-1
        call jeveuo(jexnum(ligrel//'.NEMA', k), 'L', jnuno)
        trouve=.false.
        do kk = 1, nbno
            if (zi(jnuno-1+kk) .eq. -nuno) trouve=.true.
        end do
        if (.not.trouve) goto 777
        do kk = 1, nbno
            kno=zi(jnuno-1+kk)
            if (kno .gt. 0) then
                call jenuno(jexnum(noma//'.NOMNOE', kno), nomno)
                write(ifm,*) '   - NOEUD: ',nomno
            endif
        end do
777     continue
    end do
    write(ifm,*) ' '
!
    call jedema()
end subroutine
