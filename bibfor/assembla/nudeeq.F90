subroutine nudeeq(base, nu14, neq, gds, iddlag)
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
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    character(len=14) :: nu14
    character(len=2) :: base
    integer :: neq, gds, iddlag
! ----------------------------------------------------------------------
!     BUT : CREER LES OBJETS .DEEQ ET .DELG DANS UN PROF_CHNO.
!
!     IN:
!     BASE    : BASE(1:1) : BASE POUR CREER LE NUME_DDL
!                    (SAUF LE PROF_CHNO)
!             : BASE(2:2) : BASE POUR CREER LE PROF_CHNO
!     NU14 : NOM D'UN NUME_DDL
!     NEQ    : NOMBRE D'EQUATIONS (OU DE DDL) DU PROF_CHNO
!     GDS    : NUMERO DE LA GRANDEUR SIMPLE ASSOCIEE AU CHAMP.
!     IDDLAG : ADRESSE DE L'OBJET DSCLAG (VOIR NUEFFE)
!
!     OUT:
!     NU14 EST COMPLETE DE L'OBJET ".NUME.DEEQ" V(I) DIM=2*NEQ
!         (CET OBJET EST DETRUIT S'IL EXISTE DEJA).
!     V((IDDL-1)*2+1)--> SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST PHYS.:
!                           +NUMERO DU NOEUD
!                        SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST UN
!                        LAGRANGE DE BLOCAGE :
!                           +NUMERO DU NOEUD PHYS. BLOQUE
!                        SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST UN
!                        LAGRANGE DE LIAISON :
!                            0
!     V((IDDL-1)*2+2)--> SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST PHYS.:
!                           + NUM. DANS L'ORDRE DU CATAL. DES GRAND.
!                           DE LA CMP CORRESPONDANT A L'EQUATION IDDL.
!                        SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST UN
!                        LAGRANGE DE BLOCAGE :
!                           - NUM. DANS L'ORDRE DU CATAL. DES GRAND.
!                           DE LA CMP CORRESPONDANT AU BLOCAGE.
!                        SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST UN
!                        LAGRANGE DE LIAISON :
!                            0
!     NU14 EST COMPLETE DE L'OBJET ".NUME.DELG" V(I) DIM=NEQ
!         (CET OBJET EST DETRUIT S'IL EXISTE DEJA).
!     V( IDDL ) --> 0  SI LE NOEUD SUPPORT DE L'EQUATION IDDL N'EST PAS
!                         UN NOEUD DE LAGRANGE
!                   -1 SI LE NOEUD SUPPORT DE L'EQUATION IDDL EST UN
!                         "1ER" NOEUD DE LAGRANGE
!                   -2 SI LE NOEUD SUPPORT DE L'EQUATION IDDL EST UN
!                         "2EME" NOEUD DE LAGRANGE
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
!
    character(len=8) :: ma, nono, nocmp
    character(len=19) :: numeqa
    character(len=24) :: valk(2)
!
!
!
!-----------------------------------------------------------------------
    integer :: i, iadg, iddl, ieq, ier
    integer :: ilag, j, jdeeq, jdelg,  jncmp, jnueq
    integer :: jprno, jtypl, k, l, nblag, nbligr, nbnl
    integer :: nbnm, nbno, ncmpmx, nddlb, nec, nob, nucmp
    integer :: nuno
    integer, pointer :: lnobloq(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    numeqa=nu14//'.NUME'
    call dismoi('NOM_MAILLA', numeqa, 'NUME_EQUA', repk=ma)
    call dismoi('NB_NO_MAILLA', ma, 'MAILLAGE', repi=nbnm)
    call dismoi('NB_NL_MAILLA', ma, 'MAILLAGE', repi=nbnl)
    if (nbnl .gt. 0) call jeveuo(ma//'.TYPL', 'L', jtypl)
!
!
!     - ALLOCATION DE ".DEEQ":
    call jedetr(numeqa//'.DEEQ')
    call wkvect(numeqa//'.DEEQ', base(2:2)//' V I', 2*neq, jdeeq)
!
!     - ALLOCATION DE ".DELG":
    call jedetr(numeqa//'.DELG')
    call wkvect(numeqa//'.DELG', base(1:1)//' V I', neq, jdelg)
!
!
!     - ADRESSE DE ".NUEQ":
    call jeveuo(numeqa//'.NUEQ', 'L', jnueq)
!
    call jelira(jexnum('&CATA.GD.NOMCMP', gds), 'LONMAX', ncmpmx)
    nec = nbec(gds)
    if (ncmpmx .eq. 0) then
        call utmess('F', 'ASSEMBLA_24')
    endif
    if (nec .eq. 0) then
        call utmess('F', 'ASSEMBLA_25')
    endif
    nblag = 0
!
!
    call jelira(numeqa//'.PRNO', 'NMAXOC', nbligr)
    do i = 1, nbligr
        call jelira(jexnum(numeqa//'.PRNO', i), 'LONMAX', l)
        if (l .gt. 0) then
            call jeveuo(jexnum(numeqa//'.PRNO', i), 'L', jprno)
!---- NBNO : SI I=1 --> NOMBRE DE NOEUDS DU MAILLAGE
!            SI I>1 --> NOMBRE DE NOEUDS SUPPLEMENTAIRES DU LIGREL I
            nbno = l/ (nec+2)
            if ((i.eq.1) .and. (nbno.ne. (nbnm+nbnl))) then
                call utmess('F', 'CALCULEL_2')
            endif
!
            do j = 1, nbno
!--- J : SI I=1 --> NUMERO DU NOEUD DU MAILLAGE
!        SI I>1 --> NUMERO DU NOEUD SUPPLEMENTAIRE DU
!                   LIGREL I (CHANGE DE SIGNE).
                iddl = zi(jprno-1+ (j-1)* (nec+2)+1) - 1
                iadg = jprno - 1 + (j-1)* (nec+2) + 3
                do k = 1, ncmpmx
                    if (exisdg(zi(iadg),k)) then
                        iddl = iddl + 1
                        ieq = zi(jnueq-1+iddl)
!
                        if (i .eq. 1) then
                            ASSERT((nbnl.le.0) .or. (ieq.eq.iddl))
                            zi(jdeeq-1+2* (ieq-1)+1) = j
                            zi(jdeeq-1+2* (ieq-1)+2) = k
                            zi(jdelg-1+ieq) = 0
                        else
                            ilag = nblag + j
                            nob = zi(iddlag+ (ilag-1)*3)
                            nddlb = zi(iddlag+ (ilag-1)*3+1)
                            zi(jdeeq-1+2* (ieq-1)+1) = nob
                            zi(jdeeq-1+2* (ieq-1)+2) = nddlb
                            zi(jdelg-1+ieq) = -zi(iddlag+ (ilag-1)*3+ 2)
                        endif
!
                    endif
                end do
            end do
            if (i .gt. 1) nblag = nblag + nbno
        endif
    end do
!
!
!     -- ON VERIFIE QUE LES DDLS BLOQUES NE SONT PAS BLOQUES
!        PLUSIEURS FOIS (ON NE REGARDE QUE LES 10 1ERES CMPS):
!     -------------------------------------------------------
    AS_ALLOCATE(vi=lnobloq, size=nbnm*10)
    do ieq = 1, neq
        nuno = zi(jdeeq-1+2* (ieq-1)+1)
        nucmp = zi(jdeeq-1+2* (ieq-1)+2)
        if ((nuno.gt.0) .and. (nucmp.lt.0) .and. (nucmp.ge.-10)) then
            nucmp = -nucmp
            lnobloq((nuno-1)*10+nucmp) = lnobloq((nuno-1)*10+ nucmp) + 1
        endif
    end do
!
    ier = 0
    do nuno = 1, nbnm
        do nucmp = 1, 10
            if (lnobloq((nuno-1)*10+nucmp) .gt. 2) then
                ier = ier + 1
                call jenuno(jexnum(ma//'.NOMNOE', nuno), nono)
                call jeveuo(jexnum('&CATA.GD.NOMCMP', gds), 'L', jncmp)
                nocmp = zk8(jncmp-1+nucmp)
                valk(1) = nono
                valk(2) = nocmp
                call utmess('E', 'ASSEMBLA_26', nk=2, valk=valk)
            endif
        end do
    end do
    ASSERT(ier.le.0)
    AS_DEALLOCATE(vi=lnobloq)
!
!
    call jedema()
end subroutine
