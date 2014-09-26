subroutine nudeeq(mesh, nb_node_mesh, nb_lagr_mesh, base, nume_ddl,&
                  neq , igds        , iddlag)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
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
    character(len=8), intent(in) :: mesh
    integer, intent(in) :: nb_node_mesh
    integer, intent(in) :: nb_lagr_mesh
    character(len=2), intent(in) :: base
    character(len=14), intent(in) :: nume_ddl
    integer, intent(in) :: neq
    integer, intent(in) :: igds
    integer, intent(in) :: iddlag
!
! --------------------------------------------------------------------------------------------------
!
! Numbering 
!
! Set DEEQ object (with non-physical nodes)
! Set DELG object
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh           : name of mesh
! In  nb_node_mesh   : number of (physical) nodes in mesh
! In  nb_lagr_mesh   : number of Lagrange (non-physical) nodes in mesh - Substructuring
! In  base           : JEVEUX base to create objects
!                      base(1:1) => PROF_CHNO objects
!                      base(2:2) => NUME_DDL objects
! In  nume_ddl       : name of nume_ddl object
! In  neq            : number of equations
! In  igds           : index of GRANDEUR used to numbering
! In  iddlag         : adresse of DSCLAG object (see nueffe)
!
! Object   : PROF_CHNO.DEEQ 
! Dimension: vector of size (2*neq)
! Contains : for ieq = 1,neq
!
!   DEEQ((ieq-1)*2+1)
!       SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST PHYS.:
!           +NUMERO DU NOEUD
!       SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST UN LAGRANGE DE BLOCAGE :
!           +NUMERO DU NOEUD PHYS. BLOQUE
!       SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST UN LAGRANGE DE LIAISON :
!           0
!   DEEQ((ieq-1)*2+2)
!       SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST PHYS.:
!           + NUM. DANS L'ORDRE DU CATAL. DES GRAND. DE LA CMP CORRESPONDANT A L'EQUATION IDDL.
!       SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST UN LAGRANGE DE BLOCAGE :
!           - NUM. DANS L'ORDRE DU CATAL. DES GRAND. DE LA CMP CORRESPONDANT AU BLOCAGE.
!       SI LE NOEUD SUPPORT DE L'EQUA. IDDL EST UN LAGRANGE DE LIAISON :
!           0
!
! Object   : NUME_DDL.NUME.DELG 
! Dimension: vector of size ( neq)
! Contains : 
!
!   DELG(ieq)
!       0  SI LE NOEUD SUPPORT DE L'EQUATION IDDL N'EST PAS UN NOEUD DE LAGRANGE
!       -1 SI LE NOEUD SUPPORT DE L'EQUATION IDDL EST UN "1ER" NOEUD DE LAGRANGE
!       -2 SI LE NOEUD SUPPORT DE L'EQUATION IDDL EST UN "2EME" NOEUD DE LAGRANGE
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_dof_check
    parameter (nb_dof_check=10)
!
    character(len=8) :: nono, nocmp
    character(len=19) :: prof_chno
    character(len=24) :: prno, nueq, deeq
    character(len=19) :: nume_equa
    character(len=24) :: delg
    character(len=24) :: valk(2)
    integer :: i_ligr, iadg, iddl, ieq, ier
    integer :: ilag, i_node, jdeeq, jdelg,  jncmp
    integer :: jprno, jtypl, i_cmp, l, nb_lagr, nb_ligr
    integer :: nb_node, ncmpmx, nddlb, nec, nob, nucmp
    integer :: nuno
    integer, pointer :: lnobloq(:) => null()
    integer, pointer :: p_nueq(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    nume_equa = nume_ddl//'.NUME'
    delg      = nume_equa(1:19)//'.DELG'
    prof_chno = nume_ddl//'.NUME'
    prno      = prof_chno(1:19)//'.PRNO'
    nueq      = prof_chno(1:19)//'.NUEQ'
    deeq      = prof_chno(1:19)//'.DEEQ'
    if (nb_lagr_mesh .gt. 0) then
        call jeveuo(mesh//'.TYPL', 'L', jtypl)
    endif
!
! - Information about GRANDEUR
!
    call jelira(jexnum('&CATA.GD.NOMCMP', igds), 'LONMAX', ncmpmx)
    nec     = nbec(igds)
    ASSERT(ncmpmx .ne. 0)
    ASSERT(nec .ne. 0)
!
! - Create .DELG object
!
    call jedetr(delg)
    call wkvect(delg, base(1:1)//' V I', neq, jdelg)
!
! - Access to NUEQ/DEEQ object
!
    call jeveuo(nueq, 'L', vi = p_nueq)
    call jeveuo(deeq, 'E', jdeeq)
    nb_lagr = 0
!
    call jelira(prno, 'NMAXOC', nb_ligr)
    do i_ligr = 1, nb_ligr
        call jelira(jexnum(prno, i_ligr), 'LONMAX', l)
        if (l .gt. 0) then
            call jeveuo(jexnum(prno, i_ligr), 'L', jprno)
!
! --------- Nb_node: number of nodes
! ---------  i_ligr = 1 => from mesh
! ---------  i_ligr > 1 => from other LIGREL (loads)
!
            nb_node = l/ (nec+2)
            if ((i_ligr.eq.1) .and. (nb_node.ne. (nb_node_mesh+nb_lagr_mesh))) then
                ASSERT(.false.)
            endif
            do i_node = 1, nb_node
                iddl = zi(jprno-1+ (i_node-1)* (nec+2)+1) - 1
                iadg = jprno - 1 + (i_node-1)* (nec+2) + 3
                do i_cmp = 1, ncmpmx
                    if (exisdg(zi(iadg),i_cmp)) then
                        iddl = iddl + 1
                        ieq = p_nueq(iddl)
                        if (i_ligr .eq. 1) then
                            ASSERT((nb_lagr_mesh.le.0) .or. (ieq.eq.iddl))
                            zi(jdeeq-1+2* (ieq-1)+1) = i_node
                            zi(jdeeq-1+2* (ieq-1)+2) = i_cmp
                            zi(jdelg-1+ieq) = 0
                        else
                            ilag = nb_lagr + i_node
                            nob = zi(iddlag+ (ilag-1)*3)
                            nddlb = zi(iddlag+ (ilag-1)*3+1)
                            zi(jdeeq-1+2* (ieq-1)+1) = nob
                            zi(jdeeq-1+2* (ieq-1)+2) = nddlb
                            zi(jdelg-1+ieq) = -zi(iddlag+ (ilag-1)*3+ 2)
                        endif
                    endif
                end do
            end do
            if (i_ligr .gt. 1) then
                nb_lagr = nb_lagr + nb_node
            endif
        endif
    end do
!
! - Check if dof are only once 'blocked'
!
    AS_ALLOCATE(vi=lnobloq, size=nb_node_mesh*nb_dof_check)
    do ieq = 1, neq
        nuno  = zi(jdeeq-1+2* (ieq-1)+1)
        nucmp = zi(jdeeq-1+2* (ieq-1)+2)
        if ((nuno.gt.0) .and. (nucmp.lt.0) .and. (nucmp.ge.-nb_dof_check)) then
            nucmp = -nucmp
            lnobloq((nuno-1)*nb_dof_check+nucmp) = lnobloq((nuno-1)*nb_dof_check+ nucmp) + 1
        endif
    end do
!
    ier = 0
    do nuno = 1, nb_node_mesh
        do nucmp = 1, nb_dof_check
            if (lnobloq((nuno-1)*nb_dof_check+nucmp) .gt. 2) then
                ier = ier + 1
                call jenuno(jexnum(mesh//'.NOMNOE', nuno), nono)
                call jeveuo(jexnum('&CATA.GD.NOMCMP', igds), 'L', jncmp)
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
    call jedema()
end subroutine
