subroutine nudeeq(mesh, nb_node_mesh, nb_node_subs, nume_ddl, nb_equa,&
                  igds, iddlag)
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
    integer, intent(in) :: nb_node_subs
    character(len=14), intent(in) :: nume_ddl
    integer, intent(in) :: nb_equa
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
! In  nb_node_subs   : number of (non-physical) nodes in mesh for substructuring
! In  nume_ddl       : name of nume_ddl object
! In  nb_equa        : number of equations
! In  igds           : index of GRANDEUR used to numbering
! In  iddlag         : adresse of DSCLAG object (see nueffe)
!
! Object   : PROF_CHNO.DEEQ 
! Dimension: vector of size (2*nb_equa)
! Contains : for i_equa = 1,nb_equa
!
!   DEEQ((i_equa-1)*2+1)
!       SI LE NOEUD SUPPORT DE L'EQUA. i_equa EST PHYS.:
!           +NUMERO DU NOEUD
!       SI LE NOEUD SUPPORT DE L'EQUA. i_equa EST UN LAGRANGE DE BLOCAGE :
!           +NUMERO DU NOEUD PHYS. BLOQUE
!       SI LE NOEUD SUPPORT DE L'EQUA. i_equa EST UN LAGRANGE DE LIAISON :
!           0
!   DEEQ((i_equa-1)*2+2)
!       SI LE NOEUD SUPPORT DE L'EQUA. i_equa EST PHYS.:
!           + NUM. DANS L'ORDRE DU CATAL. DES GRAND. DE LA CMP CORRESPONDANT A L'EQUATION IDDL.
!       SI LE NOEUD SUPPORT DE L'EQUA. i_equa EST UN LAGRANGE DE BLOCAGE :
!           - NUM. DANS L'ORDRE DU CATAL. DES GRAND. DE LA CMP CORRESPONDANT AU BLOCAGE.
!       SI LE NOEUD SUPPORT DE L'EQUA. i_equa EST UN LAGRANGE DE LIAISON :
!           0
!
! Object   : NUME_EQUA.DELG 
! Dimension: vector of size (nb_equa)
! Contains : for i_equa = 1,nb_equa
!
!   DELG(i_equa)
!        0 SI LE NOEUD SUPPORT DE L'EQUATION i_equa N'EST PAS UN NOEUD DE LAGRANGE
!       -1 SI LE NOEUD SUPPORT DE L'EQUATION i_equa EST UN "1ER" NOEUD DE LAGRANGE
!       -2 SI LE NOEUD SUPPORT DE L'EQUATION i_equa EST UN "2EME" NOEUD DE LAGRANGE
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_cmp_chck
    parameter (nb_cmp_chck=10)
!
    character(len=8) :: nono, nocmp
    character(len=19) :: prof_chno
    character(len=24) :: prno, nueq, deeq
    character(len=19) :: nume_equa
    character(len=24) :: delg
    character(len=24) :: valk(2)
    integer :: i_ligr, iadg, i_dof, ier, i_equ
    integer :: ilag, jncmp
    integer :: jprno, jtypl, length_prno, nb_lagr, nb_ligr
    integer :: nb_node, ncmpmx, nddlb, nec, nob
    integer :: i_node, i_cmp_glob, i_cmp_chck
    integer, pointer :: lnobloq(:) => null()
    integer, pointer :: p_nueq(:) => null()
    integer, pointer :: p_deeq(:) => null()
    integer, pointer :: p_delg(:) => null()
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
    if (nb_node_subs .gt. 0) then
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
! - Access to NUEQ/DEEQ/DELG objects
!
    call jeveuo(delg, 'E', vi = p_delg)
    call jeveuo(nueq, 'L', vi = p_nueq)
    call jeveuo(deeq, 'E', vi = p_deeq)
    nb_lagr = 0
!
    call jelira(prno, 'NMAXOC', nb_ligr)
    do i_ligr = 1, nb_ligr
        call jelira(jexnum(prno, i_ligr), 'LONMAX', length_prno)
        if (length_prno .gt. 0) then
            call jeveuo(jexnum(prno, i_ligr), 'L', jprno)
!
! --------- Nb_node: number of nodes
! ---------  i_ligr = 1 => from mesh
! ---------  i_ligr > 1 => from other LIGREL (loads)
!
            nb_node = length_prno/(nec+2)
            if ((i_ligr.eq.1) .and. (nb_node.ne.(nb_node_mesh+nb_node_subs))) then
                ASSERT(.false.)
            endif
            do i_node = 1, nb_node
                i_dof = zi(jprno-1+ (i_node-1)*(nec+2)+1) - 1
                iadg  = jprno - 1 + (i_node-1)*(nec+2) + 3
                do i_cmp_glob = 1, ncmpmx
                    if (exisdg(zi(iadg),i_cmp_glob)) then
                        i_dof  = i_dof + 1
                        i_equ  = p_nueq(i_dof)
                        if (i_ligr .eq. 1) then
                            if (p_deeq(2*(i_equ-1)+1).eq.0) then
                                p_deeq(2*(i_equ-1)+1) = i_node
                                p_deeq(2*(i_equ-1)+2) = i_cmp_glob
                            endif
                            p_delg(i_equ) = 0
                        else
                            ilag  = nb_lagr + i_node
                            nob   = zi(iddlag+ (ilag-1)*3)
                            nddlb = zi(iddlag+ (ilag-1)*3+1)
                            p_deeq(2*(i_equ-1)+1) = nob
                            p_deeq(2*(i_equ-1)+2) = nddlb
                            p_delg(i_equ) = -zi(iddlag+ (ilag-1)*3+ 2)
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
    AS_ALLOCATE(vi=lnobloq, size=nb_node_mesh*nb_cmp_chck)
    do i_equ = 1, nb_equa
        i_node     = p_deeq(2*(i_equ-1)+1)
        i_cmp_glob = p_deeq(2*(i_equ-1)+2)
        if ((i_node.gt.0) .and. (i_cmp_glob.lt.0) .and. (i_cmp_glob.ge.-nb_cmp_chck)) then
            i_cmp_glob = -i_cmp_glob
            lnobloq((i_node-1)*nb_cmp_chck+i_cmp_glob) = &
                lnobloq((i_node-1)*nb_cmp_chck+i_cmp_glob) + 1
        endif
    end do
!
    ier = 0
    do i_node = 1, nb_node_mesh
        do i_cmp_chck = 1, nb_cmp_chck
            if (lnobloq((i_node-1)*nb_cmp_chck+i_cmp_chck) .gt. 2) then
                ier = ier + 1
                call jenuno(jexnum(mesh//'.NOMNOE', i_node), nono)
                call jeveuo(jexnum('&CATA.GD.NOMCMP', igds), 'L', jncmp)
                nocmp = zk8(jncmp-1+i_cmp_chck)
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
