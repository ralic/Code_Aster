subroutine mstget(matrix, keywordfactz, nbocc, ddlsta)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/select_dof.h"
#include "asterfort/getnode.h"
#include "asterfort/pteddl.h"
#include "asterfort/rgndas.h"
#include "asterfort/typddl.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
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
    character(len=*), intent(in) :: keywordfactz
    character(len=*), intent(in) :: matrix
    integer, intent(in) :: nbocc
    integer, intent(inout) :: ddlsta(*)

!     ------------------------------------------------------------------
!     OPERATEUR : MODE_STATIQUE
!     RECUPERATION DES DDL SUR LESQUELS IL FAUT CALCULER DES MODES STATS
!     ------------------------------------------------------------------
! IN  : MATRIX : NOM DE LA MATRICE ASSEMBLEE DU SYSTEME
! IN  : KEYWORDFACT : MOT FACTEUR  'MODE_STAT', 'FORCE_NODALE', 'PSEUDO_MODE'
! IN  : NBOCC  : NOMBRE DE MOT CLE FACTEUR
! OUT : DDLSTA : TABLEAU DES DDL
!                DDLSTA(I) = 0  PAS DE MODE STATIQUE POUR LE DDL I
!                DDLSTA(I) = 1  MODE STATIQUE POUR LE DDL I
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: neq
    character(len=8) :: mesh
    character(len=14) :: nume_ddl
    character(len=16) :: keywordfact
    character(len=24) :: list_node
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: iocc, ic, ieq
    integer :: iii, imode, ind, jcmp
    integer :: jind1, jind2, lacb, lact, lblo, lcmp
    integer :: llag, na, nac, nba
    integer :: nbb, nbl, nbliai, ncmp, nd, nt
    integer :: nb_node, nsc, ntc
    integer, pointer :: list_equa(:) => null()
    integer, pointer :: p_list_node(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
!
    keywordfact = keywordfactz
    call dismoi('NOM_MAILLA', matrix, 'MATR_ASSE', repk=mesh)
    call dismoi('NOM_NUME_DDL', matrix, 'MATR_ASSE', repk=nume_ddl)
    call dismoi('NB_EQUA', matrix, 'MATR_ASSE', repi=neq)
    call wkvect('&&MSTGET.LISTE.LAGRAN', 'V V I', neq, llag)
    call wkvect('&&MSTGET.LISTE.BLOQUE', 'V V I', neq, lblo)
    call wkvect('&&MSTGET.LISTE.ACTIF', 'V V I', neq, lact)
    call wkvect('&&MSTGET.LISTE.ACTBLO', 'V V I', neq, lacb)
    call typddl('LAGR', nume_ddl, neq, zi(llag), nba,&
                nbb, nbl, nbliai)
    call typddl('BLOQ', nume_ddl, neq, zi(lblo), nba,&
                nbb, nbl, nbliai)
    call typddl('ACTI', nume_ddl, neq, zi(lact), nba,&
                nbb, nbl, nbliai)
    call typddl('ACBL', nume_ddl, neq, zi(lacb), nba,&
                nbb, nbl, nbliai)
    list_node = '&&MSTGET.LIST_NODE'
!
    if (keywordfact .eq. 'MODE_STAT') then
        jind1 = lblo
        jind2 = lact
    else if (keywordfact.eq.'FORCE_NODALE') then
        jind1 = lact
        jind2 = lblo
    else if (keywordfact.eq.'PSEUDO_MODE') then
        jind1 = lacb
        jind2 = llag
    else if (keywordfact.eq.'MODE_INTERF') then
        jind1 = lblo
        jind2 = lact
    else
        ASSERT(.false.)
    endif
!
    do iocc = 1, nbocc
        if (keywordfact .eq. 'PSEUDO_MODE') then
            call getvtx(keywordfact, 'AXE', iocc=iocc, nbval=0, nbret=na)
            call getvtx(keywordfact, 'DIRECTION', iocc=iocc, nbval=0, nbret=nd)
            if ((na+nd) .ne. 0) goto 10
        endif
!
! ----- Get nodes
!
        AS_ALLOCATE(vi = list_equa, size = neq)
        call getnode(mesh   , keywordfact, iocc, ' ', list_node, &
                     nb_node, elem_excl = .true._1) 
        if (nb_node.eq.0) then
            call utmess('F', 'MODESTAT1_1')
        endif
        call jeveuo(list_node, 'L', vi = p_list_node)
!
! ----- Select dof
!
        call select_dof(list_equa = list_equa, &
                        nume_ddlz = nume_ddl  ,&
                        nb_nodez  = nb_node , list_nodez = p_list_node)
!
! ----- Get all components
!
        call getvtx(keywordfact, 'TOUT_CMP', iocc=iocc, nbval=0, nbret=ntc)
        if (ntc .ne. 0) then
            call wkvect('&&MSTGET.LISTE.CMP', 'V V I', neq, lcmp)
            do ieq = 1, neq
                zi(lcmp+ieq-1) = 1
            end do
        endif
!
! ----- Get list of components
!
        call getvtx(keywordfact, 'AVEC_CMP', iocc=iocc, nbval=0, nbret=nac)
        if (nac .ne. 0) then
            ncmp = -nac
            call wkvect('&&MSTGET.NOM.CMP', 'V V K8', ncmp, jcmp)
            call getvtx(keywordfact, 'AVEC_CMP', iocc=iocc, nbval=ncmp, vect=zk8(jcmp))
            call wkvect('&&MSTGET.LISTE.CMP', 'V V I', neq*ncmp, lcmp)
            call pteddl('NUME_DDL', nume_ddl, ncmp, zk8(jcmp), neq,&
                        tabl_equa = zi(lcmp))
            do ic = 2, ncmp
                ind = (ic-1)*neq
                do ieq = 1, neq
                    zi(lcmp+ieq-1)= max(zi(lcmp+ind+ieq-1),zi(lcmp+ieq-1))
                end do
            end do
        endif
!
! ----- Exclude list of components
!
        call getvtx(keywordfact, 'SANS_CMP', iocc=iocc, nbval=0, nbret=nsc)
        if (nsc .ne. 0) then
            ncmp = -nsc
            call wkvect('&&MSTGET.NOM.CMP', 'V V K8', ncmp, jcmp)
            call getvtx(keywordfact, 'SANS_CMP', iocc=iocc, nbval=ncmp, vect=zk8(jcmp))
            ncmp = ncmp + 1
            zk8(jcmp+ncmp-1) = 'LAGR'
            call wkvect('&&MSTGET.LISTE.CMP', 'V V I', neq*ncmp, lcmp)
            call pteddl('NUME_DDL', nume_ddl, ncmp, zk8(jcmp), neq,&
                        tabl_equa = zi(lcmp))
            do ic = 2, ncmp
                ind = (ic-1)*neq
                do ieq = 0, neq-1
                    zi(lcmp+ieq)= max(zi(lcmp+ind+ieq),zi(lcmp+ieq))
                end do
            end do
            do ieq = 1, neq
                zi(lcmp+ieq-1) = 1 - zi(lcmp+ieq-1)
            end do
        endif
!
! ----- If TOUT = 'OUI', deselect nodes
!        
        call getvtx(keywordfact, 'TOUT', iocc=iocc, nbval=0, nbret=nt)
        if (nt.ne.0) then
            do ieq = 1, neq
                if (zi(jind1+ieq-1).ne.1) then
                    zi(lcmp+ieq-1) = 0
                endif
            end do
        endif
!
! ----- Checking:
! -----   MODE_STAT: dof must been blocked
! -----   FORCE_NODALE: dof must been free
! -----   PSEUDO_MODE: dof must been physical type
! -----   MODE_INTERF: dof must been blocked
!
        do ieq = 1, neq
            imode = list_equa(ieq) * zi(lcmp+ieq-1)
            iii = zi(jind2+ieq-1) * imode
            if (iii .ne. 0) then
                call rgndas(nume_ddl, ieq, l_print = .true.)
                if (keywordfact .eq. 'MODE_STAT') then
                    call utmess('E', 'MODESTAT1_2')
                else if (keywordfact.eq.'FORCE_NODALE') then
                    call utmess('E', 'MODESTAT1_3')
                else if (keywordfact.eq.'PSEUDO_MODE') then
                    call utmess('E', 'MODESTAT1_4')
                else if (keywordfact.eq.'MODE_INTERF') then
                    call utmess('E', 'MODESTAT1_5')
                else
                    ASSERT(.false.)
                endif
                imode = 0
            endif
            ddlsta(ieq)= max(ddlsta(ieq),imode)
        end do
!
!        --- NETTOYAGE ---
!
        call jedetr(list_node)
        AS_DEALLOCATE(vi = list_equa)
        call jedetr('&&MSTGET.LISTE.CMP')
        call jedetr('&&MSTGET.NOM.CMP')
!
 10     continue
    end do
    call jedetr('&&MSTGET.LISTE.LAGRAN')
    call jedetr('&&MSTGET.LISTE.BLOQUE')
    call jedetr('&&MSTGET.LISTE.ACTIF')
    call jedetr('&&MSTGET.LISTE.ACTBLO')
!
    call jedema()
end subroutine
