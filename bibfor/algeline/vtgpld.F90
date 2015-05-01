subroutine vtgpld(cumul , geomiz, alpha, deplaz, base,&
                  geomfz)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbec.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=4), intent(in) :: cumul
    character(len=*), intent(in) :: geomiz
    real(kind=8), intent(in) :: alpha
    character(len=*), intent(in) :: deplaz
    character(len=1), intent(in) :: base
    character(len=*), intent(in) :: geomfz
!
! --------------------------------------------------------------------------------------------------
!
! Update geometry field with displacement field
!
! --------------------------------------------------------------------------------------------------
!
!   GEOMF = GEOMI + ALPHA * DEPLA if CUMUL = 'CUMU'
!   GEOMF = ALPHA * DEPLA         if CUMUL = 'ZERO'
!
! Only on DX/DY/DZ components
! SI SUR CERTAINS NOEUDS, ON NE TROUVE PAS DE DEPLACEMENT,
! ON LES LAISSE INCHANGES
!
! In  cumul : 'ZERO' OU 'CUMU'
! In  geomi : name of initial geometry field (GEOM_R)
! In  alpha : coefficient
! In  depla : name of displacement field (DEPL_R)
! In  base  : JEVEUX base to create geomf
! In  geomf : name of final geometry field (GEOM_R)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: lili, prno, nueq
    character(len=24) :: lili_name
    integer :: i_ligr_mesh
    integer, pointer :: v_prno(:) => null()
    integer, pointer :: v_nueq(:) => null()
    integer :: desc_gran(10)
    integer :: idx_gd, ldim
    integer :: nb_cmp_gd, nb_node_mesh, nb_cmp_node, nb_ec
    integer :: length_prno
    integer :: i_cmp_glob, i_ec, i_equ, i_node, i_dof
    real(kind=8) :: rdepla
    character(len=8) :: mesh, nomgd, ktype
    character(len=19) :: geomi, depla, geomf, prof_chno
    real(kind=8), pointer :: v_depla(:) => null()
    real(kind=8), pointer :: v_geomf(:) => null()
    real(kind=8), pointer :: v_geomi(:) => null()
    character(len=8), pointer :: p_cata_cmp(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    geomi = geomiz
    geomf = geomfz
    depla = deplaz
!
! - Get mesh informations
!
    call dismoi('NOM_MAILLA', geomi, 'CHAM_NO', repk=mesh)
    call dismoi('NB_NO_MAILLA', mesh, 'MAILLAGE', repi=nb_node_mesh)
!
! - Checking
!
    call dismoi('NOM_GD', geomi, 'CHAM_NO', repk=nomgd)
    ASSERT(nomgd(1:6).eq.'GEOM_R')
    call dismoi('NOM_GD', depla, 'CHAM_NO', repk=nomgd)
    ASSERT(nomgd(1:6).eq.'DEPL_R')
    call jelira(depla//'.VALE', 'TYPE', cval=ktype)
    ASSERT(ktype(1:1).eq.'R')
    call jelira(geomi//'.VALE', 'LONMAX', ldim)
    ASSERT(ldim/3.eq.nb_node_mesh)
!
! - For new field: copy
!
    call copisd('CHAMP_GD', base, geomi, geomf)
!
! - Access
!
    call jeveuo(geomi//'.VALE', 'L', vr=v_geomi)
    call jeveuo(geomf//'.VALE', 'E', vr=v_geomf)
    call jeveuo(depla//'.VALE', 'L', vr=v_depla)
!
! - GRANDEUR
!
    call dismoi('NUM_GD', depla, 'CHAM_NO', repi=idx_gd)
    ASSERT(idx_gd.ne.0)
    nb_ec = nbec(idx_gd)
    ASSERT(nb_ec.le.10)
!
! - Access to catalog
!
    call jelira(jexnum('&CATA.GD.NOMCMP', idx_gd), 'LONMAX', nb_cmp_gd)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', idx_gd), 'L', vk8 = p_cata_cmp)
    ASSERT(p_cata_cmp(1).eq.'DX')
    ASSERT(p_cata_cmp(2).eq.'DY')
    ASSERT(p_cata_cmp(3).eq.'DZ')
!
! - Get PROF_CHNO
!
    call dismoi('PROF_CHNO', depla, 'CHAM_NO', repk=prof_chno)
    lili = prof_chno(1:19)//'.LILI'
    prno = prof_chno(1:19)//'.PRNO'
    nueq = prof_chno(1:19)//'.NUEQ'
    call jeveuo(nueq, 'L', vi = v_nueq)
!
! - Get PRNO object for mesh
!
    i_ligr_mesh = 1
    call jenuno(jexnum(lili, i_ligr_mesh), lili_name)
    ASSERT(lili_name .eq. '&MAILLA')
    call jeveuo(jexnum(prno, i_ligr_mesh), 'L', vi = v_prno)
    call jelira(jexnum(prno, i_ligr_mesh), 'LONMAX', length_prno)
    ASSERT(length_prno/(nb_ec+2).eq.nb_node_mesh)
!
! - Update
!
    do i_node = 1, nb_node_mesh
        i_dof       = v_prno((nb_ec+2)*(i_node-1)+1) - 1
        nb_cmp_node = v_prno((nb_ec+2)*(i_node-1)+2)
        if (nb_cmp_node .ne. 0) then
            desc_gran(1:10) = 0
            do i_ec = 1, nb_ec
                desc_gran(i_ec) = v_prno((nb_ec+2)*(i_node-1)+2+i_ec)
            end do
            do i_cmp_glob = 1, 3
                if (exisdg(desc_gran,i_cmp_glob)) then
                    i_dof  = i_dof + 1
                    i_equ  = v_nueq(i_dof)
                    rdepla = v_depla(i_equ)
                    if (cumul .eq. 'CUMU') then
                        v_geomf(3*(i_node-1)+i_cmp_glob) = &
                            v_geomi(3*(i_node-1)+i_cmp_glob)+alpha*rdepla
                    else if (cumul.eq.'ZERO') then
                        v_geomf(3*(i_node-1)+i_cmp_glob) = &
                            alpha*rdepla
                    else
                        ASSERT(.false.)
                    endif
                endif
            end do
        endif
    end do
!
    call jedema()
end subroutine
