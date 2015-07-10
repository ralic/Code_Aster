subroutine cfmmap(mesh, sdcont_defi, sdcont_solv)
!
implicit none
!
#include "asterfort/apcrsd.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmar.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: sdcont_defi
    character(len=24), intent(in) :: sdcont_solv
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! Continue/Discrete method - Prepare pairing datastructures
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  sdcont_solv      : name of contact solving datastructure
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: sdappa
    integer :: ifm, niv
    integer :: nb_cont_zone, nt_poin, model_ndim, nt_elem_node, nb_cont_elem, nb_cont_node
    integer :: nb_node_mesh
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> . Prepare pairing datastructures for DISCRETE/CONTINUE methods'
    endif
!
! - Parameters
!
    nb_cont_zone = cfdisi(sdcont_defi,'NZOCO' )
    nb_cont_node = cfdisi(sdcont_defi,'NNOCO' )
    nt_poin      = cfdisi(sdcont_defi,'NTPT'  )
    model_ndim   = cfdisi(sdcont_defi,'NDIM'  )
    nb_cont_elem = cfdisi(sdcont_defi,'NMACO' )
    nt_elem_node = cfdisi(sdcont_defi,'NTMANO')
    call dismoi('NB_NO_MAILLA', mesh, 'MAILLAGE', repi=nb_node_mesh)
!
! - Pairing datastructure
!
    sdappa = sdcont_solv(1:14)//'.APPA'
!
! - Create pairing datastructure
!
    call apcrsd(sdappa      , nb_cont_zone, nt_poin, nb_cont_elem, nb_cont_node,&
                nt_elem_node, nb_node_mesh)
!
! - Fill pairing datastructure
!
    call cfmmar(sdcont_defi , sdcont_solv , nb_cont_zone, model_ndim, nt_poin,&
                nb_cont_elem, nb_cont_node, nt_elem_node)
!
end subroutine
