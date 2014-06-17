function exicp(model, l_affe_all, list_elem_affe, nb_elem_affe)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/teattr.h"
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=8), intent(in) :: model
    character(len=24), intent(in) :: list_elem_affe
    logical, intent(in) :: l_affe_all
    integer, intent(in) :: nb_elem_affe
    logical :: exicp
!
! --------------------------------------------------------------------------------------------------
!
!     RENVOIE .TRUE. SI PARMI LES ELEMENTS FINIS ASSOCIES AUX MAILLES
!     DONNEES, IL EN EXISTE UN QUI SOIT EN C_PLAN
!     SI AUCUNE MAILLE N'EST DONNEE EN ENTREE (nb_elem_affe=0), ALORS
!     ON REGARDE TOUTES LES MAILLES DU MAILLAGE
!
! IN  MODELE : NOM DU MODELE
! IN  list_elem_affe : LISTE DES MAILLES SUR LESQUELLES ON FAIT LE TEST
! IN  nb_elem_affe   : NOMBRE DE MAILLES DANS list_elem_affe
!              SI nb_elem_affe = 0  ON FAIT LE TEST SUR TOUTES LES MAILLES DU
!              MAILLAGE ASSOCIÉE AU MODELE
! OUT EXICP : .TRUE. SI C_PLAN TROUVE
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_elem, nb_elem_mesh, iret, ielem, nume_elem, nutyel
    integer :: j_elem_affe
    character(len=8) :: mesh, dmo, dma
    character(len=16) :: notype, typmod
    integer, pointer :: maille(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    exicp = .false.
!
! - Access to model and mesh
!
    call jeveuo(model//'.MAILLE', 'L', vi=maille)
    call dismoi('NOM_MAILLA', model(1:8), 'MODELE', repk=mesh)
    call dismoi('NB_MA_MAILLA', mesh, 'MAILLAGE', repi=nb_elem_mesh)
!
! - Mesh affectation
!
    if (l_affe_all) then
        nb_elem = nb_elem_mesh
    else
        call jeveuo(list_elem_affe, 'L', j_elem_affe)
        nb_elem = nb_elem_affe
    endif
!
! - Loop on elements
!
    do ielem = 1, nb_elem
!
! ----- Current element
!
        if (l_affe_all) then
            nume_elem = ielem
        else
            nume_elem = zi(j_elem_affe-1+ielem)
        endif
!
! ----- Access to element type
!
        nutyel = maille(nume_elem)
!
        if (nutyel .ne. 0) then
            call jenuno(jexnum('&CATA.TE.NOMTE', nutyel), notype)
            call teattr('S', 'DIM_TOPO_MODELI', dmo, iret, typel=notype)
            call teattr('S', 'DIM_TOPO_MAILLE', dma, iret, typel=notype)
            if (dmo .eq. dma) then
                call teattr('C', 'TYPMOD', typmod, iret, typel=notype)
                if (iret .eq. 0) then
                    if (typmod(1:6) .eq. 'C_PLAN') then
                        exicp = .true.
                        goto 999
                    endif
                endif
            endif
        endif
    end do
!
999 continue
    call jedema()
end function
