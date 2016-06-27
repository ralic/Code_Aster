subroutine cnvois(mesh      , list_elem , nb_elem, elem_indx_mini, elem_indx_maxi,&
                  conx_inve , elem_neigh)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/jecrec.h"
#include "asterfort/jenuno.h"  
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/jeecra.h"
#include "asterfort/jecroc.h"
#include "asterfort/gtvois.h"
#include "asterfort/utlisi.h"
#include "asterfort/assert.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=8), intent(in) :: mesh
    character(len=24), intent(in) :: conx_inve
    integer, intent(in) :: nb_elem
    integer, intent(in) :: list_elem(nb_elem)
    integer, intent(in) :: elem_indx_mini
    integer, intent(in) :: elem_indx_maxi
    character(len=24), intent(in) :: elem_neigh
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Create object of neigbours of a list of elements
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  conx_inve        : name of object for inverse connectivity
! In  nb_elem          : number of elements
! In  list_elem        : list of elements
! In  elem_indx_mini   : minimum index in list of elements
! In  elem_indx_maxi   : minimum index in list of elements
! In  elem_neigh       : name of object for neigbours of a list of elements
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_elem, i_neigh, aux(1) ,nb_find, lmail(1), jtypma
    integer :: elem_indx, elem_nume
    integer :: list_neigh(4), nb_neigh, nt_neigh
    character(len=8) :: elem_code, elem_type
    integer, pointer :: v_elem_neigh(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jeveuo(mesh//'.TYPMAIL', 'L', jtypma)
!
! - Total number of neighbours
!
    nt_neigh = 0
    do i_elem = 1, nb_elem
        elem_nume = list_elem(i_elem)
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(jtypma+elem_nume-1)), elem_type)
        select case (elem_type)
            case('SEG2')
                nt_neigh = nt_neigh + 2
            case('SEG3')
                nt_neigh = nt_neigh + 2
            case('TRIA3')
                nt_neigh = nt_neigh + 3
            case('TRIA6')
                nt_neigh = nt_neigh + 3
            case('QUAD4')
                nt_neigh = nt_neigh + 4
            case('QUAD8')
                nt_neigh = nt_neigh + 4
            case('QUAD9')
                nt_neigh = nt_neigh + 4
            case default
                ASSERT(.false.)
        end select
    end do
!
! - Create object (collection)
!
    call jecrec(elem_neigh,'V V I', 'NU', 'CONTIG', 'VARIABLE', elem_indx_maxi+1-elem_indx_mini)
    call jeecra(elem_neigh, 'LONT', nt_neigh+(elem_indx_maxi+1-elem_indx_mini-nb_elem)) 
    do i_elem = 1, elem_indx_maxi+1-elem_indx_mini
        elem_nume = i_elem-1+elem_indx_mini
        nb_find   = 0
        lmail(1)  = elem_nume
        call utlisi('INTER', lmail, 1, list_elem, nb_elem, aux, 1, nb_find)
        if (nb_find .eq. 1) then
            call jenuno(jexnum('&CATA.TM.NOMTM', zi(jtypma+elem_nume-1)), elem_type)
            select case (elem_type)
                case('SEG2')
                    nb_neigh = 2
                case('SEG3')
                    nb_neigh = 2
                case('TRIA3')
                    nb_neigh = 3
                case('TRIA6')
                    nb_neigh = 3
                case('QUAD4')
                    nb_neigh = 4
                case('QUAD8')
                    nb_neigh = 4
                case('QUAD9')
                    nb_neigh = 4
                case default
                    ASSERT(.false.)
            end select
            call jecroc(jexnum(elem_neigh,i_elem))
            call jeecra(jexnum(elem_neigh,i_elem), 'LONMAX', ival=nb_neigh)
        elseif (nb_find .eq. 0) then
            call jecroc(jexnum(elem_neigh,i_elem))
            call jeecra(jexnum(elem_neigh,i_elem), 'LONMAX', ival=1)
        else
            ASSERT(.false.)      
        end if
    end do
!
! - Fill object
!
    do i_elem = 1, nb_elem
        elem_nume = list_elem(i_elem)
        elem_indx = elem_nume+1-elem_indx_mini
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(jtypma+elem_nume-1)), elem_type)
        select case (elem_type)
            case('SEG2')
                nb_neigh  = 2
                elem_code = 'SE2'
            case('SEG3')
                nb_neigh  = 2
                elem_code = 'SE3'
            case('TRIA3')
                nb_neigh  = 3
                elem_code = 'TR3'
            case('TRIA6')
                nb_neigh  = 3
                elem_code = 'TR6'
            case('QUAD4')
                nb_neigh  = 4
                elem_code = 'QU4'
            case('QUAD8')
                nb_neigh  = 4
                elem_code = 'QU8'
            case('QUAD9')
                nb_neigh  = 4
                elem_code = 'QU9'
            case default
                ASSERT(.false.)
        end select
        call jeveuo(jexnum(elem_neigh, elem_indx), 'E', vi = v_elem_neigh)
        call gtvois(mesh     , list_elem, nb_elem   , elem_nume, elem_code,&
                    conx_inve, nb_neigh , list_neigh)
        do i_neigh=1, nb_neigh
            v_elem_neigh(i_neigh) = list_neigh(i_neigh) 
        end do
    end do
end subroutine
