!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
#include "asterf_types.h"
!
! aslint: disable=W1504
!
interface
    subroutine nmxmat(modelz, mate  , carele, ds_constitutive,&
                      sddisc, sddyna, fonact, numins     , iterat,&
                      valinc, solalg, lischa, comref     , &
                      numedd, numfix, ds_measure, ds_algopara,&
                      nbmatr, ltypma, loptme     , loptma,&
                      lcalme, lassme, lcfint, meelem     , measse,&
                      veelem, ldccvg, ds_contact_)
        use NonLin_Datastructure_type        
        character(len=*) :: modelz
        character(len=*) :: mate
        character(len=24) :: carele
        type(NL_DS_Constitutive), intent(in) :: ds_constitutive
        character(len=19) :: sddisc
        character(len=19) :: sddyna
        integer :: fonact(*)
        integer :: numins
        integer :: iterat
        character(len=19) :: valinc(*)
        character(len=19) :: solalg(*)
        character(len=19) :: lischa
        character(len=24) :: comref
        character(len=24) :: numedd
        character(len=24) :: numfix
        type(NL_DS_Measure), intent(inout) :: ds_measure
        integer :: nbmatr
        character(len=6) :: ltypma(20)
        character(len=16) :: loptme(20)
        character(len=16) :: loptma(20)
        aster_logical :: lcalme(20)
        aster_logical :: lassme(20)
        aster_logical :: lcfint
        character(len=19) :: meelem(*)
        character(len=19) :: measse(*)
        character(len=19) :: veelem(*)
        integer :: ldccvg
        type(NL_DS_AlgoPara), intent(in) :: ds_algopara
        type(NL_DS_Contact), optional, intent(in) :: ds_contact_
    end subroutine nmxmat
end interface
