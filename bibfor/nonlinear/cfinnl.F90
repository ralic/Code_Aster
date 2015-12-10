subroutine cfinnl(ds_contact, reageo, nbliac, llf,&
                  llf1, llf2)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfdisl.h"
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
    type(NL_DS_Contact), intent(in) :: ds_contact
    integer :: nbliac, llf, llf1, llf2
    aster_logical :: reageo
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - ALGORITHME)
!
! NOMBRE DE LIAISONS INITIALES
!
! ----------------------------------------------------------------------
!
! In  ds_contact       : datastructure for contact management
! IN  REAGEO : .TRUE. SI ON VIENT DE FAIRE UN NOUVEL APPARIEMENT
! OUT NBLIAC : NOMBRE DE LIAISONS ACTIVES
! OUT LLF    : NOMBRE DE LIAISON DE FROTTEMENT (DEUX DIRECTIONS)
! OUT LLF1   : NOMBRE DE LIAISON DE FROTTEMENT (1ERE DIRECTION )
! OUT LLF2   : NOMBRE DE LIAISON DE FROTTEMENT (2EME DIRECTION )
!
!
!
!
    aster_logical :: lpenaf, llagrc
!
! ----------------------------------------------------------------------
!
    lpenaf = cfdisl(ds_contact%sdcont_defi,'FROT_PENA')
    llagrc = cfdisl(ds_contact%sdcont_defi,'CONT_LAGR')
!
! --- NOMBRE DE LIAISONS INITIALES
! --- POUR LES METHODES PUREMENT LAGRANGIENNES, ON GARDE
! --- LA MEMOIRE DES LIAISONS PRECEDEMMENT ACTIVES
!
    if (llagrc) then
        if (reageo) then
            nbliac = 0
            llf = 0
            llf1 = 0
            llf2 = 0
        else
            nbliac = cfdisd(ds_contact%sdcont_solv,'NBLIAC' )
            if (lpenaf) then
                llf = 0
                llf1 = 0
                llf2 = 0
            else
                llf = cfdisd(ds_contact%sdcont_solv,'LLF' )
                llf1 = cfdisd(ds_contact%sdcont_solv,'LLF1' )
                llf2 = cfdisd(ds_contact%sdcont_solv,'LLF2' )
            endif
        endif
    else
        nbliac = 0
        llf = 0
        llf1 = 0
        llf2 = 0
    endif
!
end subroutine
