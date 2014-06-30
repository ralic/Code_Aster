subroutine cfinnl(defico, resoco, reageo, nbliac, llf,&
                  llf1, llf2)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "jeveux.h"
#include "asterfort/cfdisd.h"
#include "asterfort/cfdisl.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    character(len=24) :: defico, resoco
    integer :: nbliac, llf, llf1, llf2
    logical(kind=1) :: reageo
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - ALGORITHME)
!
! NOMBRE DE LIAISONS INITIALES
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  REAGEO : .TRUE. SI ON VIENT DE FAIRE UN NOUVEL APPARIEMENT
! OUT NBLIAC : NOMBRE DE LIAISONS ACTIVES
! OUT LLF    : NOMBRE DE LIAISON DE FROTTEMENT (DEUX DIRECTIONS)
! OUT LLF1   : NOMBRE DE LIAISON DE FROTTEMENT (1ERE DIRECTION )
! OUT LLF2   : NOMBRE DE LIAISON DE FROTTEMENT (2EME DIRECTION )
!
!
!
!
    logical(kind=1) :: lpenaf, llagrc
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- PARAMETRES
!
    lpenaf = cfdisl(defico,'FROT_PENA')
    llagrc = cfdisl(defico,'CONT_LAGR')
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
            nbliac = cfdisd(resoco,'NBLIAC' )
            if (lpenaf) then
                llf = 0
                llf1 = 0
                llf2 = 0
            else
                llf = cfdisd(resoco,'LLF' )
                llf1 = cfdisd(resoco,'LLF1' )
                llf2 = cfdisd(resoco,'LLF2' )
            endif
        endif
    else
        nbliac = 0
        llf = 0
        llf1 = 0
        llf2 = 0
    endif
!
    call jedema()
!
end subroutine
