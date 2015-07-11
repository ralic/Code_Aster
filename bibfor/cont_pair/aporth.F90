subroutine aporth(sdappa, noma , sdcont_defi, ndimg, elem_mast_indx,&
                  coorpt, tau1m, tau2m)
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
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/cfnben.h"
#include "asterfort/cfnumm.h"
#include "asterfort/aptypm.h"
#include "asterfort/assert.h"
#include "asterfort/cforth.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/normev.h"
#include "asterfort/utmess.h"
    character(len=19) :: sdappa
    character(len=8) :: noma
    character(len=24) :: sdcont_defi
    integer :: ndimg
    integer :: elem_mast_indx
    real(kind=8) :: coorpt(3)
    real(kind=8) :: tau1m(3), tau2m(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE APPARIEMENT (UTILITAIRE)
!
! ORTHOGONALISATION DES VECTEURS TANGENTS
!
! ----------------------------------------------------------------------
!
!
! IN  SDAPPA : NOM DE LA SD APPARIEMENT
! IN  NOMA   : SD MAILLAGE
! IN  DEFICO : SD DEFINITION DU CONTACT
! IN  NDIMG  : DISMENSION DE L'ESPACE
! IN  POSMAM : POSITION MAILLE MAITRE
! IN  COORPT : COORDONNEES DU POINT A PROJETER SUR LA MAILLE
! I/O TAU1M  : VALEUR DE LA PREMIERE TANGENTE AU POINT PROJETE
! I/O TAU2M  : VALEUR DE LA SECONDE TANGENTE AU POINT PROJETE
!
!
!
!
    aster_logical :: lpoutr
    character(len=8) :: aliasm
    character(len=8) :: nommam
    real(kind=8) :: noor
    integer :: niverr, ndim
    integer :: elem_mast_nume, nnosdm
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- POINT NON PROJETE
!
    ASSERT(elem_mast_indx.ne.0)
!
! - Number of nodes
!
    call cfnben(sdcont_defi, elem_mast_indx, 'CONNEX', nnosdm)
!
! - Index of master element
!
    call cfnumm(sdcont_defi, elem_mast_indx, elem_mast_nume)
!
! - Parameters of master element
!
    call aptypm(sdappa, noma, elem_mast_nume, ndim, nnosdm,&
                aliasm, nommam)
    lpoutr = (aliasm(1:2).eq.'SE').and.(ndimg.eq.3)
!
! --- ORTHOGONALISATION VECTEURS TANGENTS
!
    if (lpoutr) then
        call normev(tau1m, noor)
        if (noor .le. r8prem()) then
            call utmess('F', 'APPARIEMENT_38', sk=nommam)
        endif
    else
        call cforth(ndimg, tau1m, tau2m, niverr)
        if (niverr .eq. 1) then
            call utmess('F', 'APPARIEMENT_14', sk=nommam, nr=3, valr=coorpt)
        else if (niverr.eq.2) then
            call utmess('F', 'APPARIEMENT_34', sk=nommam, nr=3, valr=coorpt)
        endif
    endif
!
    call jedema()
!
end subroutine
