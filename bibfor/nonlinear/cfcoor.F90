subroutine cfcoor(noma, defico, newgeo, posmam, ksi1,&
                  ksi2, coordp)
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
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfcarm.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mmcoor.h"
    character(len=8) :: noma
    character(len=24) :: defico
    character(len=19) :: newgeo
    integer :: posmam
    real(kind=8) :: ksi1
    real(kind=8) :: ksi2
    real(kind=8) :: coordp(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - APPARIEMENT)
!
! CALCUL DES COORDONNEES DE LA PROJECTION DU NOEUD ESCLAVE
! SUR LA MAILLE MAITRE
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD DE CONTACT (DEFINITION)
! IN  NEWGEO : COORDONNEES REACTUALISEES DES NOEUDS DU MAILLAGE
! IN  POSMAM : INDICE DE LA MAILLE MAITRE (DANS SD CONTACT)
! IN  KSI1   : COORDONNEE PARAMETRIQUE KSI DU PROJETE
! IN  KSI2   : COORDONNEE PARAMETRIQUE ETA DU PROJETE
! OUT COORDP : COORDONNEES DE LA PROJECTION DU NOEUD ESCLAVE
!
!
!
!
    integer :: nnomam, idim, ndim, nummam
    real(kind=8) :: coorma(27)
    character(len=8) :: alias, nommam
    character(len=4) :: typmai
!
!-----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    do 10 idim = 1, 3
        coordp(idim) = 0.d0
10  end do
!
! --- CARACTERISTIQUES DE LA MAILLE MAITRE
!
    call cfcarm(noma, defico, newgeo, posmam, typmai,&
                nummam, alias, nommam, ndim, nnomam,&
                coorma)
    if (typmai .ne. 'MAIT') then
        ASSERT(.false.)
    endif
!
! --- COORDONNEES DU PROJETE
!
    call mmcoor(alias, nnomam, ndim, coorma, ksi1,&
                ksi2, coordp)
!
    call jedema()
end subroutine
