subroutine cfreso(resoco, ldscon, ndim, nbliac, llf,&
                  llf1, llf2)
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rldlgg.h"
    character(len=24) :: resoco
    integer :: ndim
    integer :: nbliac, llf, llf1, llf2
    integer :: ldscon
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! RESOLUTION DE [-A.C-1.AT].{MU} = {JEU(DEPTOT) - A.DDEPL0}
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  LDSCON : DESCRIPTEUR DE LA MATRICE DE CONTACT
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  ISTO   : INDICATEUR D'ARRET EN CAS DE PIVOT NUL
! IN  NBLIAC : NOMBRE DE LIAISONS ACTIVES
! IN  LLF    : NOMBRE DE LIAISONS DE FROTTEMENT (EN 2D)
!              NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LES DEUX
!               DIRECTIONS SIMULTANEES (EN 3D)
! IN  LLF1   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               PREMIERE DIRECTION (EN 3D)
! IN  LLF2   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               SECONDE DIRECTION (EN 3D)
!
!
!
!
    integer :: ilifin, neqmax
    complex(kind=8) :: c16bid
    character(len=19) :: mu
    integer :: jmu
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    mu = resoco(1:14)//'.MU'
    call jeveuo(mu, 'E', jmu)
!
! --- INITIALISATIONS
!
    ilifin = nbliac+(ndim-1)*llf+llf1+llf2
!
! --- ON NE RESOUD LE SYSTEME QUE DE 1 A ILIFIN
!
    neqmax = zi(ldscon+2)
    zi(ldscon+2) = ilifin
!
! --- RESOLUTION : [-A.C-1.AT].{MU} = {JEU(DEPTOT) - A.DDEPL0}
!
    c16bid = dcmplx(0.d0, 0.d0)
    call rldlgg(ldscon, zr(jmu), [c16bid], 1)
    zi(ldscon+2) = neqmax
!
    call jedema()
!
end subroutine
