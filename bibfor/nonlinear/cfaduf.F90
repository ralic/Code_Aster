subroutine cfaduf(resoco, ndim, nbliai, nbliac, llf,&
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
    implicit none
#include "jeveux.h"
#include "asterfort/cftyli.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/r8inir.h"
    integer :: ndim, nbliai
    integer :: nbliac, llf, llf1, llf2
    character(len=24) :: resoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! CALCUL DU SECOND MEMBRE - CAS DU FROTTEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT
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
    character(len=19) :: liac, mu
    integer :: jliac, jmu
    character(len=24) :: jeux
    integer :: jjeux
    real(kind=8) :: jeuini, jexini, jeyini
    integer :: iliai, iliac, type0, deklag, btotal
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    deklag = 0
    btotal = nbliac + llf + llf1 + llf2
!
! --- ACCES STRUCTURES DE DONNEES DE CONTACT
!
    liac = resoco(1:14)//'.LIAC'
    jeux = resoco(1:14)//'.JEUX'
    mu = resoco(1:14)//'.MU'
    call jeveuo(liac, 'L', jliac)
    call jeveuo(jeux, 'L', jjeux)
    call jeveuo(mu, 'E', jmu)
!
! --- INITIALISATION MU
!
    call r8inir(ndim*nbliai, 0.d0, zr(jmu), 1)
!
! --- ON MET {JEU(DEPTOT)} - [A].{DDEPL0} DANS MU
!
    do iliac = 1, btotal
!
! ----- TYPE DE LA LIAISON
!
        iliai = zi(jliac-1+iliac)
        call cftyli(resoco, iliac, type0)
!
        select case (type0)
!
! ----- CALCUL DE MU_C
!
        case (1)
            jeuini = zr(jjeux+3*(iliai-1)+1-1)
            zr(jmu+iliac+deklag-1) = jeuini
!
! ----- CALCUL DE MU_A - 2D OU 3D DANS LES DEUX DIRECTIONS
! ----- DEPUIS LE DEBUT DU PAS DE TEMPS
!
        case (2)
            jexini = zr(jjeux+3*(iliai-1)+2-1)
            zr(jmu+iliac+deklag-1) = - jexini
            if (ndim .eq. 3) then
                jeyini = zr(jjeux+3*(iliai-1)+3-1)
                deklag = deklag + 1
                zr(jmu+iliac+deklag-1) = - jeyini
            endif
!
! ----- CALCUL DE MU_A 3D - 3D PREMIERE DIRECTION
! ----- DEPUIS LE DEBUT DU PAS DE TEMPS
!
        case (3)
            jexini = zr(jjeux+3*(iliai-1)+2-1)
            zr(jmu+iliac+deklag-1) = - jexini
!
! ----- CALCUL DE MU_A 3D - 3D SECONDE DIRECTION
! ----- DEPUIS LE DEBUT DU PAS DE TEMPS
!
        case (4)
            jeyini = zr(jjeux+3*(iliai-1)+3-1)
            zr(jmu+iliac+deklag-1) = - jeyini
!
        end select
    end do
!
    call jedema()
end subroutine
