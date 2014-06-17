subroutine cfmajf(resoco, neq, ndim, nbliai, nbliac,&
                  llf, llf1, llf2)
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
    implicit none
#include "jeveux.h"
#include "asterfort/cfmajm.h"
#include "asterfort/cftyli.h"
#include "asterfort/jedema.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "blas/daxpy.h"
!
    character(len=24) :: resoco
    integer :: neq, ndim
    integer :: nbliai, nbliac
    integer :: llf, llf1, llf2
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! MISE A JOUR DU VECTEUR SOLUTION ITERATION DE CONTACT
!
! ----------------------------------------------------------------------
!
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NEQ    : NOMBRE D'EQUATIONS
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
    integer :: iliac, iliai, posit
    integer :: posnbl, poslf0, poslf1, poslf2
    character(len=19) :: mu, cm1a, liac
    integer :: jmu, jcm1a, jliac
    character(len=19) :: ddelt
    real(kind=8), pointer :: vale(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    posnbl = 0
    poslf0 = nbliac
    poslf1 = nbliac + (ndim-1)*llf
    poslf2 = nbliac + (ndim-1)*llf + llf1
!
! --- ACCES STRUCTURES DE DONNEES DE CONTACT
!
    mu = resoco(1:14)//'.MU'
    cm1a = resoco(1:14)//'.CM1A'
    liac = resoco(1:14)//'.LIAC'
    call jeveuo(mu, 'L', jmu)
    call jeveuo(liac, 'L', jliac)
!
! --- ACCES AUX CHAMPS DE TRAVAIL
!
    ddelt = resoco(1:14)//'.DDEL'
    call jeveuo(ddelt(1:19)//'.VALE', 'E', vr=vale)
!
! --- ON REORDONNE LE VECTEUR MU
!
    call cfmajm(resoco, ndim, nbliac, llf, llf1,&
                llf2)
!
! --- CALCUL DE DDEPL0 = DDEPL0 - C-1.AT.MU
!
    do iliac = 1, nbliac + llf + llf1 + llf2
        iliai = zi(jliac-1+iliac)
        call cftyli(resoco, iliac, posit)
        select case (posit)
!
! ----- LIAISON DE CONTACT
!
        case (1)
            posnbl = posnbl + 1
            call jeveuo(jexnum(cm1a, iliai), 'L', jcm1a)
            call daxpy(neq, -zr(jmu-1+posnbl), zr(jcm1a), 1, vale,&
                       1)
            call jelibe(jexnum(cm1a, iliai))
!
! ----- LIAISON DE FROTTEMENT - 2D OU 3D DANS LES DEUX DIRECTIONS
!
        case (2)
            poslf0 = poslf0 + 1
            call jeveuo(jexnum(cm1a, iliai+nbliai), 'L', jcm1a)
            call daxpy(neq, -zr(jmu-1+poslf0), zr(jcm1a), 1, vale,&
                       1)
            call jelibe(jexnum(cm1a, iliai+nbliai))
            if (ndim .eq. 3) then
                call jeveuo(jexnum(cm1a, iliai+(ndim-1)*nbliai), 'L', jcm1a)
                call daxpy(neq, -zr(jmu-1+poslf0+llf), zr(jcm1a), 1, vale,&
                           1)
                call jelibe(jexnum(cm1a, iliai+(ndim-1)*nbliai))
            endif
!
! ----- LIAISON DE FROTTEMENT - 3D PREMIERE DIRECTION
!
        case (3)
            poslf1 = poslf1 + 1
            call jeveuo(jexnum(cm1a, iliai+nbliai), 'L', jcm1a)
            call daxpy(neq, -zr(jmu-1+poslf1), zr(jcm1a), 1, vale,&
                       1)
            call jelibe(jexnum(cm1a, iliai+nbliai))
!
! ----- LIAISON DE FROTTEMENT - 3D SECONDE DIRECTION
!
        case (4)
            poslf2 = poslf2 + 1
            call jeveuo(jexnum(cm1a, iliai+(ndim-1)*nbliai), 'L', jcm1a)
            call daxpy(neq, -zr(jmu-1+poslf2), zr(jcm1a), 1, vale,&
                       1)
            call jelibe(jexnum(cm1a, iliai+(ndim-1)*nbliai))
        end select
    end do
!
    call jedema()
!
end subroutine
