subroutine cfllaf(noma, defico, resoco, iliai, nbliai,&
                  nbliac, llf, llf1, llf2, indic,&
                  ajliai, spliai)
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cfelpv.h"
#include "asterfort/cfimp2.h"
#include "asterfort/cftabl.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
    character(len=8) :: noma
    character(len=24) :: defico, resoco
    integer :: iliai
    integer :: nbliai, nbliac, llf, llf1, llf2
    integer :: indic, spliai, ajliai
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! ON AJOUTE UNE LIAISON DE FROTTEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  ILIAI  : NUMERO DE LA LIAISON A AJOUTER
! IN  NBLIAI : NOMBRE DE LIAISONS
! OUT INDIC  :+1 ON A RAJOUTE UNE LIAISON
!             -1 ON A ENLEVE UNE LIAISON
! I/O NBLIAC : NOMBRE DE LIAISONS ACTIVES
! I/O AJLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON CORRECTE DU CALCUL
!              DE LA MATRICE DE CONTACT ACM1AT
! I/O SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
! I/O LLF    : NOMBRE DE LIAISONS DE FROTTEMENT (EN 2D)
!              NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LES DEUX
!               DIRECTIONS SIMULTANEES (EN 3D)
! I/O LLF1   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               PREMIERE DIRECTION (EN 3D)
! I/O LLF2   : NOMBRE DE LIAISONS DE FROTTEMENT SUIVANT LA
!               SECONDE DIRECTION (EN 3D)
!
!
!
!
    integer :: posit
    aster_logical :: lelpiv, lelpi1, lelpi2
    character(len=1) :: typeaj
    character(len=2) :: typef0, typef1, typef2
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    typeaj = 'A'
    typef0 = 'F0'
    typef1 = 'F1'
    typef2 = 'F2'
    posit = nbliac + llf + llf1 + llf2 + 1
!
! --- LIAISON PROVOQUANT UN PIVOT NUL ?
!
    call cfelpv(iliai, typef0, resoco, nbliai, lelpiv)
    call cfelpv(iliai, typef1, resoco, nbliai, lelpi1)
    call cfelpv(iliai, typef2, resoco, nbliai, lelpi2)
!
! --- ON NE PREND PAS EN COMPTE UNE LIAISON A PIVOT NUL
!
    if (.not.lelpiv) then
        if (lelpi1) then
            call cftabl(indic, nbliac, ajliai, spliai, llf,&
                        llf1, llf2, resoco, typeaj, posit,&
                        iliai, typef2)
            call cfimp2(defico, resoco, noma, iliai, typef2,&
                        'ACT')
        else
            if (lelpi2) then
                call cftabl(indic, nbliac, ajliai, spliai, llf,&
                            llf1, llf2, resoco, typeaj, posit,&
                            iliai, typef1)
                call cfimp2(defico, resoco, noma, iliai, typef1,&
                            'ACT')
            else
                call cftabl(indic, nbliac, ajliai, spliai, llf,&
                            llf1, llf2, resoco, typeaj, posit,&
                            iliai, typef0)
                call cfimp2(defico, resoco, noma, iliai, typef0,&
                            'ACT')
            endif
        endif
    endif
!
    call jedema()
!
end subroutine
