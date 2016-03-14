subroutine cftabl(indic, nbliac, ajliai, spliai, &
                  sdcont_solv, typope, posit,&
                  iliai)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
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
    integer :: indic
    integer :: nbliac
    integer :: ajliai
    integer :: spliai
    integer :: posit
    integer :: iliai
    character(len=1) :: typope
    character(len=24) :: sdcont_solv
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE DISCRETE - ALGORITHME)
!
! MISE A JOUR DES VECTEURS DE LIAISONS CONTACT ET/OU FROTTEMENT
! LE NOMBRE DE LIAISONS EST MIS A JOUR DANS LA ROUTINE
!
!
! ----------------------------------------------------------------------
!
! OUT INDIC  :+1 ON A RAJOUTE UNE LIAISON
!             -1 ON A ENLEVE UNE LIAISON
! I/O NBLIAC : NOMBRE DE LIAISONS ACTIVES
! I/O AJLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON CORRECTE DU CALCUL
!              DE LA MATRICE DE CONTACT ACM1AT
! I/O SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  TYPOPE : TYPE D'OPERATION DANS LE VECTEUR DES LIAISONS
!                'A' : AJOUTER UNE LIAISON
!                'S' : SUPPRIMER UNE LIAISON
! IN  POSIT  : POSITION POUR AJOUTER UNE LIAISON DANS LE
!              VECTEUR DES LIAISONS ACTIVES
! IN  ILIAI  : INDICE DE LA LIAISON A AJOUTER OU SUPPRIMER
!
!
    integer :: ii, posit2, liaisp
    character(len=1) :: typeaj, typesp
    character(len=19) :: sdcont_liac
    integer, pointer :: v_sdcont_liac(:) => null()
! ======================================================================
!
    sdcont_liac = sdcont_solv(1:14)//'.LIAC'
    call jeveuo(sdcont_liac, 'E', vi = v_sdcont_liac)
!
    typeaj = 'A'
    typesp = 'S'
    if (typope .eq. typeaj) then
        indic = 1
        v_sdcont_liac(posit) = iliai
        nbliac = nbliac + 1
    else if (typope.eq.typesp) then
        indic  = -1
        posit2 = nbliac + 1
        nbliac = nbliac - 1
        ajliai = ajliai - 1
        do ii = posit + 1, nbliac + 1
            if (v_sdcont_liac(ii) .eq. iliai) then
                ajliai = ajliai - 1
                posit2 = ii
                liaisp = 1
                goto 20
            endif
        end do
20      continue
        do ii = posit, (posit2-1) - 1
            v_sdcont_liac(ii) = v_sdcont_liac(ii+1)
        end do
        do ii = posit2 - 1, nbliac 
            v_sdcont_liac(nbliac) = v_sdcont_liac(nbliac+1+liaisp)
        end do
    endif
!
    spliai = min(spliai,posit-1)
    spliai = min(spliai,nbliac)
    if (ajliai .lt. 0) then
        ajliai = 0
    endif
!
end subroutine
