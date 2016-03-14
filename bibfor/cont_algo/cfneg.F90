subroutine cfneg(sdcont_solv, sdcont_defi, noma, indic,&
                 nbliai, nbliac, ajliai, spliai, nbpren)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfimp2.h"
#include "asterfort/cftabl.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016 EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=24) :: sdcont_solv, sdcont_defi
    character(len=8) :: noma
    integer :: indic
    integer :: ajliai, spliai, nbliai, nbpren
    integer :: nbliac
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES DISCRETES - RESOLUTION)
!
! VERIFICATION QUE LES MULTIPLICATEURS DE LAGRANGE SONT A VALEURS
!   POSITIVES (PRESSION DE CONTACT POSITIVE)
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT (ISSUE D'AFFE_CHAR_MECA)
! IN  RESOCO : SD DE TRAITEMENT NUMERIQUE DU CONTACT
! IN  NOMA   : NOM DU MAILLAGE
! OUT INDIC  :+1 ON A RAJOUTE UNE LIAISON
!             -1 ON A ENLEVE UNE LIAISON
! IN  NBLIAI : NOMBRE DE LIAISONS DE CONTACT
! I/O NBLIAC : NOMBRE DE LIAISONS ACTIVES
! I/O AJLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON CORRECTE DU CALCUL
!              DE LA MATRICE DE CONTACT ACM1AT
! I/O SPLIAI : INDICE DANS LA LISTE DES LIAISONS ACTIVES DE LA DERNIERE
!              LIAISON AYANT ETE CALCULEE POUR LE VECTEUR CM1A
! OUT NBPREN : NOMBRE DE PRESSION NEGATIVE
!
!
!
!
    integer :: deklag, dekln
    integer :: posit
    integer :: jj, iliac, lliac
    integer :: nbini, idebut, ifin, jsto
    real(kind=8) :: lambda
    character(len=1) :: typesp
    character(len=19) :: liac, mu
    integer :: jliac, jmu
    integer, pointer :: spliac(:) => null()
    integer, pointer :: supnbl(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    deklag = 0
    dekln = 0
    nbpren = 0
    typesp = 'S'
!
! --- PAS DE LIAISON DE CONTACT -> ON SORT
!
    if (nbliac .eq. 0) then
        goto 999
    endif
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    liac = sdcont_solv(1:14)//'.LIAC'
    mu = sdcont_solv(1:14)//'.MU'
    call jeveuo(liac, 'E', jliac)
    call jeveuo(mu, 'E', jmu)
!
! --- CREATION DES OBJETS DE TRAVAIL
!
    AS_ALLOCATE(vi=supnbl, size=nbliac)
    AS_ALLOCATE(vi=spliac, size=nbliac)
! ======================================================================
! --- LES VALEURS DU VECTEUR SUPNBL SONT NECESSAIREMENT CROISSANTES
! ======================================================================
    nbini = 1
    do iliac = 1, nbliac
        lambda = zr(jmu-1+iliac)
        if (lambda .lt. 0.0d0) then
            dekln = dekln + 1
            do jj = nbini, nbliac
                deklag = deklag + 1
                if (deklag .eq. iliac) then
                    supnbl(dekln) = iliac
                    spliac(dekln) = jj
                    nbini = jj + 1
                    goto 10
                endif
            end do
        endif
 10     continue
    end do
    if (dekln .eq. 0) then
        goto 999
    endif
! ======================================================================
! --- MISE A JOUR DE MU POUR LE CONTACT ET DU VECTEUR DES LIAISONS
! --- DE CONTACT 
! ======================================================================
    jsto = supnbl(1) - 1
    do iliac = 1, dekln-1
        idebut = jsto + 1
        ifin = idebut+supnbl(iliac+1)-supnbl(iliac)-1-1
        do jj = idebut, ifin
            jsto = jsto + 1
            zr(jmu-1+jsto) = zr(jmu-1+jj+iliac)
        end do
    end do
    idebut = jsto + 1
    ifin = nbliac - dekln
    do jj = idebut, ifin
        jsto = jsto + 1
        zr(jmu-1+jsto) = zr(jmu-1+jj+dekln)
    end do
    do jj = 1, dekln
        iliac = dekln - jj + 1
        posit = spliac(iliac)
        lliac = zi(jliac -1+posit)
        zr(jmu+3*nbliai-1+lliac) = 0.0d0
        call cftabl(indic, nbliac, ajliai, spliai,&
                    sdcont_solv, typesp, posit,&
                    lliac)
        call cfimp2(sdcont_defi, sdcont_solv, noma, lliac, 'NEG')
    end do

! ======================================================================
999 continue
!
    nbpren = dekln
! ======================================================================
! --- MENAGE
! ======================================================================
    AS_DEALLOCATE(vi=supnbl)
    AS_DEALLOCATE(vi=spliac)
! ======================================================================
    call jedema()
! ======================================================================
end subroutine
