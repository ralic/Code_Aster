subroutine nmexti(nomnoe, champ, nbcmp, listcp, extrcp,&
                  nvalcp, valres)
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
    implicit      none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmextv.h"
#include "asterfort/posddl.h"
    real(kind=8) :: valres(*)
    character(len=8) :: nomnoe
    integer :: nbcmp
    integer :: nvalcp
    character(len=19) :: champ
    character(len=24) :: listcp
    character(len=8) :: extrcp
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (EXTRACTION - UTILITAIRE)
!
! EXTRAIRE LES VALEURS DES COMPOSANTES - NOEUD
!
! ----------------------------------------------------------------------
!
!
! IN  EXTRCP : TYPE D'EXTRACTION SUR LES COMPOSANTES
! IN  NOMNOE : NOM DU NOEUD
! IN  CHAMP  : CHAMP OBSERVE
! IN  NBCMP  : NOMBRE DE COMPOSANTES
! IN  LISTCP : LISTE DES COMPOSANTES
! OUT VALRES : VALEUR DES COMPOSANTES
! OUT NVALCP : NOMBRE EFFECTIF DE COMPOSANTES
!
!
!
!
    integer :: nparx
    parameter    (nparx=20)
    character(len=8) :: nomcmp(nparx)
    real(kind=8) :: valcmp(nparx)
    integer :: neff
    integer :: ieff, icmp, ipar
    integer :: jcmp
    character(len=8) :: cmp
    integer :: nuno, nuddl
    real(kind=8), pointer :: vale(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    ieff = 1
    ASSERT(nbcmp.le.nparx)
!
! --- ACCES CHAMP
!
    call jeveuo(champ //'.VALE', 'L', vr=vale)
!
! --- NOM DES COMPOSANTES
!
    call jeveuo(listcp, 'L', jcmp)
!
    do 20 icmp = 1, nbcmp
        nomcmp(icmp) = zk8(jcmp-1+icmp)
20  end do
!
! --- VALEURS DES COMPOSANTES
!
    do 30 ipar = 1, nbcmp
        cmp = nomcmp(ipar)
        call posddl('CHAM_NO', champ, nomnoe, cmp, nuno,&
                    nuddl)
        if ((nuno.ne.0) .and. (nuddl.ne.0)) then
            valcmp(ieff) = vale(nuddl)
            ieff = ieff + 1
        endif
30  end do
    neff = ieff - 1
!
! --- EVALUATION
!
    call nmextv(neff, extrcp, nomcmp, valcmp, nvalcp,&
                valres)
    ASSERT(nvalcp.le.nbcmp)
!
    call jedema()
!
end subroutine
