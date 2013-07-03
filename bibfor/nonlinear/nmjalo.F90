subroutine nmjalo(sddisc, inst, prec, jalon)
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
#include "asterc/r8vide.h"
#include "asterfort/compr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=19) :: sddisc
    real(kind=8) :: inst, prec, jalon
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! PROCHAIN INSTANT DE PASSAGE DANS LA LISTE DES JALONS
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  INST   : INSTANT RECHERCHE
! IN  PREC   : PRECISION
! OUT JALON  : VALEUR DE L'INSTANT JALON TROUVE
!              VAUT R8VIDE SI L'INSTANT EST AU DELA DE LA LSITE
!
!
!
!
    character(len=24) :: tpsipo
    integer :: jipo
    character(len=8) :: k8bid
    integer :: ipo, nipo
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    jalon = r8vide()
!
! --- LISTE DES JALONS
!
    tpsipo = sddisc(1:19)//'.LIPO'
    call jelira(tpsipo, 'LONMAX', nipo, k8bid)
    call jeveuo(tpsipo, 'L', jipo)
!
! --- RECHERCHE PROCHAIN JALON
!
    do 10 ipo = 1, nipo
        if (compr8(zr(jipo-1+ipo),'GT',inst,prec,1)) then
            jalon = zr(jipo-1+ipo)
            goto 20
        endif
10  end do
20  continue
!
    call jedema()
end subroutine
