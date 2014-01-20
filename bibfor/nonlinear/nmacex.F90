subroutine nmacex(sddisc, iterat, lextra, valext)
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
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmdcrg.h"
#include "asterfort/nmlere.h"
#include "asterfort/nmlerr.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    character(len=19) :: sddisc
    integer :: iterat
    logical :: lextra
    real(kind=8) :: valext(4)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - GESTION DES EVENEMENTS)
!
! EXTRAPOLATION LINEAIRE DES RESIDUS
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  ITERAT : NUMERO D'ITERATION DE NEWTON
! OUT LEXTRA : .TRUE. SI EXTRAPOLATION OK
! OUT VALEXT : VALEURS DE L'EXTRAPOLATION (XA0 + ITER*XA1) / XDET
!               VALEXT(1): XA0
!               VALEXT(2): XA1
!               VALEXT(3): XDET
!               VALEXT(4): CRESI (RESIDU CIBLE)
!
!
!
!
    integer :: ibid, regres, depart
    real(kind=8) :: cresi, crela, cmaxi
    real(kind=8) :: vrela(1), vmaxi(1)
    real(kind=8) :: r8bid
    real(kind=8) :: xa0, xa1, xdet
    integer :: nbiter, mniter, mxiter
    integer :: nbigno
    real(kind=8), pointer :: erreurs(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    regres = 0
    lextra = .false.
    valext(1) = 0.d0
    valext(2) = 0.d0
    valext(3) = 0.d0
    valext(4) = 0.d0
!
! --- AFFICHAGE
!
    call utmess('I', 'EXTRAPOLATION_1')
!
! --- LECTURE DES INFOS SUR LES CONVERGENCES
!
    call nmlerr(sddisc, 'L', 'MXITER', r8bid, mxiter)
    call nmlerr(sddisc, 'L', 'MNITER', r8bid, mniter)
    call nmlerr(sddisc, 'L', 'NBITER', r8bid, nbiter)
    call nmlerr(sddisc, 'L', 'RESI_GLOB_RELA', crela, ibid)
    call nmlerr(sddisc, 'L', 'RESI_GLOB_MAXI', cmaxi, ibid)
!
! --- REGRESSION SUR GLOB_RELA OU GLOB_MAXI ?
!
    call nmlerr(sddisc, 'L', 'TYPE_RESI', r8bid, regres)
    call nmlere(sddisc, 'L', 'VRELA', iterat, vrela)
    call nmlere(sddisc, 'L', 'VMAXI', iterat, vmaxi)
!
! --- SI REGRES=3 ON DOIT FAIRE LA REGRESSION SUR LES 2, MAIS ON
! --- COMMENCE PAR LA FAIRE SUR GLOB_RELA
! --- SI VRELA > RGRELA ON MET REGRES=1 POUR FAIRE LA REGRESSION
! ---  SUR GLOB_RELA
! --- SI VMAXI > RGMAXI ON MET REGRES=2 POUR FAIRE LA REGRESSION
! ---  SUR GLOB_MAXI
!
    if (regres .eq. 3) then
        regres = 0
        if (vrela(1) .gt. crela) then
            regres = 1
        else if (vmaxi(1) .gt. cmaxi) then
            regres = 2
        endif
    endif
!
! --- LES CRITERES D'ERREUR SONT OK, MAIS PAS DETECTE DANS LE
! --- STAT_NON_LINE -> NI GLOB_RELA, NI GLOB_MAXI !
!
    if (regres .eq. 0) then
        call utmess('A', 'EXTRAPOLATION_2')
        lextra = .false.
        goto 999
    endif
!
! --- PARAMETRES DE LA METHODE D'EXTRAPOLATION
!
    nbigno = 3
!
! --- ASSEZ D'ITERATIONS POUR FAIRE L'EXTRAPOLATION ?
!
    if ((nbigno+3) .le. iterat) then
        depart = nbigno
    else
        lextra = .false.
        call utmess('I', 'EXTRAPOLATION_3')
        goto 999
    endif
!
! --- TOUTES LES RESIDUS AU COURS DES ITERATIONS [0,ITERAT]
!
    AS_ALLOCATE(vr=erreurs, size=iterat+1)
    if (regres .eq. 1) then
        cresi = crela
        call nmlere(sddisc, 'L', 'VRELA_TOUS', iterat,erreurs)
    else if (regres .eq. 2) then
        cresi = cmaxi
        call nmlere(sddisc, 'L', 'VMAXI_TOUS', iterat,erreurs)
    else
        ASSERT(.false.)
    endif
!
! --- CALCUL DE L'EXTRAPOLATION LINEAIRE
!
    call nmdcrg(depart, iterat, erreurs, xa0, xa1,&
                xdet)
    AS_DEALLOCATE(vr=erreurs)
!
! --- EXTRAPOLATION REUSSIE ?
!
    if (xdet .le. r8prem()) then
        call utmess('I', 'EXTRAPOLATION_10')
        lextra = .false.
    else
        valext(1) = xa0
        valext(2) = xa1
        valext(3) = xdet
        valext(4) = cresi
        lextra = .true.
    endif
!
999  continue
!
    call jedema()
end subroutine
