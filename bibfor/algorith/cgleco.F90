subroutine cgleco(resu, modele, iord0, typfis, compor,&
                  incr)
    implicit none
!
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/gverlc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmdorc.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
    integer :: iord0
    character(len=8) :: resu, modele, typfis
    character(len=24) :: compor
    logical :: incr
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
! person_in_charge: samuel.geniaut at edf.fr
!
!     SOUS-ROUTINE DE L'OPERATEUR CALC_G
!
!     BUT : LECTURE DE LA CARTE DE COMPORTEMENT UTILISEE DANS LE CALCUL
!
!  IN :
!     RESU   : MOT-CLE RESULTAT
!     MODELE : MODELE ASSOCIE AU RESULTAT
!     IORD0  : PREMIER NUME_ORDRE
!     TYPFIS : TYPE D'OBJET POUR DECRIRE LE FOND DE FISSURE
!              'FONDFISS' OU 'FISSURE' OU 'THETA'
!  OUT :
!     COMPOR : CARTE DU COMPORTEMENT
!     INCR   : .TRUE. SI COMPORTEMENT INCREMENTAL
! ======================================================================
!
    integer :: nbcomp, ntmp, ier, i, ibid
    character(len=16) :: moclef
    character(len=24) :: k24b, repk
    logical :: limpel
!
    call jemarq()
!
!     RECUPERATION DE LA CARTE DE COMPORTEMENT UTILISEE DANS LE CALCUL
    moclef = 'COMPORTEMENT'
!
!     NOMBRE MAX D'OCCURENCES DE COMPORTEMENT DANS CALC_G
    nbcomp=0

    call getfac(moclef, ntmp)
    nbcomp=max(nbcomp,ntmp)

!
    limpel=.false.
!
!
!     1) RECUP OU CREATION DE COMPOR
!     ------------------------------
!
    if (nbcomp .eq. 0) then
!
!       COMPORTEMENT N'EST PAS RENSEIGNE DANS CALC_G ALORS
!       ON VA CHERCHE LE COMPORTEMENT ASSOCIE AU RESU (1ER NUME_ORDRE)
        call rsexch(' ', resu, 'COMPORTEMENT', iord0, compor,&
                    ier)
!
        if (ier .ne. 0) then
!         PROBLEME  DANS LA RECUPERATION DU COMPORTEMENT DANS RESU
!         (CAS MECA_STATIQUE PAR EXEMPLE)
!         --> ON VA IMPOSER DANS CALC_G COMP_ELAS
            limpel=.true.
            call nmdorc(modele, compor, k24b)
        endif
!
    else
!
!       COMPORTEMENT EST RENSEIGNE DANS CALC_G ALORS ON LE PREND
        call nmdorc(modele, compor, k24b)
!
    endif
!
!
!     2) RECUPERATION DE INCR
!     -----------------------
!
    if (limpel) then
!       C'EST FACILE, ON A IMPOSE COMP_ELAS DONC INCR EST FAUX
        incr = .false.
    else
!       SOIT COMPORTEMENT EST IMPOSE DANS CALC_G
!       SOIT ON A PRIS LE COMPOR DE RESU
        call dismoi('F', 'ELAS_INCR', compor, 'CARTE_COMPOR', ibid,&
                    repk, ier)
        if (repk .eq. 'ELAS') then
            incr = .false.
        else if (repk.eq.'INCR'.or.repk.eq.'MIXTE') then
            incr = .true.
        else
            ASSERT(.false.)
        endif
    endif
!
!
!     3) VERIFS
!     ---------
!
!     VERIF COHERENCE (CAS OU COMPORTEMENT EST IMPOSE DANS CALC_G)
    if (nbcomp .ne. 0) call gverlc(resu, compor, iord0)
!
!     X-FEM N'EST PAS ENCORE DEVELOPPE POUR GTP (G EN INCREMENTAL)
    if (incr .and. typfis .eq. 'FISSURE') then
        call utmess('F', 'RUPTURE1_43')
    endif
!
    call jedema()
!
end subroutine
