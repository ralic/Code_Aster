subroutine nmcadt(sddisc, iadapt, numins, valinc, dtp)
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
    implicit none
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/extdch.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utdidt.h"
    integer :: iadapt, numins
    character(len=19) :: valinc(*)
    character(len=19) :: sddisc
    real(kind=8) :: dtp
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! CALCUL DU NOUVEAU PAS DE TEMPS EN CAS D'ADAPTATION
!
! ----------------------------------------------------------------------
!
!
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  IADAPT : NUMERO DE LA METHODE D ADAPTATION TRAITEE
! IN  NUMINS : NUMERO D'INSTANT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! OUT DTP    : NOUVEAU PAS DE TEMPS (DT+)
!
!
!
!
    integer :: ibid, nit, nbiter
    real(kind=8) :: r8bid, dtm, pcent, valref, dval
    character(len=8) :: k8bid, typext
    character(len=16) :: modetp, nocham, nocmp
    real(kind=8) :: eta, etad
    character(len=24) :: tpsite
    integer :: jiter
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD
!
    tpsite = sddisc(1:19)//'.ITER'
    call jeveuo(tpsite, 'L', jiter)
!
! --- METHODE DE CALCUL DE DT+
!
    call utdidt('L', sddisc, 'ADAP', iadapt, 'METHODE',&
                r8bid, ibid, modetp)
!
! --- PAS DE TEMPS PAR DEFAUT (LE DERNIER, SAUF SI JALON)
!
    call utdidt('L', sddisc, 'LIST', ibid, 'DT-',&
                dtm, ibid, k8bid)
!
!     ------------------------------------------------------------------
    if (modetp .eq. 'FIXE') then
!     ------------------------------------------------------------------
!
        call utdidt('L', sddisc, 'ADAP', iadapt, 'PCENT_AUGM',&
                    pcent, ibid, k8bid)
        dtp = dtm * (1.d0 + pcent / 100.d0)
!
!     ------------------------------------------------------------------
    else if (modetp.eq.'DELTA_GRANDEUR') then
!     ------------------------------------------------------------------
!
        call utdidt('L', sddisc, 'ADAP', iadapt, 'NOM_CHAM',&
                    r8bid, ibid, nocham)
        call utdidt('L', sddisc, 'ADAP', iadapt, 'NOM_CMP',&
                    r8bid, ibid, nocmp)
        call utdidt('L', sddisc, 'ADAP', iadapt, 'VALE_REF',&
                    valref, ibid, k8bid)
        typext = 'MAX_ABS'
!
! ----- CALCUL DE C = MIN (VREF / |DELTA(CHAMP+CMP)| )
! -----             = VREF / MAX ( |DELTA(CHAMP+CMP)| )
! ----- DVAL :MAX EN VALEUR ABSOLUE DU DELTA(CHAMP+CMP)
!
        call extdch(typext, valinc, nocham, nocmp, dval)
!
! ----- LE CHAMP DE VARIATION EST IDENTIQUEMENT NUL : ON SORT
!
        if (dval .eq. 0.d0) then
            dtp = r8vide()
        else
            dtp = dtm * valref/dval
        endif
!
!     ------------------------------------------------------------------
    else if (modetp.eq.'ITER_NEWTON') then
!     ------------------------------------------------------------------
!
        call utdidt('L', sddisc, 'ADAP', iadapt, 'NB_ITER_NEWTON_REF',&
                    r8bid, nit, k8bid)
        nbiter = zi(jiter-1+numins)
        dtp = dtm * sqrt( dble(nit) / dble(nbiter+1) )
!
!     ------------------------------------------------------------------
    else if (modetp.eq.'IMPLEX') then
!     ------------------------------------------------------------------
!
! ----- FACTEUR D'ACCELERATION ETA
!
        eta = 1.2d0
!
! ----- FACTEUR DE DECELERATION ETAD
!
        etad = 0.5d0
        typext = 'MIN_VAR'
!
! ----- CALCUL DE C = MIN (VREF / |DELTA(CHAMP+CMP)| )
!                   = VREF / MAX ( |DELTA(CHAMP+CMP)| )
! ----- DVAL :MAX EN VALEUR ABSOLUE DU DELTA(CHAMP+CMP)
!
        nocham = 'VARI_ELGA'
        nocmp = 'V1'
        call extdch(typext, valinc, nocham, nocmp, dval)
!
! ----- LE CHAMP DE VARIATION EST IDENTIQUEMENT NUL : ON SORT
!
        if (dval .ge. r8maem()) dval=eta
        dtp = dtm * dval
!
! ----- ON IMPOSE QUE LE DT SOIT COMPRIS ENTRE ETAD*DTM ET ETA*DTM
!
        if (dtp/dtm .ge. eta) then
            dtp = eta * dtm
        else if (dtp/dtm.le.etad) then
            dtp = etad * dtm
        endif
!
    else
        call assert(.false.)
    endif
!
    call jedema()
end subroutine
