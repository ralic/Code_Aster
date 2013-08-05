function nucalc(opt, te, memoir)
    implicit none
    integer :: nucalc
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
    integer :: opt, te, memoir
! ----------------------------------------------------------------------
!     ENTREES:
!        OPT : OPTION
!         TE : TYPE_ELEMENT
!     MEMOIR : 1 : ON MET EN MEMOIRE LES OBJETS CATALOGUE NECESSAIRES
!              0 : ON NE FAIT PAS DE JEVEUX CAR LES OBJETS SONT DEJA LA
!                  (POUR ETRE PLUS RAPIDE DANS CALCUL).
!
!     SORTIES:
!     NUCALC : NUMERO DU CALCUL ELEMENTAIRE A LANCER
!
! ----------------------------------------------------------------------
    integer :: iaoptt, lgco, iaopmo, ilopmo, iaopno, ilopno, iaopds
    integer :: iaoppa, npario, nparin, iamloc, ilmloc, iadsgd
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: optmod, jj, ianblc
!
! DEB-------------------------------------------------------------------
    ASSERT(memoir.eq.0 .or. memoir.eq.1)
    if (memoir .eq. 1) then
        call jeveuo('&CATA.TE.OPTTE', 'L', iaoptt)
        call jeveuo('&CATA.TE.OPTMOD', 'L', iaopmo)
        call jeveuo(jexatr('&CATA.TE.OPTMOD', 'LONCUM'), 'L', ilopmo)
        call jeveuo('&CATA.TE.NBLIGCOL', 'L', ianblc)
        lgco = zi(ianblc-1+1)
    endif
!
!     JJ = IOPTTE(OPT,TE)
    jj = zi(iaoptt-1+ (te-1)*lgco+opt)
    if (jj .eq. 0) then
!        -- LE TYPE_ELEMENT TE NE SAURA JAMAIS CALCULER L'OPTION OPT:
        nucalc = 0
    else
        optmod = iaopmo + zi(ilopmo-1+jj) - 1
        nucalc = zi(optmod-1+1)
    endif
end function
