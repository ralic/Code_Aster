subroutine te0594(option, nomte)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: sam.cuvilliez at edf.fr
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/iselli.h"
#include "asterfort/jevech.h"
#include "asterfort/xrechp.h"
#include "asterfort/xthddl.h"
#include "asterfort/xthini.h"
    character(len=16) :: option, nomte
!
!-----------------------------------------------------------------------
!
!     BUT: THERMIQUE LINEAIRE / ELEMENTS PRINCIPAUX X-FEM LINEAIRES
!          ECHANGE_PAROI POUR FISSURES X-FEM
!
!          OPTION : 'RIGI_THER_PARO_F' ET 'RIGI_THER_PARO_R'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!
!-----------------------------------------------------------------------
!
    integer :: ndim, nfh, nfe, igeom, nnop, jptint, jcface
    integer :: jlonch, jlst, itps, ihechp, jstno, jbasec, jlsn
    integer :: imattt, nddlno
    character(len=8) :: elrefp
    character(len=4) :: fonree
!
! ----------------------------------------------------------------------
! --- PREALABLES AU CALCUL
! ----------------------------------------------------------------------
!
!     ON INTERDIT LES ELTS QUADRATIQUES
    call elref1(elrefp)
    ASSERT(iselli(elrefp))
!
!     CHAMPS IN CLASSIQUES
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itps)
!
!     SI LE COEFF D'ECHANGE PAROI EST NUL, IL N'Y A RIEN A FAIRE
    if (option .eq. 'RIGI_THER_PARO_R') then
        fonree = 'REEL'
        call jevech('PHECHPR', 'L', ihechp)
        if (abs(zr(ihechp)) .lt. r8prem()) goto 999
    else if (option.eq.'RIGI_THER_PARO_F') then
        fonree = 'FONC'
        call jevech('PHECHPF', 'L', ihechp)
        if (zk8(ihechp) .eq. '&FOZERO ') goto 999
    else
        ASSERT(.false.)
    endif
!
!     CHAMPS IN X-FEM
    call jevech('PPINTER', 'L', jptint)
    call jevech('PCFACE', 'L', jcface)
    call jevech('PLONGCO', 'L', jlonch)
    call jevech('PLST', 'L', jlst)
    call jevech('PLSN', 'L', jlsn)
    call jevech('PSTANO', 'L', jstno)
    call jevech('PBASECO', 'L', jbasec)
!
!     CHAMPS OUT
    call jevech('PMATTTR', 'E', imattt)
!
!     ELT DE REF PARENT : RECUP NDIM ET NNOP (NOEUDS PARENT)
!     -> RQ : 'RIGI' POUR LA FAMILLE DE PG EST DONC SANS CONSQUENCE
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nnop)
!
!     RECUP DE NFH (NBRE FCT HEAVISIDE) ET NFE (NBRE FCT SINGULIER)
    call xthini(nomte, nfh, nfe)
    nddlno = 1+nfh+nfe
!
! ----------------------------------------------------------------------
! --- CALCUL DE LA MATRICE DE RIGIDITE ELEMENTAIRE DUE A ECHANGE_PAROI
! ----------------------------------------------------------------------
!
    call xrechp(ndim, elrefp, nnop, igeom, itps,&
                ihechp, jptint, jcface, jlonch,&
                jlst, jlsn, jbasec, nfh, nfe, fonree,&
                imattt)
!
! ----------------------------------------------------------------------
! --- SUPPRESSION DES DDLS SUPERFLUS
! ----------------------------------------------------------------------
!
    call xthddl(nfh, nddlno, nnop, zi(jstno), option,&
                nomte, mat=zr(imattt))
!
999 continue
!
end subroutine
