subroutine te0599(option, nomte)
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
#include "asterfort/tecach.h"
#include "asterfort/xthddl.h"
#include "asterfort/xthini.h"
#include "asterfort/xvechp.h"
    character(len=16) :: option, nomte
!
!-----------------------------------------------------------------------
!
!     BUT: THERMIQUE LINEAIRE / ELEMENTS PRINCIPAUX X-FEM LINEAIRES
!          ECHANGE_PAROI POUR FISSURES X-FEM
!
!          OPTION : 'CHAR_THER_PARO_F' ET 'CHAR_THER_PARO_R'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!
!-----------------------------------------------------------------------
!
    integer :: ndim, nfh, nfe, igeom, nnop, jptint, jcface
    integer :: jlonch, jlst, itps, ihechp, jstno, jbasec
    integer :: heavn(27,5), ino, ig, jheavn, ncompn, jtab(7), iret
    integer :: itemp, ivectt, nddlno
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
    call jevech('PTEMPER', 'L', itemp)
    call jevech('PTEMPSR', 'L', itps)
!
!     SI LE COEFF D'ECHANGE PAROI EST NUL, IL N'Y A RIEN A FAIRE
    if (option .eq. 'CHAR_THER_PARO_R') then
        fonree = 'REEL'
        call jevech('PHECHPR', 'L', ihechp)
        if (abs(zr(ihechp)) .lt. r8prem()) goto 999
    else if (option.eq.'CHAR_THER_PARO_F') then
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
    call jevech('PSTANO', 'L', jstno)
    call jevech('PBASECO', 'L', jbasec)
!
!     CHAMPS OUT
    call jevech('PVECTTR', 'E', ivectt)
!
!     ELT DE REF PARENT : RECUP NDIM ET NNOP (NOEUDS PARENT)
!     -> RQ : 'RIGI' POUR LA FAMILLE DE PG EST DONC SANS CONSQUENCE
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nnop)
!
!     RECUP DE NFH (NBRE FCT HEAVISIDE) ET NFE (NBRE FCT SINGULIER)
    call xthini(nomte, nfh, nfe)
    nddlno = 1+nfh+nfe
!
!   RECUPERATION DE LA DEFINITION DES FONCTIONS HEAVISIDES
    if (nfh.gt.0) then
      call jevech('PHEA_NO', 'L', jheavn)
      call tecach('OOO', 'PHEA_NO', 'L', iret, nval=7,&
                itab=jtab)
      ncompn = jtab(2)/jtab(3)
      ASSERT(ncompn.eq.5)
      do ino = 1, nnop
        do ig = 1 , ncompn
          heavn(ino,ig) = zi(jheavn-1+ncompn*(ino-1)+ig)
        enddo
      enddo
    endif
!
! ----------------------------------------------------------------------
! --- CALCUL DU VECTEUR ELEMENTAIRE DU A ECHANGE_PAROI
! ----------------------------------------------------------------------
!
    call xvechp(ndim, elrefp, nnop, igeom, itemp,&
                itps, ihechp, jptint, jcface,&
                jlonch, jlst, jbasec, nfh, nfe,&
                fonree, ivectt, heavn)
!
! ----------------------------------------------------------------------
! --- SUPPRESSION DES DDLS SUPERFLUS
! ----------------------------------------------------------------------
!
    call xthddl(nfh, nddlno, nnop, zi(jstno), option,&
                nomte, vect=zr(ivectt))
!
999 continue
!
end subroutine
