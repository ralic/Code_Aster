subroutine te0572(option, nomte)
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/elref1.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/iselli.h"
#include "asterfort/jevech.h"
#include "asterfort/tecach.h"
#include "asterfort/xmasth.h"
#include "asterfort/xthddl.h"
#include "asterfort/xthini.h"
    character(len=16) :: option, nomte
!
!-----------------------------------------------------------------------
!
!     BUT: CALCUL DES MATRICES DE MASSE ELEMENTAIRES EN THERMIQUE
!          LINEAIRE, ELEMENTS X-FEM LINEAIRES
!
!          OPTION : 'MASS_THER'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!
!-----------------------------------------------------------------------
!
    integer :: ndim, nfh, nfe, igeom, nnop, jpintt, imate, itps, jstno
    integer :: imattt, jcnset, jheavt, jlonch, jbaslo, jlsn, jlst, nddlno
    integer :: heavn(27,5), ino, ig, jheavn, ncompn, jtab(7), iret
    character(len=8) :: elrefp
!
! ----------------------------------------------------------------------
! --- PREALABLES AU CALCUL DE LA MASSE ELEMENTAIRE
! ----------------------------------------------------------------------
!
!     ON INTERDIT LES ELTS QUADRATIQUES
    call elref1(elrefp)
    ASSERT(iselli(elrefp))
!
!     CHAMPS IN 'CLASSIQUES'
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PTEMPSR', 'L', itps)
!     CHAMPS IN X-FEM
    call jevech('PSTANO', 'L', jstno)
    call jevech('PPINTTO', 'L', jpintt)
    call jevech('PCNSETO', 'L', jcnset)
    call jevech('PHEAVTO', 'L', jheavt)
    call jevech('PLONCHA', 'L', jlonch)
    call jevech('PBASLOR', 'L', jbaslo)
    call jevech('PLSN', 'L', jlsn)
    call jevech('PLST', 'L', jlst)
!     CHAMP OUT
    call jevech('PMATTTR', 'E', imattt)
!
!     ELT DE REF PARENT : RECUP NDIM ET NNOP (NOEUDS PARENT)
!     -> RQ : 'RIGI' POUR LA FAMILLE DE PG EST DONC SANS CONSQUENCE
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nnop)
!
!     NBRE DE DDLS PAR NOEUD
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
! --- CALCUL DE LA MATRICE DE MASSE ELEMENTAIRE
! ----------------------------------------------------------------------
!
    call xmasth(ndim, elrefp, nnop, imate, itps,&
                igeom, zi(jlonch), zi(jcnset), jpintt, zr(jlsn),&
                zr(jlst), heavn, zr(jbaslo), zi(jheavt), nfh, nfe,&
                zr(imattt))
!
! ----------------------------------------------------------------------
! --- SUPPRESSION DES DDLS SUPERFLUS
! ----------------------------------------------------------------------
!
!     SUPPRESSION DES DDLS SUPERFLUS
    call xthddl(nfh, nddlno, nnop, zi(jstno), option,&
                nomte, mat=zr(imattt))
!
end subroutine
