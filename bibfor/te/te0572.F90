subroutine te0572(option, nomte)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/elref1.h'
    include 'asterfort/elref4.h'
    include 'asterfort/iselli.h'
    include 'asterfort/jevech.h'
    include 'asterfort/xmasth.h'
    include 'asterfort/xthddl.h'
    include 'asterfort/xthini.h'
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
    integer :: ndim, nfh, nfe, ibid, igeom, nnop, jpintt, imate, itps, jstno
    integer :: imattt, jcnset, jheavt, jlonch, jbaslo, jlsn, jlst, nddlno
    real(kind=8) :: r8bid
    character(len=8) :: elrefp
!
! ----------------------------------------------------------------------
! --- PREALABLES AU CALCUL DE LA MASSE ELEMENTAIRE
! ----------------------------------------------------------------------
!
!     ON INTERDIT LES ELTS QUADRATIQUES
    call elref1(elrefp)
    call assert(iselli(elrefp))
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
    call elref4(' ', 'RIGI', ndim, nnop, ibid,&
                ibid, ibid, ibid, ibid, ibid)
!
!     NBRE DE DDLS PAR NOEUD
    call xthini(nomte, nfh, nfe)
    nddlno = 1+nfh+nfe
!
! ----------------------------------------------------------------------
! --- CALCUL DE LA MATRICE DE MASSE ELEMENTAIRE
! ----------------------------------------------------------------------
!
    call xmasth(ndim, elrefp, nnop, imate, itps,&
                igeom, zi(jlonch), zi(jcnset), jpintt, zr(jlsn),&
                zr(jlst), zr(jbaslo), zi(jheavt), nfh, nfe,&
                zr(imattt))
!
! ----------------------------------------------------------------------
! --- SUPPRESSION DES DDLS SUPERFLUS
! ----------------------------------------------------------------------
!
!     SUPPRESSION DES DDLS SUPERFLUS
    call xthddl(nfh, nddlno, nnop, zi(jstno), option,&
                nomte, zr(imattt), r8bid)
!
end subroutine
