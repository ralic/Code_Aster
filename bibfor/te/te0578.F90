subroutine te0578(option, nomte)
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
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/elref1.h'
    include 'asterfort/elref4.h'
    include 'asterfort/iselli.h'
    include 'asterfort/jevech.h'
    include 'asterfort/xtelga.h'
    include 'asterfort/xthini.h'
    character(len=16) :: option, nomte
!
!-----------------------------------------------------------------------
!
!     BUT: THERMIQUE LINEAIRE / ELEMENTS X-FEM LINEAIRES
!          OPTION : 'TEMP_ELGA'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!
!-----------------------------------------------------------------------
!
    integer :: ndim, nfh, nfe, ibid, itempn, igeom, nnop, itempg, jpintt
    integer :: jcnset, jheavt, jlonch, jbaslo, jlsn, jlst
    character(len=8) :: elrefp
!
! ----------------------------------------------------------------------
! --- PREALABLES AU CALCUL
! ----------------------------------------------------------------------
!
!     ON INTERDIT LES ELTS QUADRATIQUES
    call elref1(elrefp)
    call assert(iselli(elrefp))
!
!     CHAMPS IN
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPER', 'L', itempn)
    call jevech('PPINTTO', 'L', jpintt)
    call jevech('PCNSETO', 'L', jcnset)
    call jevech('PHEAVTO', 'L', jheavt)
    call jevech('PLONCHA', 'L', jlonch)
    call jevech('PBASLOR', 'L', jbaslo)
    call jevech('PLSN', 'L', jlsn)
    call jevech('PLST', 'L', jlst)
!     CHAMPS OUT
    call jevech('PTEMPPG', 'E', itempg)
!
!     ELT DE REF PARENT : RECUP NDIM ET NNOP (NOEUDS PARENT)
!     -> RQ : 'RIGI' POUR LA FAMILLE DE PG EST DONC SANS CONSQUENCE
    call elref4(' ', 'RIGI', ndim, nnop, ibid,&
                ibid, ibid, ibid, ibid, ibid)
!
!     RECUP DE NFH (NBRE FCT HEAVISIDE) ET NFE (NBRE FCT SINGULIER)
    call xthini(nomte, nfh, nfe)
!
! ----------------------------------------------------------------------
! --- CALCUL DU CHAMP DE TEMPERATURE AUX POINTS DE GAUSS
! ----------------------------------------------------------------------
!
    call xtelga(ndim, elrefp, nnop, igeom, zr(itempn),&
                zi(jlonch), zi(jcnset), jpintt, zr(jlsn), zr(jlst),&
                zr(jbaslo), zi(jheavt), nfh, nfe, zr(itempg))
!
end subroutine
