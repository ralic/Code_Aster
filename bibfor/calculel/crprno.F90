subroutine crprno(champ, base, nbnoeu, nequa)
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!     ------------------------------------------------------------------
!     CREATION ET ALLOCATION D'UNE STRUCTURE PROF_CHNO "CHAMP"
!     ------------------------------------------------------------------
! IN  CHAMP  : CH19: NOM DU PROF_CHNO A CREER
! IN  BASE   : CH1 : NOM DE LA BASE SUR LAQUELLE LE PROF_CHNO DOIT ETRE
!                    CREE
! IN  NBNOEU : I   : NOMBRE DE NOEUDS DU MAILLAGE ASSOCIE AU PROF_CHNO
! IN  NEQUA  : I   : NOMBRE DE CMPS TOTAL DU CHAMP (LONG(.VALE))
!     ------------------------------------------------------------------
!     PRECAUTIONS D'EMPLOI :
!       1) LE PROF_CHNO "CHAMP" NE DOIT PAS EXISTER
!       2) LE .PRNO N'EST PAS AFFECTE
!       3) ON SUPPOSE QUE LE PRNO EST VARIABLE EN CHAQUE NOEUD
!       4) LE DEEQ N'EST PAS AFFECTE DANS CETTE ROUTINE (VOIR PTEEQU)
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    include 'asterfort/crprn2.h'
    include 'asterfort/dismoi.h'
    integer :: nbnoeu, nequa, nec, ibid, ie
    character(len=*) :: champ, base
    character(len=8) :: cbid, gran
!     ------------------------------------------------------------------
    call dismoi('F', 'NOM_GD', champ, 'CHAM_NO', ibid,&
                gran, ie)
    call dismoi('F', 'NB_EC', gran, 'GRANDEUR', nec,&
                cbid, ie)
    call crprn2(champ, base, nbnoeu, nequa, nec)
end subroutine
