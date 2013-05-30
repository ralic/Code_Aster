subroutine crsmos(nomsto, typroz, neq)
    implicit    none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/wkvect.h'
    character(len=*) :: nomsto, typroz
!-----------------------------------------------------------------------
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
! TOLE CRP_4
!-----------------------------------------------------------------------
!    BUT: CREER UN STOCKAGE_MORSE POUR UNE MATRICE PLEINE OU DIAGONALE
!
!    DETERMINER LA NUMEROTATION GENERALISEE A PARTIR D'UN MODE_MECA
!    OU D'UN MODE_GENE
!    LA NUMEROTATION SERA PAR DEFAUT PLEINE
!
! IN  JXOUT K19 NOMSTO  : NOM DU STOCKAGE A CREER
! IN        K*  TYPROZ : 'PLEIN' /'DIAG'
! IN        I   NEQ     : DIMENSION DE LA MATRICE
!-----------------------------------------------------------------------
!
!
!
    integer :: i, j, nterm, ico, jsmde, jsmdi, jsmhc, neq
    character(len=19) :: sto19
    character(len=5) :: typrof
!     ------------------------------------------------------------------
!
!
    call jemarq()
    sto19=nomsto
    typrof=typroz
!
    call assert(typrof.eq.'PLEIN' .or. typrof.eq.'DIAG')
    if (typrof .eq. 'DIAG') then
        nterm=neq
    else
        nterm=neq*(neq+1)/2
    endif
!
    call wkvect(sto19//'.SMHC', 'G V S', nterm, jsmhc)
    call wkvect(sto19//'.SMDI', 'G V I', neq, jsmdi)
    call wkvect(sto19//'.SMDE', 'G V I', 6, jsmde)
!
!
    zi(jsmde-1+1 ) = neq
    zi(jsmde-1+2) = nterm
    zi(jsmde-1+3) = 1
!
!
    if (typrof .eq. 'DIAG') then
        do 201 i = 1, neq
            zi(jsmdi+i-1) = i
            zi4(jsmhc+i-1) = i
201      continue
    else if (typrof.eq.'PLEIN') then
        ico=0
        do 202 i = 1, neq
            zi(jsmdi+i-1) = i*(i+1)/2
            do 203 j = 1, i
                ico=ico+1
                zi4(jsmhc-1+ico) = j
203          continue
202      continue
    endif
!
    call jedema()
end subroutine
