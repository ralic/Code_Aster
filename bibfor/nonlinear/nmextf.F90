subroutine nmextf(motfac, iocc, extrcp)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit      none
    include 'asterc/getvid.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    character(len=16) :: motfac
    integer :: iocc
    character(len=8) :: extrcp
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (EXTRACTION - LECTURE)
!
! LECTURE TYPE EXTRACTION SUR LES COMPOSANTES
!
! ----------------------------------------------------------------------
!
!
! IN  MOTFAC : MOT-FACTEUR POUR LIRE
! IN  IOCC   : OCCURRENCE DU MOT-CLEF FACTEUR MOTFAC
! OUT EXTRCP : TYPE D'EXTRACTION SUR LES COMPOSANTES
!               ' ' POUR LES VALEURS OU NOM DE LA FORMULE
!
! ----------------------------------------------------------------------
!
    character(len=8) :: typext
    integer :: n1
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call getvtx(motfac, 'EVAL_CMP', iocc, iarg, 1,&
                typext, n1)
    if (typext .eq. 'VALE') then
        extrcp = ' '
    else if (typext.eq.'FORMULE') then
        call getvid(motfac, 'FORMULE', iocc, iarg, 1,&
                    extrcp, n1)
    else
        call assert(.false.)
    endif
!
end subroutine
