subroutine cfnbsf(defico, isurf, typent, nbent, jdec)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=24) :: defico
    character(len=4) :: typent
    integer :: isurf, jdec
    integer :: nbent
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - UTILITAIRE)
!
! ACCES AUX NOEUDS/MAILLES D'UNE ZONE DONNEE
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  TYPENT : TYPE D'ENTITE
!               NOEU  NBRE NOEUDS ATTACHES A LA SURFACE
!               MAIL  NBRE MAILLES ATTACHEES A LA SURFACE
! IN  ISURF  : NUMERO DE LA SURFACE
! OUT NBENT  : NOMBRE D'ENTITES
! OUT JDEC   : DECALAGE DANS LES VECTEURS POUR LE PREMIER DE LA SURFACE
!               NOEU  DEFICO(1:16)//'.NOEUCO'
!               MAIL  DEFICO(1:16)//'.MAILCO'
!
!
!
!
    character(len=24) :: psurno, psurma
    integer :: jsuno, jsuma
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- RECUPERATION DE QUELQUES DONNEES
!
    psurno = defico(1:16)//'.PSUNOCO'
    psurma = defico(1:16)//'.PSUMACO'
    call jeveuo(psurno, 'L', jsuno)
    call jeveuo(psurma, 'L', jsuma)
!
! --- INITIALISATIONS
!
    if (typent .eq. 'NOEU') then
        nbent = zi(jsuno+isurf) - zi(jsuno+isurf-1)
        jdec = zi(jsuno+isurf-1)
    else if (typent.eq.'MAIL') then
        nbent = zi(jsuma+isurf) - zi(jsuma+isurf-1)
        jdec = zi(jsuma+isurf-1)
    else
        call assert(.false.)
    endif
!
    call jedema()
!
end subroutine
