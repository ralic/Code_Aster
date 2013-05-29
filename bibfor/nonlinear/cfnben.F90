subroutine cfnben(defico, posent, typent, nbent, jdecen)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    character(len=6) :: typent
    integer :: posent
    integer :: nbent
    integer :: jdecen
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - UTILITAIRE)
!
! ACCES AUX TABLEAUX DE CONNECTIVITES
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD DE DEFINITION DU CONTACT
! IN  POSENT : POSITION DE L'ENTITE DANS LES SD CONTACT
! IN  TYPENT : TYPE D'ENTITE
!               'CONINV' POSENT EST UN NOEUD
!                  -> ON ACCEDE AUX MAILLES ATTACHEES A CE NOEUD
!                     (CONNECTIVITE INVERSE)
!               'CONNEX' POSENT EST UNE MAILLE
!                  -> ON ACCEDE AUX NOEUDS ATTACHES A CETTE MAILLE
!                     (CONENCTIVITE DIRECTE)
! OUT NBENT  : NOMBRE D'ENTITES ATTACHES
! OUT JDECEN : DECALAGE POUR TABLEAU
!
!
!
!
    character(len=24) :: pnoma, pmano
    integer :: jpono, jpoma
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- RECUPERATION DE QUELQUES DONNEES
!
    pnoma = defico(1:16)//'.PNOMACO'
    pmano = defico(1:16)//'.PMANOCO'
    call jeveuo(pnoma, 'L', jpono)
    call jeveuo(pmano, 'L', jpoma)
!
! --- INITIALISATIONS
!
    if (typent .eq. 'CONNEX') then
        nbent = zi(jpono+posent) - zi(jpono+posent-1)
        jdecen = zi(jpono+posent-1)
    else if (typent.eq.'CONINV') then
        nbent = zi(jpoma+posent) - zi(jpoma+posent-1)
        jdecen = zi(jpoma+posent-1)
    else
        call assert(.false.)
    endif
!
    call jedema()
!
end subroutine
