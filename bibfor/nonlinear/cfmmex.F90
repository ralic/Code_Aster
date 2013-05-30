subroutine cfmmex(defico, typexc, izone, numnoe, suppok)
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
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    character(len=24) :: defico
    character(len=4) :: typexc
    integer :: izone
    integer :: numnoe
    integer :: suppok
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - UTILITAIRE)
!
! DIT SI LE NOEUD FAIT PARTIE D'UNE LISTE DONNE PAR L'UTILISATEUR
!   SANS_GROUP_NO/SANS_NOEUD
!   SANS_GROUP_NO_FR/SANS_NOEUD_FR
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  TYPEXC : TYPE D'EXCLUSION
!               'FROT' DONNE PAR SANS_*_FR
!               'CONT' DONNE PAR SANS_*
! IN  IZONE  : NUMERO DE LA ZONE DE CONTACT
! IN  NUMNOE : NUMERO ABSOLUE DU NOEUD A CHERCHER
! OUT SUPPOK : VAUT 1 SI LE NOEUD FAIT PARTIE DES NOEUDS EXCLUS
!
!
!
!
    character(len=24) :: sansno, psans, frotno, pfrot
    integer :: jsanc, jpsanc, jsanf, jpsanf
    integer :: jsans, jpsans
    integer :: nsans, numsan, k
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES OBJETS
!
    sansno = defico(1:16)//'.SSNOCO'
    psans = defico(1:16)//'.PSSNOCO'
    frotno = defico(1:16)//'.SANOFR'
    pfrot = defico(1:16)//'.PSANOFR'
!
! --- INITIALISATIONS
!
    suppok = 0
    if (typexc .eq. 'CONT') then
        call jeveuo(sansno, 'L', jsanc)
        call jeveuo(psans, 'L', jpsanc)
        jsans = jsanc
        jpsans = jpsanc
    else if (typexc.eq.'FROT') then
        call jeveuo(frotno, 'L', jsanf)
        call jeveuo(pfrot, 'L', jpsanf)
        jsans = jsanf
        jpsans = jpsanf
    else
        call assert(.false.)
    endif
    nsans = zi(jpsans+izone) - zi(jpsans+izone-1)
!
! --- REPERAGE SI LE NOEUD EST UN NOEUD DE LA LISTE
!
    do 30 k = 1, nsans
        numsan = zi(jsans+zi(jpsans+izone-1)+k-1)
        if (numnoe .eq. numsan) then
            suppok = 1
            goto 40
        endif
30  end do
40  continue
!
    call jedema()
end subroutine
