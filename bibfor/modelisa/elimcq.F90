subroutine elimcq(char, noma, indqua, nzoco, nsuco,&
                  nnoco)
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
    include 'asterfort/cfleq8.h'
    include 'asterfort/cfmeno.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    character(len=8) :: char
    character(len=8) :: noma
    integer :: indqua
    integer :: nzoco, nsuco, nnoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES)
!
! ELIMINATION AU SEIN DE CHAQUE SURFACE DE CONTACT POTENTIELLE DES
! NOEUDS ET MAILLES REDONDANTS. MODIFICATION DES POINTEURS ASSOCIES.
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NOMA   : NOM DU MAILLAGE
! IN  INDQUA : VAUT 0 LORSQUE L'ON DOIT TRAITER LES NOEUDS MILIEUX
!                     A PART
!              VAUT 1 LORSQUE L'ON DOIT TRAITER LES NOEUDS MILIEUX
!                     NORMALEMENT
! IN  NZOCO  : NOMBRE TOTAL DE ZONES DE CONTACT
! IN  NSUCO  : NOMBRE TOTAL DE SURFACES DE CONTACT
! I/O NNOCO  : NOMBRE TOTAL DE NOEUDS DES SURFACES
!
!
!
!
    integer :: nnoco0
    character(len=24) :: defico
    character(len=24) :: poinsn, listno
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    defico = char(1:8)//'.CONTACT'
    listno = '&&ELIMCQ.TRAVNO'
    poinsn = '&&ELIMCQ.ELIMNO'
!
! --- CAS DES QUAD8
!
    if (indqua .eq. 0) then
!
! ----- CREATION D'UNE LISTE DES NOEUDS MILIEUX DES ARETES POUR
! ----- LES MAILLES QUADRATIQUES
!
        nnoco0 = nnoco
        call cfleq8(noma, defico, nzoco, nsuco, nnoco,&
                    nnoco0, listno, poinsn)
!
! ----- MISE A JOUR DE LA LISTE DES NOEUDS APRES ELIMINATION
!
        if (nnoco0 .ne. nnoco) then
            call cfmeno(defico, nsuco, nnoco0, listno, poinsn,&
                        nnoco)
        endif
!
    endif
!
    call jedetr(listno)
    call jedetr(poinsn)
!
    call jedema()
end subroutine
