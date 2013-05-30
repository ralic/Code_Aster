subroutine elimco(char, noma, nomo, indqua, nzoco,&
                  nsuco, nmaco, nnoco)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
    include 'jeveux.h'
    include 'asterfort/cfdisi.h'
    include 'asterfort/cflecq.h'
    include 'asterfort/cflema.h'
    include 'asterfort/cfleno.h'
    include 'asterfort/cfmema.h'
    include 'asterfort/cfmeno.h'
    include 'asterfort/elimcq.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    character(len=8) :: char
    character(len=8) :: noma, nomo
    integer :: indqua
    integer :: nzoco, nsuco, nmaco, nnoco
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
! IN  NOMO   : NOM DU MODELE
! IN  INDQUA : VAUT 0 LORSQUE L'ON DOIT TRAITER LES NOEUDS MILIEUX
!                     A PART
!              VAUT 1 LORSQUE L'ON DOIT TRAITER LES NOEUDS MILIEUX
!                     NORMALEMENT
! IN  NZOCO  : NOMBRE TOTAL DE ZONES DE CONTACT
! IN  NSUCO  : NOMBRE TOTAL DE SURFACES DE CONTACT
! I/O NMACO  : NOMBRE TOTAL DE MAILLES DES SURFACES
! I/O NNOCO  : NOMBRE TOTAL DE NOEUDS DES SURFACES
!
!
!
!
    integer :: nnoco0, nmaco0
    character(len=24) :: defico
    character(len=24) :: poinsm, listma
    character(len=24) :: poinsn, listno
    integer :: iform
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- LECTURE DES STRUCTURES DE DONNEES DE CONTACT
!
    defico = char(1:8)//'.CONTACT'
!
! --- TYPE DE FORMULATION (DISCRETE/CONTINUE/XFEM)
!
    iform = cfdisi(defico,'FORMULATION')
!
! --- NOMBRE INITIAL
!
    nmaco0 = nmaco
    nnoco0 = nnoco
!
! --- REPERAGE DES MAILLES REDONDANTES
!
    listma = '&&ELIMCO.TRAVMA'
    poinsm = '&&ELIMCO.ELIMMA'
    call cflema(defico, nsuco, nmaco0, listma, poinsm,&
                nmaco)
!
! --- MISE A JOUR DE LA LISTE DES MAILLES APRES ELIMINATION
!
    if (nmaco0 .ne. nmaco) then
        call cfmema(defico, nsuco, nmaco0, listma, poinsm,&
                    nmaco)
    endif
    call jedetr(listma)
    call jedetr(poinsm)
!
! --- REPERAGE DES NOEUDS REDONDANTS
!
    listno = '&&ELIMCO.TRAVNO'
    poinsn = '&&ELIMCO.ELIMNO'
    call cfleno(defico, nsuco, nnoco0, listno, poinsn,&
                nnoco)
!
! --- MISE A JOUR DE LA LISTE DES NOEUDS APRES ELIMINATION
!
    if (nnoco0 .ne. nnoco) then
        call cfmeno(defico, nsuco, nnoco0, listno, poinsn,&
                    nnoco)
    endif
    call jedetr(listno)
    call jedetr(poinsn)
!
! --- CAS DES COQUES 3D (ON REPERE LE NOEUD MILIEU DES TRIA7/QUAD9)
!
    nnoco0 = nnoco
    call cflecq(iform, noma, nomo, defico, nsuco,&
                nnoco0, listno, poinsn, nnoco)
!
! --- MISE A JOUR DE LA LISTE DES NOEUDS APRES ELIMINATION
!
    if (nnoco0 .ne. nnoco) then
        call cfmeno(defico, nsuco, nnoco0, listno, poinsn,&
                    nnoco)
    endif
    call jedetr(listno)
    call jedetr(poinsn)
!
! --- TRAITEMENT MOT-CLEFS SPECIFIQUES FORMULATION DISCRETE
!
    if (iform .eq. 1) then
        call elimcq(char, noma, indqua, nzoco, nsuco,&
                    nnoco)
    endif
!
    call jedema()
end subroutine
