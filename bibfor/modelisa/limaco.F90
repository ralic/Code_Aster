subroutine limaco(char, motfac, noma, nomo, ndim,&
                  nzoco, ligret)
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
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/cacoco.h'
    include 'asterfort/cacoeq.h'
    include 'asterfort/capoco.h'
    include 'asterfort/cfbord.h'
    include 'asterfort/cfdisi.h'
    include 'asterfort/cfdisl.h'
    include 'asterfort/cfnodb.h'
    include 'asterfort/chckco.h'
    include 'asterfort/dimeco.h'
    include 'asterfort/dimecz.h'
    include 'asterfort/elimco.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/limacx.h'
    include 'asterfort/listco.h'
    include 'asterfort/mmprel.h'
    include 'asterfort/poinco.h'
    include 'asterfort/quadco.h'
    include 'asterfort/sanscc.h'
    include 'asterfort/sansco.h'
    include 'asterfort/tablco.h'
    include 'asterfort/typeco.h'
    include 'asterfort/xconta.h'
    include 'asterfort/xmacon.h'
    character(len=8) :: char
    character(len=8) :: noma
    character(len=8) :: nomo
    character(len=16) :: motfac
    character(len=19) :: ligret
    integer :: nzoco, ndim
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - LECTURE DONNEES)
!
! LECTURE DES MAILLES DE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
! IN  NDIM   : NOMBRE DE DIMENSIONS DU PROBLEME
! IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
! OUT LIGRET : LIGREL D'ELEMENTS TARDIFS DU CONTACT
!
!
!
!
    character(len=24) :: defico
    integer :: iform
    integer :: indqua
    logical :: lmail, ltfcm
    integer :: nsuco, nmaco, nnoco
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
    nsuco = 0
    nmaco = 0
    nnoco = 0
!
! --- TYPE DE FORMULATION (DISCRETE/CONTINUE/XFEM)
!
    iform = cfdisi(defico,'FORMULATION')
    lmail = cfdisl(defico,'FORMUL_MAILLEE')
!
! --- LECTURE DES MAILLES/NOEUDS DE CONTACT
!
    if (lmail) then
!
! --- TRAITEMENT DU CAS DES MAILLES QUADRATIQUES
! --- INDQUA VAUT 0 SI LINEARISATION DES MAILLES QUADRATIQUES (CACOEQ)
!
        call quadco(char, indqua)
!
! --- DETERMINATION DU NOMBRE TOTAL DE SURFACES
!
        call poinco(char, motfac, noma, nzoco, nsuco)
!
! --- STOCKAGE DES MAILLES ET NOEUDS DE CONTACT
!
        call listco(char, motfac, noma, nomo, nzoco,&
                    nmaco, nnoco)
!
! --- ELIMINATION DES MAILLES ET NOEUDS DE CONTACT EN DOUBLE
!
        call elimco(char, noma, nomo, indqua, nzoco,&
                    nsuco, nmaco, nnoco)
!
! --- CONSTRUCTION DES CONNECTIVITES INVERSES
!
        call tablco(char, noma, nsuco, nmaco, nnoco)
!
! --- CONSTRUCTION DU VECTEUR D'INFORMATION PAR ZONE
!
        call dimecz(char, noma, nzoco, iform)
!
! --- CONSTRUCTION DU VECTEUR D'INFORMATION SUR LES LONGUEURS
!
        call dimeco(char, ndim, nzoco, nsuco, nmaco,&
                    nnoco)
!
! --- TRAITEMENT MOT-CLEF SANS_GROUP_NO
!
        call sansco(char, motfac, noma)
!
! --- TRAITEMENT MOT-CLEFS SPECIFIQUES FORMULATION CONTINUE
!
        if (iform .eq. 2) then
            call sanscc(char, motfac, noma)
        endif
!
! --- RECHERCHE DE NOEUDS COMMUNS AUX SURFACES MAITRES ET ESCLAVES
!
        call cfnodb(char)
!
! --- VERIFICATION DE LA COHERENCE DES DIMENSIONS
!
        call cfbord(char, noma)
!
! --- CONSTRUCTION DU TABLEAU POUR TYPE DE NOEUD
!
        call typeco(char, noma)
!
! --- CREATION DES RELATIONS LINEAIRES POUR MAILLES QUADRATIQUES
!
        if (indqua .eq. 0) then
            call cacoeq(char, noma)
        endif
!
! --- RECUPERATION DES CARACTERISTIQUES DE POUTRE
!
        call capoco(char, motfac)
!
! --- RECUPERATION DES CARACTERISTIQUES DE COQUE
!
        call cacoco(char, motfac, noma)
!
! --- VERIFICATION DES TANGENTES/NORMALES
!
        call chckco(char, noma, ndim)
!
! --- AJOUT DES ELEMENTS TARDIFS
!
        if (iform .eq. 2) then
            call mmprel(char, noma, nomo, ligret)
        endif
    else
        if (iform .eq. 3) then
            call limacx(char, motfac, ndim, nzoco)
            ltfcm = cfdisl(defico,'CONT_XFEM_GG')
            if (ltfcm) then
                call xmacon(char, noma, nomo)
            endif
            call xconta(char, noma, nomo, ndim)
        else
            call assert(.false.)
        endif
    endif
!
    call jedema()
end subroutine
