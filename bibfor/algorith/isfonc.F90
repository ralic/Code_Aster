function isfonc(fonact, nomfoz)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    logical :: isfonc
    include 'asterfort/assert.h'
    integer :: fonact(*)
    character(len=*) :: nomfoz
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE)
!
! DIT SI UNE FONCTIONNALITE EST ACTIVEE
!
! ---------------------------------------------------------------------
!
!
! IN  FONACT : FONCTIONNALITES SPECIFIQUES ACTIVEES
! IN  NOMFOZ : NOM DE LA FONCTIONNALITE
!       RECH_LINE          :  RECHERCHE LINEAIRE
!       PILOTAGE           :  PILOTAGE
!       NEWTON_KRYLOV      :  METHODE DE NEWTON_KRYLOV
!       DEBORST            :  ALGORITHME DE DE BORST
!       SOUS_STRUC         :  CALCUL PAR SOUS-STRUCTURATION
!       PROJ_MODAL         :  PROJECTION MODALE
!       IMPLEX             :  METHODE IMPLEX
!       EXPLICITE          :  METHODE EXPLICITE
!       CONTACT            :  CONTACT DISCRET OU CONTINU OU XFEM
!       CONT_DISCRET       :  CONTACT DISCRET
!       CONT_CONTINU       :  CONTACT CONTINU
!       CONT_XFEM          :  CONTACT XFEM
!       CONTACT_INIT       :  CONTACT INITIAL
!       FROT_DISCRET       :  FROTTEMENT DISCRET
!       FROT_CONTINU       :  FROTTEMENT CONTINU
!       FROT_XFEM          :  FROTTEMENT XFEM
!       BOUCLE_EXTERNE     :  PRESENCE D'UNE BOUCLE EXTERNE
!       BOUCLE_EXT_GEOM    :  PRESENCE D'UNE BOUCLE EXTERNE POUR
!                             LA GEOMETRIE
!       BOUCLE_EXT_FROT    :  PRESENCE D'UNE BOUCLE EXTERNE POUR
!                             LE FROTTEMENT
!       BOUCLE_EXT_CONT    :  PRESENCE D'UNE BOUCLE EXTERNE POUR
!                             LE CONTACT
!       FROT_NEWTON        :  NEWTON GENERALISE POUR LE CONTACT CONTINU
!                             FROTTEMENT
!       CONT_NEWTON        :  NEWTON GENERALISE POUR LE CONTACT CONTINU
!                             CONTACT
!       GEOM_NEWTON        :  NEWTON GENERALISE POUR LE CONTACT CONTINU
!                             GEOMETRIE
!       CONT_ALL_VERIF     :  CONTACT SANS CALCUL SUR TOUTES LES ZONES
!       LIAISON_UNILATER   :  LIAISON UNILATERALE
!       ELT_CONTACT        :  ELEMENTS DE CONTACT (CONTINU/XFEM)
!       ELT_FROTTEMENT     :  ELEMENTS DE FROTTEMENT (CONTINU/XFEM)
!       DIS_CHOC           :  ELEMENTS DIS_CHOC
!       GD_ROTA            :  ELEMENTS DE STRUCTURE EN GRANDES ROTATIONS
!       XFEM               :  ELEMENTS XFEM
!       EXI_STRX           :  ELEMENTS UTILISANT STRX (PMF)
!       RESI_REFE          :  CONVERGENCE PAR RESIDU DE REFERENCE
!       RESI_COMP          :  CONVERGENCE NORME PAR FORCE NODALE CMP
!       DIRI_CINE          :  PRESENCE DE CHARGEMENTS DE DIRICHLET
!                             DE TYPE ELIMINATION (AFFE_CHAR_CINE)
!       FORCE_SUIVEUSE     :  CHARGEMENT SUIVEUR
!       LAPLACE            :  FORCE DE LAPLACE
!       DIDI               :  FORCE DE TYPE DIFF. DIRICHLET
!       THM                :  MODELISATION THM
!       MACR_ELEM_STAT     :  MACRO-ELEMENTS STATIQUES
!       ENDO_NO            :  MODELISATION ENDO AUX NOEUDS *_GVNO
!
!       ENERGIE            :  CALCUL DES ENERGIES
!       CRIT_STAB          :  CALCUL DE STABILITE
!       DDL_STAB           :  DDLS IRREVERSIBLES DANS CRIT_STAB
!       MODE_VIBR          :  CALCUL DE MODES VIBRATOIRES
!       ERRE_TEMPS_THM     :  CALCUL DE L'ERREUR EN TEMPS POUR LA THM
!       EXI_VARC           :  PRESENCE DE VARIABLES DE COMMANDES
!       REUSE              :  CONCEPT RE-ENTRANT
!       FETI               :  SOLVEUR FETI
!       LDLT               :  SOLVEUR LDLT
!       MULT_FRONT         :  SOLVEUR MULT_FRONT
!       GCPC               :  SOLVEUR GCPC
!       MUMPS              :  SOLVEUR MUMPS
!       PETSC              :  SOLVEUR PETSC
!       LDLT_SP            :  PRECONDITIONNEUR LDLT_SP
!       MATR_DISTRIBUEE    :  MATRICES DISTRIBUEES
!
! DERNIER NUMERO UTILISE: 56
!
! ---------------------------------------------------------------------
!
    character(len=24) :: nomfon
!
! ---------------------------------------------------------------------
!
    nomfon = nomfoz
!
    if (nomfon .eq. 'RECH_LINE') then
        isfonc = fonact(1).eq.1
    else if (nomfon.eq.'PILOTAGE') then
        isfonc = fonact(2).eq.1
!
    else if (nomfon.eq.'CONTACT') then
        isfonc = (fonact(4).eq.1) .or. (fonact(5).eq.1) .or. (fonact( 9).eq.1)
    else if (nomfon.eq.'LIAISON_UNILATER') then
        isfonc = fonact(12).eq.1
    else if (nomfon.eq.'ELT_CONTACT') then
        isfonc = fonact(26).eq.1
    else if (nomfon.eq.'ELT_FROTTEMENT') then
        isfonc = fonact(27).eq.1
    else if (nomfon.eq.'CONT_DISCRET') then
        isfonc = fonact(4).eq.1
    else if (nomfon.eq.'CONT_CONTINU') then
        isfonc = fonact(5).eq.1
    else if (nomfon.eq.'CONT_XFEM') then
        isfonc = fonact(9) .eq.1
    else if (nomfon.eq.'CONTACT_INIT') then
        isfonc = fonact(17).eq.1
    else if (nomfon.eq.'DIS_CHOC') then
        isfonc = fonact(29).eq.1
    else if (nomfon.eq.'FROT_DISCRET') then
        isfonc = fonact(3).eq.1
    else if (nomfon.eq.'FROT_CONTINU') then
        isfonc = fonact(10).eq.1
    else if (nomfon.eq.'FROT_XFEM') then
        isfonc = fonact(25).eq.1
    else if (nomfon.eq.'BOUCLE_EXT_GEOM') then
        isfonc = fonact(31).eq.1
    else if (nomfon.eq.'BOUCLE_EXT_FROT') then
        isfonc = fonact(32).eq.1
    else if (nomfon.eq.'BOUCLE_EXT_CONT') then
        isfonc = fonact(33).eq.1
    else if (nomfon.eq.'BOUCLE_EXTERNE') then
        isfonc = fonact(34).eq.1
    else if (nomfon.eq.'GEOM_NEWTON') then
        isfonc = fonact(55).eq.1
    else if (nomfon.eq.'FROT_NEWTON') then
        isfonc = fonact(47).eq.1
    else if (nomfon.eq.'CONT_NEWTON') then
        isfonc = fonact(53).eq.1
    else if (nomfon.eq.'CONT_ALL_VERIF') then
        isfonc = fonact(38).eq.1
!
    else if (nomfon.eq.'XFEM') then
        isfonc = fonact(6).eq.1
    else if (nomfon.eq.'DEBORST') then
        isfonc = fonact(7).eq.1
!
    else if (nomfon.eq.'RESI_REFE') then
        isfonc = fonact(8).eq.1
    else if (nomfon.eq.'RESI_COMP') then
        isfonc = fonact(35).eq.1
    else if (nomfon.eq.'DIRI_CINE') then
        isfonc = fonact(36).eq.1
    else if (nomfon.eq.'FORCE_SUIVEUSE') then
        isfonc = fonact(13).eq.1
    else if (nomfon.eq.'LAPLACE') then
        isfonc = fonact(20).eq.1
    else if (nomfon.eq.'DIDI') then
        isfonc = fonact(22).eq.1
!
    else if (nomfon.eq.'MACR_ELEM_STAT') then
        isfonc = fonact(14).eq.1
    else if (nomfon.eq.'GD_ROTA') then
        isfonc = fonact(15) .eq.1
    else if (nomfon.eq.'ENDO_NO') then
        isfonc = fonact(40).eq.1
    else if (nomfon.eq.'CRIT_STAB') then
        isfonc = fonact(18) .eq.1
    else if (nomfon.eq.'DDL_STAB') then
        isfonc = fonact(49) .eq.1
    else if (nomfon.eq.'MODE_VIBR') then
        isfonc = fonact(19).eq.1
    else if (nomfon.eq.'ERRE_TEMPS_THM') then
        isfonc = fonact(21).eq.1
    else if (nomfon.eq.'SOUS_STRUC') then
        isfonc = fonact(24).eq.1
    else if (nomfon.eq.'IMPLEX') then
        isfonc = fonact(28).eq.1
    else if (nomfon.eq.'EXI_VARC') then
        isfonc = fonact(30).eq.1
    else if (nomfon.eq.'THM') then
        isfonc = fonact(37).eq.1
    else if (nomfon.eq.'REUSE') then
        isfonc = fonact(39).eq.1
!
    else if (nomfon.eq.'FETI') then
        isfonc = fonact(11).eq.1
    else if (nomfon.eq.'LDLT') then
        isfonc = fonact(41).eq.1
    else if (nomfon.eq.'MULT_FRONT') then
        isfonc = fonact(42).eq.1
    else if (nomfon.eq.'GCPC') then
        isfonc = fonact(43).eq.1
    else if (nomfon.eq.'MUMPS') then
        isfonc = fonact(44).eq.1
    else if (nomfon.eq.'PETSC') then
        isfonc = fonact(45).eq.1
    else if (nomfon.eq.'LDLT_SP') then
        isfonc = fonact(46).eq.1
    else if (nomfon.eq.'NEWTON_KRYLOV') then
        isfonc = fonact(48).eq.1
    else if (nomfon.eq.'ENERGIE') then
        isfonc = fonact(50).eq.1
    else if (nomfon.eq.'PROJ_MODAL') then
        isfonc = fonact(51).eq.1
    else if (nomfon.eq.'MATR_DISTRIBUEE') then
        isfonc = fonact(52).eq.1
    else if (nomfon.eq.'EXPLICITE') then
        isfonc = fonact(54).eq.1
    else if (nomfon.eq.'EXI_STRX') then
        isfonc = fonact(56).eq.1
!
    else
        call assert(.false.)
    endif
!
end function
