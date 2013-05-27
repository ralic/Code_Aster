subroutine cgcrtb(table, option, lmelas, cas, typfis,&
                  lmoda, nbprup, noprup, typrup)
!
    implicit none
!
    include 'asterfort/tbajpa.h'
    include 'asterfort/tbcrsd.h'
    integer :: nbprup
    logical :: lmelas, lmoda
    character(len=8) :: table, typrup(nbprup), typfis
    character(len=16) :: option, cas, noprup(nbprup)
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
! person_in_charge: samuel.geniaut at edf.fr
!
!     SOUS-ROUTINE DE L'OPERATEUR CALC_G
!
!     BUT : CREATION DE LA TABLE ISSUE DE CALC_G
!           ET AFFECTATION DES PARAMETRES
!
! ----------------------------------------------
!  IN :
!     TABLE : NOM DE LA TABLE
!     OPTION : OPTION DE CALCUL
!     LMELAS : .TRUE.  SI TYPE SD RESULTAT = MULT_ELAS
!              .FALSE. SINON
!     CAS    : '2D', '3D_LOCAL'  OU '3D_GLOBAL'
!     TYPFIS : TYPE D'OBJET POUR DECRIRE LE FOND DE FISSURE
!              'FONDFISS' OU 'FISSURE' OU 'THETA'
!     LMODA  : .TRUE.  SI TYPE SD RESULTAT = MODE_MECA
!              .FALSE. SINON
!
!  OUT :
!     NBPRUP : NOMBRE DE PARAMETRES
!     NOPRUP : NOMS DES PARAMETRES
!     TYPRUP : TYPES DES PARAMETRES
! ----------------------------------------------
!
    if ((option.eq.'CALC_G'.and.cas.eq.'2D'.and.typfis.ne.'FISSURE') .or.&
        (option.eq.'CALC_G_GLOB')) then
        nbprup = 3
        if (lmelas) then
            noprup(1) = 'NUME_CAS'
            typrup(1) = 'I'
            noprup(2) = 'NOM_CAS'
            typrup(2) = 'K16'
        else
            noprup(1) = 'NUME_ORDRE'
            typrup(1) = 'I'
            noprup(2) = 'INST'
            typrup(2) = 'R'
        endif
        noprup(3) = 'G'
        typrup(3) = 'R'
        elseif(option.eq.'CALC_G'.and.cas.eq.'3D_LOCAL'.and.&
     &       typfis.eq.'FISSURE')then
        nbprup = 6
        noprup(1) = 'NUME_FOND'
        typrup(1) = 'I'
        noprup(2) = 'NUME_ORDRE'
        typrup(2) = 'I'
        noprup(3) = 'INST'
        typrup(3) = 'R'
        noprup(4) = 'NUM_PT'
        typrup(4) = 'I'
        noprup(5) = 'ABSC_CURV'
        typrup(5) = 'R'
        noprup(6) = 'G'
        typrup(6) = 'R'
        elseif(option.eq.'CALC_G'.and.cas.eq.'2D'.and. typfis.eq.'FISSURE'&
    )then
        nbprup = 4
        noprup(1) = 'NUME_FOND'
        typrup(1) = 'I'
        noprup(2) = 'NUME_ORDRE'
        typrup(2) = 'I'
        noprup(3) = 'INST'
        typrup(3) = 'R'
        noprup(4) = 'G'
        typrup(4) = 'R'
        elseif(option.eq.'CALC_G'.and.cas.eq.'3D_LOCAL'.and.&
     &       typfis.ne.'FISSURE') then
        nbprup = 5
        if (lmelas) then
            noprup(1) = 'NUME_CAS'
            typrup(1) = 'I'
            noprup(2) = 'NOM_CAS'
            typrup(2) = 'K16'
        else
            noprup(1) = 'NUME_ORDRE'
            typrup(1) = 'I'
            noprup(2) = 'INST'
            typrup(2) = 'R'
        endif
        noprup(3) = 'NOEUD'
        typrup(3) = 'K8'
        noprup(4) = 'ABSC_CURV'
        typrup(4) = 'R'
        noprup(5) = 'G'
        typrup(5) = 'R'
        else if (option.eq.'CALC_K_G'.and.cas.eq.'2D'.and.&
     &         typfis.ne.'FISSURE'.and..not. lmoda) then
        nbprup = 6
        if (lmelas) then
            noprup(1) = 'NUME_CAS'
            typrup(1) = 'I'
            noprup(2) = 'NOM_CAS'
            typrup(2) = 'K16'
        else
            noprup(1) = 'NUME_ORDRE'
            typrup(1) = 'I'
            noprup(2) = 'INST'
            typrup(2) = 'R'
        endif
        noprup(3) = 'G'
        typrup(3) = 'R'
        noprup(4) = 'K1'
        typrup(4) = 'R'
        noprup(5) = 'K2'
        typrup(5) = 'R'
        noprup(6) = 'G_IRWIN'
        typrup(6) = 'R'
        else if(option.eq.'CALC_K_G'.and.cas.eq.'2D'.and.&
     &        typfis.eq.'FISSURE'.and..not. lmoda) then
        nbprup = 7
        noprup(1) = 'NUME_FOND'
        typrup(1) = 'I'
        if (lmelas) then
            noprup(2) = 'NUME_CAS'
            typrup(2) = 'I'
            noprup(3) = 'NOM_CAS'
            typrup(3) = 'K16'
        else
            noprup(2) = 'NUME_ORDRE'
            typrup(2) = 'I'
            noprup(3) = 'INST'
            typrup(3) = 'R'
        endif
        noprup(4) = 'G'
        typrup(4) = 'R'
        noprup(5) = 'K1'
        typrup(5) = 'R'
        noprup(6) = 'K2'
        typrup(6) = 'R'
        noprup(7) = 'G_IRWIN'
        typrup(7) = 'R'
        elseif((option.eq.'CALC_K_G'.or.option.eq.'CALC_K_MAX') .and.(&
    cas.eq.'3D_LOCAL').and.(.not.lmoda)) then
        nbprup = 11
        noprup(1) = 'NUME_FOND'
        typrup(1) = 'I'
        if (lmelas) then
            noprup(2) = 'NUME_CAS'
            typrup(2) = 'I'
            noprup(3) = 'NOM_CAS'
            typrup(3) = 'K16'
        else
            noprup(2) = 'NUME_ORDRE'
            typrup(2) = 'I'
            noprup(3) = 'INST'
            typrup(3) = 'R'
        endif
        noprup(4) = 'NUM_PT'
        typrup(4) = 'I'
        noprup(5) = 'ABSC_CURV'
        typrup(5) = 'R'
        noprup(6) = 'K1'
        typrup(6) = 'R'
        noprup(7) = 'K2'
        typrup(7) = 'R'
        noprup(8) = 'K3'
        typrup(8) = 'R'
        noprup(9) = 'G'
        typrup(9) = 'R'
        noprup(10) = 'BETA'
        typrup(10) = 'R'
        noprup(11) = 'G_IRWIN'
        typrup(11) = 'R'
    else if (option .eq. 'CALC_K_G' .and. lmoda) then
        if (cas .eq. '3D_LOCAL') then
            nbprup = 9
            noprup(1) = 'NUME_MODE'
            typrup(1) = 'I'
            noprup(2) = 'NUM_PT'
            typrup(2) = 'I'
            noprup(3) = 'ABSC_CURV'
            typrup(3) = 'R'
            noprup(4) = 'K1'
            typrup(4) = 'R'
            noprup(5) = 'K2'
            typrup(5) = 'R'
            noprup(6) = 'K3'
            typrup(6) = 'R'
            noprup(7) = 'G'
            typrup(7) = 'R'
            noprup(8) = 'BETA'
            typrup(8) = 'R'
            noprup(9) = 'G_IRWIN'
            typrup(9) = 'R'
        else
            nbprup = 5
            noprup(1) = 'NUME_MODE'
            typrup(1) = 'I'
            noprup(2) = 'G'
            typrup(2) = 'R'
            noprup(3) = 'K1'
            typrup(3) = 'R'
            noprup(4) = 'K2'
            typrup(4) = 'R'
            noprup(5) = 'G_IRWIN'
            typrup(5) = 'R'
        endif
    else if (option .eq. 'G_BILI' .or.option .eq. 'G_MAX') then
        nbprup = 6
        if (lmelas) then
            noprup(1) = 'NOM_CAS'
            typrup(1) = 'K16'
        else
            noprup(1) = 'INST'
            typrup(1) = 'R'
        endif
        noprup(2) = 'NUME_CMP_I'
        typrup(2) = 'I'
        noprup(3) = 'NUME_CMP_J'
        typrup(3) = 'I'
        noprup(4) = 'NOEUD'
        typrup(4) = 'K8'
        noprup(5) = 'ABSC_CURV'
        typrup(5) = 'R'
        noprup(6) = 'G_BILI_LOCAL'
        typrup(6) = 'R'
        elseif ( option .eq. 'G_BILI_GLOB' .or.option .eq. 'G_MAX_GLOB')&
    then
        nbprup = 3
        noprup(1) = 'NUME_CMP_I'
        typrup(1) = 'I'
        noprup(2) = 'NUME_CMP_J'
        typrup(2) = 'I'
        noprup(3) = 'G_BILIN'
        typrup(3) = 'R'
    endif
    call tbcrsd(table, 'G')
    call tbajpa(table, nbprup, noprup, typrup)
!
end subroutine
