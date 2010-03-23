       SUBROUTINE NVITHM(COMPOR, MECA, THMC, THER, HYDR, NVIM, NVIT,
     +                   NVIH, NVIC, ADVIME, ADVITH, ADVIHY, ADVICO,
     +                   VIHRHO, VICPHI, VICPVP, VICSAT,VICPR1,VICPR2)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 23/03/2010   AUTEUR ANGELINI O.ANGELINI 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY  
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY  
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR     
C (AT YOUR OPTION) ANY LATER VERSION.                                   
C                                                                       
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT   
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF            
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU      
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                              
C                                                                       
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE     
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,         
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C ======================================================================
C --- DEFINITION DES ADRESSES DE STOCKAGES DES VARIABLES INTERNES ------
C --- POUR LA THM, RECUPERATION DES RELATIONS DE COMPORTEMENTS ET DU ---
C --- NOMBRE DE VARIABLES INTERNES ASSOCIEES A CHAQUE RELATION ---------
C --- ON FAIT LE CHOIX DE STOCKES LES VARIABLES INTERNES DANS L'ORDRE --
C --- SUIVANT : MECANIQUE - THERMIQUE - HYDRAULIQUE - COUPLAGE ---------
C ======================================================================
      IMPLICIT NONE
      INTEGER       NVIM, NVIT, NVIH, NVIC
      INTEGER       ADVIME, ADVITH, ADVIHY, ADVICO
      INTEGER       VIHRHO, VICPHI, VICPVP, VICSAT,VICPR1,VICPR2
      CHARACTER*16  COMPOR(*), MECA, THMC, THER, HYDR
C ======================================================================
      INTEGER       NBCOMP
C ======================================================================
C --- NBCOMP EST LE NOMBRE DE VARIABLES DANS LA CARTE COMPOR DE --------
C --- GRANDEUR_SIMPLE AVANT LA DEFINITION DU NOMBRE DE VARIABLES -------
C --- INTERNES ASSOCIEES AUX RELATIONS DE COMPORTEMENT POUR LA THM -----
C ======================================================================
      PARAMETER ( NBCOMP = 7 + 9 )
C ======================================================================
C --- RECUPERATION DES DIFFERENTES RELATIONS DE COMPORTEMENT -----------
C ======================================================================
C --- THMC EST LA LOI DE COMPORTEMENT DE COUPLAGE ----------------------
C --- THER EST LA LOI DE COMPORTEMENT THERMIQUE ------------------------
C --- HYDR EST LA LOI DE COMPORTEMENT HYDRAULIQUE ----------------------
C --- MECA EST LA LOI DE COMPORTEMENT MECANIQUE ------------------------
C ======================================================================
      THMC   = COMPOR( 8)
      THER   = COMPOR( 9)
      HYDR   = COMPOR(10)
      MECA   = COMPOR(11)
C ======================================================================
C --- RECUPERATION DU NOMBRE DE VARIABLES INTERNES ASSOCIEES A CHAQUE --
C --- RELATIONS DE COMPORTEMENT ----------------------------------------
C ======================================================================
C --- NVIC EST LE NOMBRE DE VARIABLES INTERNES POUR LA LOI DE COUPLAGE -
C --- NVIT EST LE NOMBRE DE VARIABLES INTERNES POUR LA LOI THERMIQUE ---
C --- NVIH EST LE NOMBRE DE VARIABLES INTERNES POUR LA LOI HYDRAULIQUE -
C --- NVIM EST LE NOMBRE DE VARIABLES INTERNES POUR LA LOI MECANIQUE ---
C ======================================================================
      READ (COMPOR(NBCOMP+1),'(I16)') NVIC
      READ (COMPOR(NBCOMP+2),'(I16)') NVIT
      READ (COMPOR(NBCOMP+3),'(I16)') NVIH
      READ (COMPOR(NBCOMP+4),'(I16)') NVIM
C ======================================================================
C --- AFFECTATION D'UNE ADRESSE DE STOCKAGE POUR LES VARIABLES ---------
C --- INTERNES SUIVANT LA RELATION DE COMPORTEMENT ---------------------
C ======================================================================
C --- ADVIME EST L'ADRESSE DE STOCKAGE DES VAR. INT. MECANIQUE ---------
C --- ADVITH EST L'ADRESSE DE STOCKAGE DES VAR. INT. THERMIQUE ---------
C --- ADVIHY EST L'ADRESSE DE STOCKAGE DES VAR. INT. HYDRAULIQUE -------
C --- ADVICO EST L'ADRESSE DE STOCKAGE DES VAR. INT. DE COUPLAGE -------
C ======================================================================
      ADVIME = 1
      ADVITH = ADVIME + NVIM
      ADVIHY = ADVITH + NVIT
      ADVICO = ADVIHY + NVIH
C ======================================================================
C --- ORDRE DE STOCKAGE POUR LES VARIABLES INTERNES SUIVANT LES --------
C --- RELATIONS DE COMPORTEMENT : --------------------------------------
C --- POUR LA MECANIQUE  : L'ORDRE DE STOCKAGE EST DEFINI SELON LA LOI -
C --- POUR LA THERMIQUE  : PAS DE VARIABLES INTERNES ACTUELLEMENT ------
C --- POUR L'HYDRAULIQUE : VAR. INT. 1 : RHO_LIQUIDE - RHO_0 -----------
C --- POUR LE COUPLAGE   : VAR. INT. 1 : PHI - PHI_0 -------------------
C ---                    : VAR. INT. 2 : PVP - PVP_0 SI VAPEUR ---------
C ---                    : VAR. INT. 3 : SATURATION SI LOI NON SATUREE -
C         EN CAS DE LIQU_AD_GAZ VARINT2 VAUT TOUJOURS ZERO
C ======================================================================
C --- HYDRAULIQUE ------------------------------------------------------
C ======================================================================
      VIHRHO = 0
C ======================================================================
C --- COUPLAGE ---------------------------------------------------------
C ======================================================================
      VICPHI = 0
      IF ( ( THMC .EQ. 'LIQU_GAZ' )        .OR.
     +     ( THMC .EQ. 'LIQU_GAZ_ATM' )    .OR.
     +     ( THMC .EQ. 'LIQU_VAPE')        .OR.
     +     ( THMC .EQ. 'LIQU_VAPE_GAZ')    .OR.
     +     ( THMC .EQ. 'LIQU_AD_GAZ')      .OR.
     +     ( THMC .EQ. 'LIQU_AD_GAZ_VAPE') ) THEN
         IF ( ( THMC .EQ. 'LIQU_GAZ' )        .OR.
     +        ( THMC .EQ. 'LIQU_GAZ_ATM' ) )  THEN
            VICSAT = VICPHI + 1
         ELSE
            VICPVP = VICPHI + 1
            VICSAT = VICPVP + 1
         ENDIF
         IF(THMC .EQ. 'LIQU_AD_GAZ') THEN
          VICPR1=VICSAT+1
          VICPR2=VICPR1+1
         ENDIF
      ENDIF
C ======================================================================
      END
