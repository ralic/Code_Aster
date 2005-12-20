        SUBROUTINE DSIPDP(THMC,ADCOME,ADDEP1,ADDEP2,
     1                     DIMCON,DIMDEF,DSDE,DSPDP1,DSPDP2,PRE2TR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/12/2005   AUTEUR JOUMANA J.EL-GHARIB 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C --- LOI DE COMPORTEMENT DE TYPE HOEK BROWN EN CONTRAINTES TOTALES ----
C --- ELASTICITE ISOTROPE ----------------------------------------------
C --- CRITERE DE PLASTICITE DE HEOK BROWN ------------------------------
C --- ECOULEMENT PLASTIQUE DE DRUCKER PRAGER ---------------------------
C ======================================================================
C OUT DSPDP1  DERIVEE DE SIP  PAR RAPPORT A PRE1
C OUT DSPDP2  DERIVEE DE SIP  PAR RAPPORT A PRE2
C ======================================================================
      IMPLICIT      NONE
      LOGICAL       PRE2TR
      INTEGER       ADCOME,ADDEP1, ADDEP2, DIMCON, DIMDEF
      REAL*8        DSPDP1,DSPDP2,DSDE(DIMCON,DIMDEF)
      CHARACTER*16  THMC
C ======================================================================
C    CETTE ROUTINE CALCULE LES DERIVEES DE SIGMAP PAR RAPPORT
C    A P1 ET A P2 SELON LE CAS DE LA LOI DE COUPLAGE HYDRAULIQUE
C
C    INITIALISATIONS
        DSPDP1 = 0.0D0
        DSPDP2 = 0.0D0 
        PRE2TR = .FALSE.
C ======================================================================
C --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_SATU ----------------------
C ======================================================================
      IF (THMC.EQ.'LIQU_SATU') THEN
          DSPDP1 = DSPDP1+DSDE(ADCOME+6,ADDEP1)
          DSPDP2 = 0.D0
C ======================================================================
C --- CAS D'UNE LOI DE COUPLAGE DE TYPE GAZ ----------------------------
C ======================================================================
      ELSE IF (THMC.EQ.'GAZ') THEN
          DSPDP1 = 0.D0
          DSPDP2 = DSPDP2+DSDE(ADCOME+6,ADDEP1)
          PRE2TR = .TRUE.
C ======================================================================
C --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_VAPE ----------------------
C ======================================================================
      ELSE IF (THMC.EQ.'LIQU_VAPE') THEN
          DSPDP1 = DSPDP1+DSDE(ADCOME+6,ADDEP1)
          DSPDP2 = 0.D0
C ======================================================================
C --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_VAPE_GAZ ------------------
C ======================================================================
      ELSE IF (THMC.EQ.'LIQU_VAPE_GAZ') THEN
          DSPDP1 = DSPDP1+DSDE(ADCOME+6,ADDEP1)
          DSPDP2 = DSPDP2+DSDE(ADCOME+6,ADDEP2)
          PRE2TR = .TRUE.
C ======================================================================
C --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_GAZ -----------------------
C ======================================================================
      ELSE IF (THMC.EQ.'LIQU_GAZ') THEN
          DSPDP1 = DSPDP1+DSDE(ADCOME+6,ADDEP1)
          DSPDP2 = DSPDP2+DSDE(ADCOME+6,ADDEP2)
          PRE2TR = .TRUE.
C ======================================================================
C --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_GAZ_ATM -------------------
C ======================================================================
      ELSE IF (THMC.EQ.'LIQU_GAZ_ATM') THEN
          DSPDP1 = DSPDP1+DSDE(ADCOME+6,ADDEP1)
          DSPDP2 = 0.0D0
C ======================================================================
C --- CAS D'UNE LOI DE COUPLAGE DE TYPE LIQU_AD_GAZ_VAPE ---------------
C ======================================================================
      ELSE IF (THMC.EQ.'LIQU_AD_GAZ_VAPE') THEN
          DSPDP1 = DSPDP1+DSDE(ADCOME+6,ADDEP1)
          DSPDP2 = DSPDP2+DSDE(ADCOME+6,ADDEP2)
          PRE2TR = .TRUE.
C ======================================================================
      ENDIF
C ======================================================================
      END
