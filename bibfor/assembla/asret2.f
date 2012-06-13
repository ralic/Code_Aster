      SUBROUTINE ASRET2(LMASYM,JTMP2,LGTMP2,NBTERM,JSMHC,JSMDI,NBI1,TI1,
     &                  TI2)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      LOGICAL LMASYM
      INTEGER JTMP2,LGTMP2,NBTERM,JSMHC,JSMDI
      INTEGER IDEB,IFIN,IMIL
      INTEGER NBI1,TI1(NBI1),TI2(NBI1)
C -----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ASSEMBLA  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C TOLE CRP_4
C     ROUTINE SERVANT A RETENIR OU S'ACCUMULENT LES TERMES ELEMENTAIRES:
C     DANS LE CAS D'UN STOCKAGE MORSE SYMETRIQUE
C -----------------------------------------------------------------
C IN/OUT I JTMP2   : ADRESSE JEVEUX DE L'OBJET ".TMP2"
C IN    I4 JSMHC   : ADRESSE DE ".SMHC".
C IN     I JSMDI   : ADRESSE DE ".SMDI".
C IN     I NBI1    : NOMBRES DE TERMES DANS TI1 (ET TI2)
C IN     I TI1(*),TI2(*)   : NUMEROS GLOBAUX (LIGNE ET COLONNE)
C IN/OUT I NBTERM   : NOMBRE DE TERMES (R/C) A RECOPIER
C                     (ISSU DE LA MATRICE ELEMENTAIRE)
C -----------------------------------------------------------------
      INTEGER ILI,JCO,ICOEFC,ICOEFL,I,NCOEFC,NUBLOC,K,I1,I2
C -----------------------------------------------------------------
      DO 50,K=1,NBI1
        I1=TI1(K)
        I2=TI2(K)

        IF (I1.LE.I2) THEN
          ILI=I1
          JCO=I2
          NUBLOC=1
        ELSE
          ILI=I2
          JCO=I1
          NUBLOC=2
        ENDIF
        IF (LMASYM)NUBLOC=1

        IF (JCO.EQ.1) THEN
          ICOEFC=0
        ELSE
          ICOEFC=ZI(JSMDI+JCO-2)
        ENDIF
        NCOEFC=ZI(JSMDI+JCO-1)-ICOEFC


C     -- CALCUL DE ICOEFL :
C     ------------------------------------------
        ICOEFL=0
        IF (.FALSE.) THEN
C         -- RECHERCHE BESTIALE :
          DO 10 I=1,NCOEFC
            IF (ZI4(JSMHC-1+ICOEFC+I).EQ.ILI) THEN
              ICOEFL=I
              GOTO 40

            ENDIF
   10     CONTINUE

        ELSE
C          -- RECHERCHE PAR DICHOTOMIE :
          IDEB=1
          IFIN=NCOEFC
   20     CONTINUE
          IF (IFIN-IDEB.LT.5) THEN
            DO 30 I=IDEB,IFIN
              IF (ZI4(JSMHC-1+ICOEFC+I).EQ.ILI) THEN
                ICOEFL=I
                GOTO 40

              ENDIF
   30       CONTINUE
          ENDIF
          IMIL=(IDEB+IFIN)/2
          IF (ZI4(JSMHC-1+ICOEFC+IMIL).GT.ILI) THEN
            IFIN=IMIL
          ELSE
            IDEB=IMIL
          ENDIF
          GOTO 20

        ENDIF
        IF (ICOEFL.EQ.0) CALL U2MESS('F','MODELISA_67')


   40   CONTINUE

C     -- NBTERM COMPTE LES REELS TRAITES:
        NBTERM=NBTERM+1
        IF (2*NBTERM.GT.LGTMP2) THEN
          LGTMP2=2*LGTMP2
          CALL JUVECA('&&ASSMAM.TMP2',LGTMP2)
C         -- IL NE FAUT PAS QUE .TMP2 SOIT LIBERE :
          CALL JEVEUT('&&ASSMAM.TMP2','E',JTMP2)
        ENDIF
        ZI(JTMP2-1+(NBTERM-1)*2+1)=NUBLOC
        ZI(JTMP2-1+(NBTERM-1)*2+2)=ICOEFC+ICOEFL
   50 CONTINUE
      END
