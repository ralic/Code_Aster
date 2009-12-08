      SUBROUTINE DIL2GR(IMATE,COMPOR,NDIM,REGULA,DIMDEF,DEFGEP,SIGP,
     +                  DSDE2G)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 21/05/2007   AUTEUR FERNANDES R.FERNANDES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C --- BUT : CALCUL DE LA LOI DE COMPORTEMENT ELASTIQUE POUR LA PARTIE --
C ---       SECOND GRADIENT --------------------------------------------
C ======================================================================
      IMPLICIT      NONE
      INTEGER       IMATE,NDIM,DIMDEF,REGULA(6)
      REAL*8        SIGP(NDIM),DSDE2G(NDIM,NDIM),DEFGEP(DIMDEF)
      CHARACTER*16  COMPOR(*)
C ======================================================================
C --- VARIABLES LOCALES ------------------------------------------------
C ======================================================================
      INTEGER       I,J,ADDER2
      REAL*8        VAL(5)
      CHARACTER*2   CODRET(5)
      CHARACTER*8   NCRA(5)
C ======================================================================
C --- DEFINITION DES DONNEES INITIALES ---------------------------------
C ======================================================================
      DATA NCRA  / 'A1','A2','A3','A4','A5' /
      IF (COMPOR(1).EQ.'ELAS') THEN
         DO 10 I=1,NDIM
            DO 20 J=1,NDIM
               DSDE2G(J,I)=0.0D0
 20         CONTINUE
 10      CONTINUE
         CALL RCVALA(IMATE,' ', 'ELAS_2NDG', 0, ' ', 0.0D0,
     +                                 1, NCRA(1),VAL(1),CODRET(1),'FM')
         CALL RCVALA(IMATE,' ', 'ELAS_2NDG', 0, ' ', 0.0D0,
     +                                 1, NCRA(3),VAL(3),CODRET(3),'FM')
         DO 30 I=1,NDIM
            DSDE2G(I,I)=(1+NDIM)*(VAL(1)-VAL(3))
 30      CONTINUE

         ADDER2 = REGULA(2)
         DO 40 I=1,NDIM
            SIGP(I)=0.0D0
            DO 50 J=1,NDIM
               SIGP(I)=SIGP(I)+DSDE2G(I,J)*DEFGEP(ADDER2-1+J)
 50         CONTINUE
 40      CONTINUE
      ELSE
         CALL U2MESK('F','ALGORITH4_50',1,COMPOR(1))
      ENDIF
C ======================================================================
      END
