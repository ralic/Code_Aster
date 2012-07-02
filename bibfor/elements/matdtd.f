      SUBROUTINE MATDTD(NOMTE,TESTL1,TESTL2,DSIDEP,CISAIL,X3,COUR,R,
     &                                                COSA,KAPPA,DTILDI)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
      IMPLICIT NONE
C
      CHARACTER*16 NOMTE
      REAL*8 CISAIL,X3,COUR,R,COSA,RHOS,RHOT,KAPPA
      REAL*8 DSIDEP(6,6),MATA(3,3),DTILDI(5,5)
      LOGICAL TESTL1,TESTL2
C
C-----------------------------------------------------------------------
      REAL*8 RHOS2 ,RHOST ,RHOT2 ,X32 
C-----------------------------------------------------------------------
      CALL R8INIR(25,0.D0,DTILDI,1)
C
C     CALCULS DE LA MATRICE DTILDI
C
C
C   EXTRACTION DE DSIDEP LA SOUS MATRICE MATA CONCERNEE
C   ON NOTE QUE LA MATRICE DSIDEP OBTENUE EST DE TYPE CONTRAINTES PLANES
C   LES COMPOSANTES CORRESPONDANT AU TERME DE CISAILLEMENT SONT REMPLIES
C   "ARTIFICIELLEMENT"
C
      IF ( NOMTE .EQ. 'MECXSE3' ) THEN
C
        MATA(1,1)=DSIDEP(1,1)
        MATA(1,2)=DSIDEP(1,2)
        MATA(1,3)=0.D0
        MATA(2,1)=DSIDEP(2,1)
        MATA(2,2)=DSIDEP(2,2)
        MATA(2,3)=0.D0
        MATA(3,1)=0.D0
        MATA(3,2)=0.D0
        MATA(3,3)=CISAIL*KAPPA/2.D0
C
       IF (TESTL1) THEN
          RHOS=1.D0
       ELSE
          RHOS=1.D0 + X3 * COUR
       ENDIF
       IF (TESTL2) THEN
          RHOT=1.D0
       ELSE
          RHOT=1.D0 + X3 * COSA / R
       ENDIF
C
           X32  =X3*X3
           RHOS2=RHOS*RHOS
           RHOT2=RHOT*RHOT
           RHOST=RHOS*RHOT
C
        DTILDI(1,1)= MATA(1,1)   /RHOS2
        DTILDI(1,2)= MATA(1,1)*X3/RHOS2
        DTILDI(1,3)= MATA(1,2)   /RHOST
        DTILDI(1,4)= MATA(1,2)*X3/RHOST
        DTILDI(1,5)= MATA(1,3)   /RHOS2
C
        DTILDI(2,2)= MATA(1,1)*X32/RHOS2
        DTILDI(2,3)= MATA(1,2)*X3 /RHOST
        DTILDI(2,4)= MATA(1,2)*X32/RHOST
        DTILDI(2,5)= MATA(1,3)*X3 /RHOS2
C
        DTILDI(3,3)= MATA(2,2)    /RHOT2
        DTILDI(3,4)= MATA(2,2)*X3 /RHOT2
        DTILDI(3,5)= MATA(2,3)    /RHOST
C
        DTILDI(4,4)= MATA(2,2)*X32/RHOT2
        DTILDI(4,5)= MATA(2,3)*X3 /RHOST
C
        DTILDI(5,5)= MATA(3,3)    /RHOS2
C
        DTILDI(2,1)=DTILDI(1,2)
        DTILDI(3,1)=DTILDI(1,3)
        DTILDI(3,2)=DTILDI(2,3)
        DTILDI(4,1)=DTILDI(1,4)
        DTILDI(4,2)=DTILDI(2,4)
        DTILDI(4,3)=DTILDI(3,4)
        DTILDI(5,1)=DTILDI(1,5)
        DTILDI(5,2)=DTILDI(2,5)
        DTILDI(5,3)=DTILDI(3,5)
        DTILDI(5,4)=DTILDI(4,5)
C
      ELSE
        IF ( NOMTE .EQ. 'METDSE3' ) THEN
           MATA(1,1)=DSIDEP(1,1)
           MATA(1,2)=0.D0
           MATA(2,1)=0.D0
           MATA(2,2)=CISAIL*KAPPA/2.D0
        ELSE IF ( NOMTE .EQ. 'METCSE3' ) THEN
           MATA(1,1)=DSIDEP(1,1) - 
     &              (DSIDEP(1,2)*DSIDEP(2,1))/DSIDEP(2,2)
           MATA(1,2)=0.D0
           MATA(2,1)=0.D0
           MATA(2,2)=CISAIL*KAPPA/2.D0
        ENDIF
C
        IF ( TESTL1 ) THEN
           RHOS=1.D0
        ELSE
           RHOS=1.D0 + X3 * COUR
        ENDIF
C
           X32  =X3*X3
           RHOS2=RHOS*RHOS
C
        DTILDI(1,1)= MATA(1,1)/RHOS2
        DTILDI(1,2)= MATA(1,1)*X3/RHOS2
        DTILDI(1,3)= MATA(1,2)/RHOS2
        DTILDI(2,2)= MATA(1,1)*X32/RHOS2
        DTILDI(2,3)= MATA(1,2)*X3/RHOS2
        DTILDI(3,3)= MATA(2,2)/RHOS2
C
        DTILDI(2,1)=DTILDI(1,2)
        DTILDI(3,1)=DTILDI(1,3)
        DTILDI(3,2)=DTILDI(2,3)
      ENDIF
C
      END
