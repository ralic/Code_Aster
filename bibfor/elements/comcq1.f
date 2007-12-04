      SUBROUTINE COMCQ1(FAMI,KPG,KSP,MOD,IMATE,
     &                  COMPOR,CARCRI,INSTM,INSTP,
     &                  EPS,DEPS,TEMPM,TEMPP,SIGM,VIM,
     &                  OPTION,ANGMAS,SIGP,VIP,DSDE,CODRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 16/10/2007   AUTEUR SALMONA L.SALMONA 
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
C TOLE CRP_21
C  VARIABLE ENTREE/SORTIE
      IMPLICIT NONE
      CHARACTER*(*)  FAMI
      CHARACTER*16   OPTION,COMPOR(*)
      INTEGER        CODRET,KPG,KSP,MOD,IMATE
      REAL*8         TEMPM,TEMPP,TREF,ANGMAS(3),SIGM(4),EPS(4),DEPS(4)
      REAL*8         VIM(*),VIP(*),SIGP(4),DSDE(6,6),CARCRI(*),LC(1)
      REAL*8         INSTM,INSTP,EP,EM,ALPHAP,ALPHAM,DEPSTH,ETG
      CHARACTER*8    TYPMOD(2),NOMPAR
      CHARACTER*2    CODRES


C  VARIABLE LOCALE
      INTEGER IRET
      CALL R8INIR(36,0.D0,DSDE,1)
      CALL R8INIR(4,0.D0,SIGP,1)
      
      CODRET=0
C     INTEGRATION DE LA LOI DE COMPORTEMENT POUR LES COQUE_1D :
C     COQUE_C_PLAN : COMPORTEMNT 1D
C     COQUE_AXIS ET COQUE_D_PLAN : COMPORTEMENT C_PLAN


      IF (MOD.GT.0) THEN
      
C     EN COQUE_D_PLAN ET COQUE_AXIS, DIRECTION Y : CPLAN (EVT DEBORST)
C     DIRECTION Z : EPSZZ CONNU

         TYPMOD(1) = 'C_PLAN  '
         TYPMOD(2) = '        '
         CALL NMCOMP(FAMI,KPG,KSP,2,TYPMOD,IMATE,
     &               COMPOR,CARCRI,INSTM,INSTP,
     &               EPS,DEPS,SIGM,VIM,OPTION,ANGMAS,LC,
     &               SIGP,VIP,DSDE,CODRET)
      ELSE
      
C         EN COQUE_C_PLAN : COMPORTEMENT 1D.

         IF ((COMPOR(5).EQ.'DEBORST').OR.(COMPOR(1).EQ.'SANS')) THEN
             TYPMOD(1) = 'COMP1D'
             TYPMOD(2) = '        '
             CALL NMCOMP(FAMI,KPG,KSP,2,TYPMOD,IMATE,
     &                   COMPOR,CARCRI,INSTM,INSTP,
     &                   EPS,DEPS,SIGM,VIM,OPTION,ANGMAS,LC,
     &                   SIGP,VIP,DSDE,CODRET)

         ELSE

C            COMPORTEMENT INTEGRE EN 1D
         
C            CARACTERISTIQUES ELASTIQUES A TMOINS
             NOMPAR = 'TEMP'
             CALL RCVALB('RIGI',KPG,1,'-',IMATE,' ','ELAS',1,NOMPAR,
     &            TEMPM,1,'E',EM,CODRES,'FM')

C ---        CARACTERISTIQUES ELASTIQUES A TPLUS 
             CALL RCVALB('RIGI',KPG,1,'+',IMATE,' ','ELAS',1,NOMPAR,
     &            TEMPP,1,'E',EP,CODRES,'FM')
             IF (COMPOR(1).EQ.'ELAS') THEN
                CALL VERIFG('RIGI',KPG,3,'T',IMATE,'ELAS',1,DEPSTH,IRET)
                SIGP(1) = EP* (SIGM(1)/EM+DEPS(1)-DEPSTH)
                VIP(1) = 0.D0
                DSDE(1,1) = EP
                DSDE(2,2) = EP
             ELSE IF ((COMPOR(1).EQ.'VMIS_ISOT_LINE') .OR.
     &                (COMPOR(1).EQ.'VMIS_ISOT_TRAC')) THEN
                CALL NM1DIS(FAMI,KPG,KSP,IMATE,EM,EP,
     &               SIGM,DEPS,VIM,OPTION,COMPOR,' ',SIGP(1),VIP,ETG)
                DSDE(1,1) = ETG+1.D-6*EP
C               DSDE(1,1) = ETG
                DSDE(2,2) = EP
                SIGP(2)=0.D0
             ELSE IF (COMPOR(1).EQ.'VMIS_CINE_LINE') THEN
                CALL NM1DCI(FAMI,KPG,KSP,IMATE,EM,EP,
     &                  SIGM,DEPS,VIM,OPTION,' ',SIGP(1),VIP,EP)
                DSDE(1,1) = ETG
                DSDE(2,2) = EP
             ELSE
               CALL U2MESK('F','ALGORITH6_81',1,COMPOR)
             ENDIF

          ENDIF

      ENDIF

      END
