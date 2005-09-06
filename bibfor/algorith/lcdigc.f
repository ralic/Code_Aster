      SUBROUTINE LCDIGC(X,Y,Z,DX,DY,DZ,V1M,V2M,V3M,KN,KT,MU,FNO,
     &                   ECRO,V1P,V2P,V3P,CONT,F,DSIDEP)

      IMPLICIT NONE
      REAL*8  X,Y,Z,V1M,V2M,V3M,KN,KT,MU,FNO,ECRO,DX,DY,DZ
      REAL*8  V1P,V2P,V3P,CONT,F(3),DSIDEP(3,3)

C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/09/2005   AUTEUR GODARD V.GODARD 
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
C
C RESPONSABLE GODARD V.GODARD
C
C ======================================================================
C  COMPORTEMENT DIS_GRICRA 
C------------------------------------------------------------------
C------------------------------------------------------------------
C-- Loi de comportement des 'sous-éléments' H1,B1,R1
C-- pour la liaison grille-crayon DIS_GRICRA
C-- dans le repère local de l'élément 
C--
C-- Pour chaque sous-élément, on a
C    -une relation élastique
C     + perte de contact possible dans la direction du sous élément:
C    -une loi élastoplastique dans le plan orthogonal pour représenter
C       le contact avec frottement
C       H1, B1, R1: plan XY 
C--      
C-- l'extension , dans la direction du sous-élément, 
C   est positive pour les bossettes H1, B1 
C   et négative pour le ressort R1 
C
C IN  : X ,Y, Z      : extension au pas de temps précédent
C       DX, DY, DZ   : Incrément d'extension
C       V1M, V2M,V3M : variables internes au pas de temps précédent
C       KN           : rigidité normale du sous-élément
C       KT           : rigidité tangentielle du sous-élément
C       MU           : coefficient de frottement de coulomb
C       FN0          : force de serrage initiale
C                      (FNO/2 pour les bossettes)
C       ECRO         : paramètre d'écrouissage
C
C OUT : V1P, V2P,V3P : variables internes réactualisées
C       CONT         : indicateur de contact (0 ou 1)
C       F            : contrainte dans le sous-élément
C------------------------------------------------------------------
C------------------------------------------------------------------

      LOGICAL    ELAS
      INTEGER    I,J,K
      REAL*8     CRIT,DV1,DV2,DV3
      REAL*8     FTM(2),FTP(2),NOFTM,NOFTP,MATA(2,2),MATB(2,2)
      REAL*8     DETA,INVA(2,2),DFTDUT(2,2),DFTDUE(2,2),INTE(2,2)
      REAL*8     KRON(2,2),INTR(2,2),MATD(2,2),INVD(2,2),DETD
      REAL*8     DFTDU(2,2),FN,FT(2),FM(2)


      CALL R8INIR(3,0.D0,F,1)
      CONT=0.D0


C---ACTUALISATION DES VARIABLES INTERNES ET CALCUL DES FORCES
C
      ELAS=.TRUE.
C
      FN = -FNO+KN*(X+DX)
C
      IF (FN.GT.0.D0) THEN
         F(1)=0.D0   
         F(2)=0.D0   
         F(3)=0.D0
         V1P=V1M      
         V2P=V2M      
         V3P=V3M      
      ELSE
         FT(1)= KT*(Y+DY-V1M)
         FT(2)= KT*(Z+DZ-V2M)
         CONT=1.D0

         CRIT=SQRT(FT(1)**2.D0+FT(2)**2.D0)+MU*FN-ECRO*V3M

         IF (CRIT.LE.0.D0) THEN
           V1P=V1M       
           V2P=V2M       
           V3P=V3M       
         ELSE
           ELAS=.FALSE.
C
           FM(1)= KT*(Y-V1M)
           FM(2)= KT*(Z-V2M)
C
           DV3=(MU*FN-ECRO*V3M+
     &        SQRT((FM(1)+KT*DY)**2.D0+(FM(2)+KT*DZ)**2.D0))
     &        /(KT+ECRO)
           V3P=V3M+DV3
C
           FT(1)=(-MU*FN+V3P*ECRO)*(FM(1)+KT*DY)
     &        /(-MU*FN+V3P*ECRO+DV3*KT)
C
           FT(2)=(-MU*FN+V3P*ECRO)*(FM(2)+KT*DZ)
     &        /(-MU*FN+V3P*ECRO+DV3*KT)
C
           DV1=DV3*FT(1)/SQRT(FT(1)**2.D0+FT(2)**2.D0)
           DV2=DV3*FT(2)/SQRT(FT(1)**2.D0+FT(2)**2.D0)
           V1P=V1M+DV1
           V2P=V2M+DV2
         ENDIF
           F(1)=FN
           F(2)=FT(1)
           F(3)=FT(2)
      ENDIF
C     
C
C---CALCUL DE LA MATRICE TANGENTE DU SOUS ELEMENT-

      CALL R8INIR(9,0.D0,DSIDEP,1)
      IF (FN.GT.0.D0) THEN
        GOTO 999
      ENDIF
      IF (ELAS) THEN
        DSIDEP(1,1)=KN
        DSIDEP(2,2)=KT
        DSIDEP(3,3)=KT
      ELSE
        CALL R8INIR(4,0.D0,KRON,1)
        KRON(1,1)=1.D0
        KRON(2,2)=1.D0
        FTM(1)= KT*(Y-V1M)+KT*DY
        FTM(2)= KT*(Z-V2M)+KT*DZ
        FTP(1)= KT*(Y+DY-V1P)
        FTP(2)= KT*(Z+DZ-V2P)
        NOFTM=SQRT(FTM(1)**2.D0+FTM(2)**2.D0)
        NOFTP=SQRT(FTP(1)**2.D0+FTP(2)**2.D0)
C
        CALL R8INIR(4,0.D0,MATA,1)
        CALL R8INIR(4,0.D0,MATB,1)
C
        DO 44 I=1,2
          DO 45 K=1,2
            MATA(I,K)=MATA(I,K)+KRON(I,K)+KT*DV3*
     &                  (KRON(I,K)/NOFTP-FTP(I)*FTP(K)/NOFTP**3.D0)
            MATB(I,K)=MATB(I,K)+KT*KRON(I,K)
     &                  -KT*FTP(I)/NOFTP*KT/(KT+ECRO)*FTM(K)/NOFTM
 45       CONTINUE
 44     CONTINUE
C
        CALL R8INIR(4,0.D0,INVA,1)
        DETA=MATA(1,1)*MATA(2,2)-MATA(1,2)*MATA(2,1)
        IF (DETA.EQ.0.D0) THEN
        CALL UTMESS('F','LCDGIC','MATRICE A NON INVERSIBLE')
        ENDIF
        INVA(1,1)=MATA(2,2)/DETA
        INVA(2,2)=MATA(1,1)/DETA
        INVA(1,2)=-MATA(1,2)/DETA
        INVA(2,1)=-MATA(2,1)/DETA
C
        CALL PMAT(2,INVA,MATB,DFTDU)
        DSIDEP(2,2)=DFTDU(1,1)
        DSIDEP(3,3)=DFTDU(2,2)
        DSIDEP(2,3)=DFTDU(1,2)
        DSIDEP(3,2)=DFTDU(2,1)
        DSIDEP(1,1)=KN
        DSIDEP(2,1)=-MU*KT*KN/(ECRO+KT)*FTP(1)/NOFTP
        DSIDEP(3,1)=-MU*KT*KN/(ECRO+KT)*FTP(2)/NOFTP
C
      ENDIF
C
 999    CONTINUE
C
      END
