        SUBROUTINE IRRMAT ( FAMI,KPG,KSP,MOD,IMAT,NMAT,ITMAX,RELA,VIND,
     &                 TEMPD,TEMPF,MATERD,MATERF,MATCST,NDT,NDI,NR,NVI)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/02/2006   AUTEUR CIBHHPD L.SALMONA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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

       IMPLICIT NONE
       CHARACTER*8 MOD
       CHARACTER*3 MATCST
       CHARACTER*(*) FAMI
       INTEGER IMAT,NMAT,NDT,NDI,NR,NVI,KPG,KSP,IRET,ITMAX
       REAL*8  TEMPD,TEMPF,MATERD(NMAT,2),MATERF(NMAT,2),NC,FC,RELA
       REAL*8  VIND(*)

C       ----------------------------------------------------------------
C       IRRAD3M   : RECUPERATION DU MATERIAU A T(TEMPD) ET T+DT(TEMPF)
C                    NB DE CMP DIRECTES/CISAILLEMENT , NB VAR. INTERNES
C                  MATER(*,1) = E , NU , ALPHA
C                  MATER(*,2) = R02 EPSILON_U RM AI0 ETAI_S R ALPHA PHI0
C       ----------------------------------------------------------------
C       IN  FAMI   :  FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
C           KPG,KSP:  NUMERO DU (SOUS)POINT DE GAUSS
C           IMAT   :  ADRESSE DU MATERIAU CODE
C           MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION  DE MATER
C           ITMAX  :  NOMBRE D ITERATION MAX
C           RELA   :  TOLERANCE RELATIVE DES VALEURTS MATERIAUX
C           VIND   :  VARIABLES INTERNES A T
C           TEMPD  :  TEMPERATURE  A T
C           TEMPF  :  TEMPERATURE  A T+DT
C       OUT MATERD :  COEFFICIENTS MATERIAU A T
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C                     MATER(*,1) = CARACTERISTIQUES   ELASTIQUES
C                     MATER(*,2) = CARACTERISTIQUES   AUTRE
C           MATCST :  'OUI' SI  MATERIAU A T = MATERIAU A T+DT
C                     'NON' SINON
C           NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
C           NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
C           NR     :  NB DE COMPOSANTES SYSTEME NL
C           NVI    :  NB DE VARIABLES INTERNES
C       ----------------------------------------------------------------
       INTEGER     I,ITERAT
       CHARACTER*2 CERR(12)
       CHARACTER*8 NOMC(12)
       REAL*8      MAT(9),R,P0,N,PAS,IRRAD,IRRAF,P,PE,K,A,B
       DATA PE        /2.D-3/
C -     NB DE COMPOSANTES / VARIABLES INTERNES -------------------------
C
       CALL IRRNVI ( MOD , NDT , NDI, NR )
       
       NVI = 5
       P=VIND(1)
C =================================================================
C --- DEFINITION DES CHAMPS ---------------------------------------
C =================================================================

       NOMC(1)  = 'E        '
       NOMC(2)  = 'NU       '
       NOMC(3)  = 'ALPHA    '
       NOMC(4)  = 'R02      '
       NOMC(5)  = 'EPSILON_U'
       NOMC(6)  = 'RM       '
       NOMC(7)  = 'AI0      '
       NOMC(8)  = 'ETAI_S   '
       NOMC(9)  = 'R        '
       NOMC(10) = 'ALPHA    '
       NOMC(11) = 'PHI0     '
       NOMC(12) = 'KAPPA    '
       CALL RCVARC('F','IRRA','-',FAMI,KPG,KSP,IRRAD,IRET)
        
       CALL RCVALB(FAMI,KPG,KSP,'-',IMAT,' ','ELAS',1,'TEMP',TEMPD,
     +               3,NOMC,MATERD(1,1),CERR(1), 'FM' )

       CALL RCVALB(FAMI,KPG,KSP,'-',IMAT,' ','IRRAD3M',1,'TEMP',TEMPD,
     +               9,NOMC(4),MAT,CERR(3), 'FM' )

       
C     RECUPERATION DES DONNEES DE LA LOI SEUIL_PLASTIQUE = K*(P+P0)**N
C     METHODE DE DICHOTOMIE POUR CALCULER LES VALEURS A L INSTANT -
      NC=MAT(2)
      IF ( NC.NE.0.D0) THEN
        FC=1-((MAT(3)/MAT(1))*EXP(MAT(2))*(PE+NC-MAT(2))**NC)/
     &     (NC**NC)
      ELSE
        FC=1-(MAT(3)/MAT(1))*EXP(MAT(2))
      ENDIF

      PAS=MAT(2)/2.D0

      ITERAT=0

 1    CONTINUE
      
      ITERAT=ITERAT+1

C     CONVERGENCE ?
      IF (ABS(FC).LE.RELA) THEN
C       VALEUR DE N
        MATERD(2,2)=NC
C       VALEUR DE K
        IF ( MATERD(2,2).NE.0.D0) THEN
          MATERD(1,2)=MAT(3)*EXP(MAT(2))/(MATERD(2,2)**MATERD(2,2))
        ELSE
          MATERD(1,2)=MAT(3)*EXP(MAT(2))
        ENDIF
C       VALEUR DE P0
        MATERD(3,2)=MATERD(2,2)-MAT(2)
      ELSE
        IF (ITERAT.LT.ITMAX) THEN
          PAS=PAS/2.D0
          IF (FC.GT.0.D0) THEN
            NC=NC+PAS
          ELSE
            NC=NC-PAS
          ENDIF
          IF (NC.NE.0.D0) THEN
            IF ( (PE+NC).GT.MAT(2)) THEN
              FC=1.D0-((MAT(3)/MAT(1))*EXP(MAT(2))*(PE+NC-MAT(2))**NC)
     &              /(NC**NC)
            ELSE
              IF (FC.GT.0.D0) THEN
                NC=NC-PAS
              ELSE
                NC=NC+PAS
              ENDIF
            ENDIF
          ELSE
            FC=1.D0-(MAT(3)/MAT(1))*EXP(MAT(2))
          ENDIF
          GOTO 1
        ELSE
C       VALEUR DE K
          IF ( MAT(2).NE.0.D0) THEN
            MATERD(1,2)=MAT(3)/(MAT(2)**MAT(2))
          ELSE
            MATERD(1,2)=MAT(3)
          ENDIF

C       VALEUR DE N
          MATERD(2,2)=MAT(2)
C       VALEUR DE P0
          MATERD(3,2)=0.D0
        ENDIF
      ENDIF
      K=MATERD(1,2)
      N=MATERD(2,2)
      P0=MATERD(3,2)
C  AFFECTATION DES VALEURS PLASTIQUES
C  ECRETAGE DE LA LOI POUR LES FAIBLES VALEURS DE P

      IF (N.EQ.0.D0) THEN
        MATERD(1,2)=K
        MATERD(2,2)=0.D0
        MATERD(3,2)=0.D0
      ELSE
        IF ((P.GE.PE).AND.((K*(P+P0)**N).GT.
     &       (MAT(9)*MAT(1)))) THEN
          MATERD(1,2)=K
          MATERD(2,2)=N
          MATERD(3,2)=P0
        ELSE
          A=N*K*((PE+P0)**(N-1.D0))
          B=K/A*(PE+P0)**N-PE
          IF ( A*(P+B).LT.MAT(9)*MAT(1)) THEN
            MATERD(1,2)=MAT(9)*MAT(1)
            MATERD(2,2)=0.D0
            MATERD(3,2)=0.D0
          ELSE
            MATERD(1,2)=A
            MATERD(2,2)=1.D0
            MATERD(3,2)=B
          ENDIF
        ENDIF
      ENDIF

C  VALEUR DE AI0
      MATERD(4,2)=MAT(4)
C  VALEUR DE ETAI_S
      MATERD(5,2)=MAT(5)
C  VALEUR DE AG
      MATERD(6,2)=MAT(6)*(1-EXP(MAT(7)*(MAT(8)-IRRAD))/
     &                   (1+EXP(MAT(7)*(MAT(8)-IRRAD))))/3.D0

C -     RECUPERATION MATERIAU A TEMPF ET IRRAF 
C
       CALL RCVALB(FAMI,KPG,KSP,'+',IMAT,' ','ELAS',1,'TEMP',TEMPF,
     +               3,NOMC(1),MATERF(1,1),CERR(1), 'FM' )

       CALL RCVALB(FAMI,KPG,KSP,'+',IMAT,' ','IRRAD3M',1,'TEMP',TEMPF,
     +               9,NOMC(4),MAT,CERR(3), 'FM' )

       CALL RCVARC('F','IRRA','+',FAMI,KPG,KSP,IRRAF,IRET)
C     METHODE DE DICHOTOMIE POUR CALCULER LES VALEURS A L INSTANT +

C     POUR LA PLASTICITE, LES COEFFICIENTS DE LA COURBE SONT
C     SUSCEPTIBLES DE CHANGER SELON LA VALEUR DE LA DEFORMATION
C     PLASTIQUE CUMULE ( LOI LINEAIRE POUR LES FAIBLES VALEURS ).
C     ON STOCKE LES COEFFICIENTS K,P0 ET N DE LA LOI PUISSANCE
C     A LA PLACE 7,8 ET 9.
C     LES VRAIS COEFFICIENTS DE LA LOI SONT CALCULE DANS IRRRES
C     POUR L INCREMENT DE DEFORMATION PLASTIQUE ACTUEL ET STOCKE
C     DANS LE TABLEAU A LA PLACE 1,2 ET 3.
C     POUR LE CALCUL ELASTIQUE FAIT DANS LCELAS, ON STOCKE EN
C     1,2 ET 3 LES COEFFICIENTS EN SUPPOSANT UN INCREMENT PUREMENT 
C     ELASTIQUE ( DEFORMATION PLASTIQUE CUMULE INCHANGE )


      NC=MAT(2)
      IF ( NC.NE.0.D0) THEN
        FC=1-((MAT(3)/MAT(1))*EXP(MAT(2))*(PE+NC-MAT(2))**NC)/
     &     (NC**NC)
      ELSE
        FC=1-(MAT(3)/MAT(1))*EXP(MAT(2))
      ENDIF

      PAS=MAT(2)/2.D0
      ITERAT=0

10    CONTINUE

      ITERAT=ITERAT+1

C   CONVERGENCE ?
      IF (ABS(FC).LE.RELA) THEN
C   VALEUR DE N
        MATERF(8,2)=NC
C   VALEUR DE K
        IF (MATERF(8,2).NE.0.D0) THEN
          MATERF(7,2)=MAT(3)*EXP(MAT(2))/(MATERF(8,2)**MATERF(8,2))
        ELSE
          MATERF(7,2)=MAT(3)*EXP(MAT(2))
        ENDIF
C   VALEUR DE P0
        MATERF(9,2)=MATERF(8,2)-MAT(2)
      ELSE
        IF (ITERAT.LT.ITMAX) THEN
          PAS=PAS/2.D0
          IF (FC.GT.0.D0) THEN
            NC=NC+PAS
          ELSE
            NC=NC-PAS
          ENDIF
          IF (NC.NE.0.D0) THEN
            IF ( (PE+NC).GT.MAT(2)) THEN
              FC=1.D0-((MAT(3)/MAT(1))*EXP(MAT(2))*(PE+NC-MAT(2))**NC)
     &              /(NC**NC)
            ELSE
              IF (FC.GT.0.D0) THEN
                NC=NC-PAS
              ELSE
                NC=NC+PAS
              ENDIF
            ENDIF
          ELSE
            FC=1.D0-(MAT(3)/MAT(1))*EXP(MAT(2))
          ENDIF
          GOTO 10
        ELSE
          CALL UTMESS ('A','IRRMAT','LA PARTIE PLASTIQUE DE LA'//
     &  ' LOI DE COMPORTEMENT NE PEUT S ECRIRE SOUS LA FORME'//
     &  ' K*(P+P0)**N. SOIT LE NOMBRE D ITERATION (ITER_INTE_MAXI)'//
     &  ' N EST PAS ASSEZ GRAND SOIT LA SOLUTION THEORIQUE N EXISTE'//
     &  ' PAS. ON PREND LA LIMITE D ELASTICITE SOUS LA FORME K*P**N')
C     VALEUR DE K
          IF (MAT(2).NE.0.D0) THEN
            MATERF(7,2)=MAT(3)/(MAT(2)**MAT(2))
          ELSE
            MATERF(7,2)=MAT(3)
          ENDIF
C     VALEUR DE N
          MATERF(8,2)=MAT(2)
C     VALEUR DE P0
          MATERF(9,2)=0.D0
        ENDIF
      ENDIF

      K=MATERF(7,2)
      N=MATERF(8,2)
      P0=MATERF(9,2)
      
C     AFFECTATION DES VALEURS PLASTIQUES
C     ECRETAGE DE LA LOI POUR LES FAIBLES VALEURS DE P

      IF (N.EQ.0.D0) THEN
        MATERF(1,2)=K
        MATERF(2,2)=0.D0
        MATERF(3,2)=0.D0
      ELSE
        IF ((P.GE.PE).AND.((K*(P+P0)**N).GT.
     &       (MATERF(10,2)*MATERF(11,2)))) THEN
          MATERF(1,2)=K
          MATERF(2,2)=N
          MATERF(3,2)=P0
        ELSE
          A=N*K*(PE+P0)**(N-1.D0)
          B=K/A*(PE+P0)**N-PE
          IF ( A*(PE+B).LT.MAT(9)*MAT(1)) THEN
            MATERF(1,2)=MAT(9)*MAT(1)
            MATERF(2,2)=0.D0
            MATERF(3,2)=0.D0
          ELSE
            MATERF(1,2)=A
            MATERF(2,2)=1.D0
            MATERF(3,2)=B
          ENDIF
        ENDIF
      ENDIF
C     VALEUR DE AI0
      MATERF(4,2)=MAT(4)
C     VALEUR DE ETAI_S
      MATERF(5,2)=MAT(5)
C     VALEUR DE AG
      MATERF(6,2)=MAT(6)*(1-EXP(MAT(7)*(MAT(8)-IRRAF))/
     &                   (1+EXP(MAT(7)*(MAT(8)-IRRAF))))/3.D0
C     VALEUR DE KAPPA
      MATERF(10,2)=MAT(9)
C     VALEUR DE R0.2
      MATERF(11,2)=MAT(1)
C -   MATERIAU CONSTANT ?
C -   ON NE PEUT PAS SAVOIR A L AVANCE DONC NON
      MATCST = 'NON'

      END
