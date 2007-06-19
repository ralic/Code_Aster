      SUBROUTINE MMMAA1(NBDM,NDIM,NNE,NNM,
     &                  IMA,IMABAR,INDNOB,INDRAC,
     &                  HPG,FFPC,FFPR,JACOBI,
     &                  TYALGC,COEFCA,COEFCS,COEFCP,ICOMPL,
     &                  COEASP,ASPERI,JEU,NORM,IUSURE,KWEAR,
     &                  HWEAR,DISSIP,VECT,DEPLE,
     &                  MMAT)
      IMPLICIT NONE
      INTEGER  NBDM,NDIM,NNE,NNM,IMA,IMABAR,INDNOB,INDRAC
      INTEGER  ICOMPL,IUSURE,TYALGC
      REAL*8   MMAT(81,81),HPG,FFPC(9),FFPR(9),JACOBI
      REAL*8   NORM(3),VECT(3),DEPLE(6)
      REAL*8   COEFCA,COEFCS,COEFCP,COEASP
      REAL*8   ASPERI,KWEAR,HWEAR,DISSIP,JEU
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/06/2007   AUTEUR VIVAN L.VIVAN 
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
C TOLE CRP_21
C
C ROUTINE APPELLEE PAR : TE0364
C ----------------------------------------------------------------------
C
C CALCUL DE A ET DE AT POUR LE CONTACT METHODE CONTINUE
C
C IN  NBDM   : NB DE DDL DE LA MAILLE ESCLAVE
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
C IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
C IN  IMA    : NUMERO DE LA MAILLE ESCLAVE
C IN  IMABAR : NUMERO DE LA MAILLE ESCLAVE DE L'ELEMENT DE BARSOUM
C IN  INDNOB : NUMERO DU NOEUD A EXCLURE DANS LA MAILLE POUR BARSOUM
C IN  INDRAC : NUMERO DU NOEUD EN FACE DU NOEUD A EXCLURE POUR BARSOUM
C IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
C IN  FFPC   : FONCTIONS DE FORME DU POINT DE CONTACT
C IN  FFPR   : FONCTIONS DE FORME DE LA PROJECTION DU POINT DE CONTACT
C IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
C IN  COEFCA : COEF_REGU_CONT
C IN  COEFFS : COEF_STAB_CONT
C IN  COEFFP : COEF_PENA_CONT
C IN  TYALGC : TYPE D'ALGORITHME DE CONTACT
C IN  ICOMPL : INDICATEUR DE COMPLIANCE 
C IN  COEASP : PARAMETRE E_N POUR LA COMPLIANCE
C IN  ASPERI : VALEUR DE L'ASPERITE 
C IN  JEU    : VALEUR DU JEU
C IN  NORM   : VALEUR DE LA NORMALE AU POINT DE CONTACT
C I/O MMAT   : MATRICE ELEMENTAIRE DE CONTACT/FROTTEMENT
C ----------------------------------------------------------------------
      INTEGER   I, J, K, L, II, JJ
C ----------------------------------------------------------------------
C
C --- TRAITEMENT FOND DE FISSURE       
C  
      IF (IMA .EQ. IMABAR) THEN
        IF (INDNOB .GT. 0) THEN
          DO 69 I = INDNOB,INDNOB
            DO 59 J = INDNOB,INDNOB
              II = NBDM*(I-1)+NDIM+1
              JJ = NBDM*(J-1)+NDIM+1
              MMAT(II,JJ)= -FFPC(J)*FFPC(I)
  59        CONTINUE
  69      CONTINUE
        ENDIF 
      ENDIF
C
C --- TRAITEMENT DE RACCORD SURFACIQUE
C
      IF (INDRAC .GT. 0) THEN
        DO 526 I = INDRAC,INDRAC
          DO 527 J = INDRAC,INDRAC
            II = NBDM*(I-1)+NDIM+1
            JJ = NBDM*(J-1)+NDIM+1
            MMAT(II,JJ) = -FFPC(J)*FFPC(I)
 527      CONTINUE
 526    CONTINUE
      ENDIF          
C
C --- CALCUL DE A ET DE AT
C --- PREMIERE PARTIE DE A ET AT : PARTIE ESCLAVE ESCLAVE
C 
      DO 90 I = 1,NNE
        DO 80 J = 1,NNE
          DO 70 K = 1,NDIM
            II = NBDM*(I-1)+NDIM+1
            JJ = NBDM*(J-1)+K
            IF (IUSURE .EQ. 1 .AND. DISSIP .NE. 0.D0) THEN
              MMAT(II,JJ) = -HPG*FFPC(I)*FFPC(J)*JACOBI*NORM(K)*
     &                    (1+((KWEAR/HWEAR)*DISSIP*COEFCA))
              MMAT(JJ,II) = -HPG*FFPC(I)*FFPC(J)*JACOBI*NORM(K)+
     &                    (HPG*FFPC(I)*FFPC(J)*JACOBI*VECT(K)*
     &                    (KWEAR/(HWEAR*DISSIP))*DEPLE(NDIM+1))
            ELSE
              MMAT(II,JJ) = -HPG*FFPC(I)*FFPC(J)*JACOBI*NORM(K)
              MMAT(JJ,II) = -HPG*FFPC(I)*FFPC(J)*JACOBI*NORM(K)
            END IF
   70     CONTINUE
   80   CONTINUE
   90 CONTINUE
C   
C --- TRAITEMENT FOND DE FISSURE 
C             
      IF (IMA .EQ. IMABAR) THEN
        IF (INDNOB .GT. 0) THEN
          DO 92 I = INDNOB,INDNOB
            DO 82 J = 1,NNE
              DO 72 K = 1,NDIM
                II = NBDM*(I-1)+NDIM+1
                JJ = NBDM*(J-1)+K
                MMAT(II,JJ) = 0.D0
                MMAT(JJ,II) = 0.D0
   72         CONTINUE
   82       CONTINUE
   92     CONTINUE
        END IF
      END IF
C
C --- TRAITEMENT RACCORD SURFACIQUE      
C        
      IF (INDRAC .GT. 0) THEN
        DO 93 I = INDRAC,INDRAC
          DO 83 J = 1,NNE
            DO 73 K = 1,NDIM
              II = NBDM*(I-1)+NDIM+1
              JJ = NBDM*(J-1)+K            
              MMAT(II,JJ) = 0.D0
              MMAT(JJ,II) = 0.D0
   73       CONTINUE
   83     CONTINUE
   93   CONTINUE
      END IF
C
C --- DEUXIEME PARTIE DE A ET AT : PARTIE ESCLAVE MAITRE
C 
      DO 120 I = 1,NNE
        DO 110 J = 1,NNM
          DO 100 K = 1,NDIM
            II = NBDM*(I-1)+NDIM+1
            JJ = NBDM*NNE+NDIM*(J-1)+K          
            IF (IUSURE .EQ. 1 .AND. DISSIP .NE. 0.D0) THEN
                 MMAT(II,JJ) = HPG*FFPC(I)*FFPR(J)*JACOBI*NORM(K)*
     &                   (1-((KWEAR/HWEAR)*DISSIP*COEFCA))
                 MMAT(JJ,II) = HPG*FFPC(I)*FFPR(J)*JACOBI*NORM(K)+
     &                   (HPG*FFPC(I)*FFPR(J)*JACOBI*VECT(K)*
     &                   (KWEAR/(HWEAR*DISSIP))*DEPLE(NDIM+1))
            ELSE
                 MMAT(II,JJ) = HPG*FFPC(I)*FFPR(J)*JACOBI*NORM(K)
                 MMAT(JJ,II) = HPG*FFPC(I)*FFPR(J)*JACOBI*NORM(K)
            END IF
  100     CONTINUE
  110   CONTINUE
  120 CONTINUE
C
C --- TRAITEMENT FOND DE FISSURE
C
      IF (IMA .EQ. IMABAR) THEN
        IF (INDNOB .GT. 0) THEN
          DO 129 I = INDNOB,INDNOB
            DO 119 J = 1,NNM
              DO 109 K = 1,NDIM
                II = NBDM*(I-1)+NDIM+1
                JJ = NBDM*NNE+NDIM*(J-1)+K               
                MMAT(II,JJ) = 0.D0
                MMAT(JJ,II) = 0.D0                
  109         CONTINUE
  119       CONTINUE
  129     CONTINUE
        END IF
      END IF
C
C --- TRAITEMENT DE RACCORD SURFACIQUE
C
      IF (INDRAC .GT. 0) THEN
        DO 159 I = INDRAC,INDRAC
          DO 149 J = 1,NNM
            DO 139 K = 1,NDIM
              II = NBDM*(I-1)+NDIM+1
              JJ = NBDM*NNE+NDIM*(J-1)+K                
              MMAT(II,JJ) = 0.D0
              MMAT(JJ,II) = 0.D0  
  139       CONTINUE
  149     CONTINUE
  159   CONTINUE
      END IF
C
C --- CALCUL DE A_U
C --- PREMIER BLOC DE LA MATRICE [AU]: PARTIE ESCLAVE ESCLAVE
C           
      DO 160 I = 1,NNE
        DO 150 J = 1,NNE
          DO 140 K = 1,NDIM
            DO 130 L = 1,NDIM
              II = NBDM*(I-1)+L
              JJ = NBDM*(J-1)+K            
              IF (IUSURE .EQ. 1 .AND. DISSIP .NE. 0.D0) THEN
                MMAT(II,JJ) = (COEFCA+(ICOMPL*COEASP*2*(JEU-ASPERI)))*
     &                       HPG*FFPC(I)*NORM(L)*FFPC(J)*JACOBI*
     &                       NORM(K)-(COEFCA*HPG*FFPC(I)*FFPC(J)*
     &                       JACOBI*NORM(K)*(KWEAR/(HWEAR*DISSIP))*
     &                       VECT(L)*DEPLE(NDIM+1))
              ELSE
                IF (TYALGC .EQ. 1) THEN
                MMAT(II,JJ) = (COEFCA+(ICOMPL*COEASP*2*(JEU-ASPERI)))*
     &                       HPG*FFPC(I)*NORM(L)*FFPC(J)*JACOBI*NORM(K)
                ELSEIF (TYALGC .EQ. 2) THEN
                MMAT(II,JJ) = 0.D0
                ELSEIF (TYALGC .EQ. 3) THEN
                MMAT(II,JJ) = (COEFCP+(ICOMPL*COEASP*2*(JEU-ASPERI)))*
     &                       HPG*FFPC(I)*NORM(L)*FFPC(J)*JACOBI*NORM(K)
                END IF
              END IF
  130       CONTINUE
  140     CONTINUE
  150   CONTINUE
  160 CONTINUE
C
C --- DEUXIEME BLOC DE LA MATRICE [AU] PARTIE ESCLAVE MAITRE
C 
      DO 200 I = 1,NNE
        DO 190 J = 1,NNM
          DO 180 K = 1,NDIM
            DO 170 L = 1,NDIM
              II = NBDM*(I-1)+L
              JJ = NBDM*NNE+NDIM*(J-1)+K              
            IF (IUSURE .EQ. 1 .AND. DISSIP .NE. 0.D0) THEN           
              MMAT(II,JJ) = -(COEFCA+(ICOMPL*COEASP*2*(JEU-ASPERI)))*
     &                      HPG*FFPC(I)*NORM(L)*FFPR(J)*JACOBI*
     &                      NORM(K)+(COEFCA*HPG*FFPC(I)*FFPR(J)*
     &                      JACOBI*NORM(K)*(KWEAR/(HWEAR*DISSIP))*
     &                      VECT(L)*DEPLE(NDIM+1))
            ELSE
                  IF (TYALGC .EQ. 1) THEN
              MMAT(II,JJ) = -(COEFCA+(ICOMPL*COEASP*2*(JEU-ASPERI)))*
     &                      HPG*FFPC(I)*NORM(L)*FFPR(J)*JACOBI*NORM(K)
              ELSEIF (TYALGC .EQ. 2) THEN
              MMAT(II,JJ) = 0.D0
              ELSEIF (TYALGC .EQ. 3) THEN
              MMAT(II,JJ) = -(COEFCP+(ICOMPL*COEASP*2*(JEU-ASPERI)))*
     &                      HPG*FFPC(I)*NORM(L)*FFPR(J)*JACOBI*NORM(K)
              END IF
            END IF
  170       CONTINUE
  180     CONTINUE
  190   CONTINUE
  200 CONTINUE
C
C --- TROISIEME BLOC DE LA MATRICE AU PARTIE MAITRE ESCLAVE
C 
      DO 240 I = 1,NNM
        DO 230 J = 1,NNE
          DO 220 K = 1,NDIM
            DO 210 L = 1,NDIM
              II = NBDM*NNE+NDIM*(I-1)+L
              JJ = NBDM*(J-1)+K             
              IF (IUSURE .EQ. 1 .AND. DISSIP .NE. 0.D0) THEN
                MMAT(II,JJ) = -(COEFCA+(COEASP*ICOMPL*2*(JEU-ASPERI)))*
     &                      HPG*FFPR(I)*NORM(L)*FFPC(J)*JACOBI*   
     &                      NORM(K)+(COEFCA*HPG*FFPR(I)*FFPC(J)*
     &                      JACOBI*NORM(K)*(KWEAR/(HWEAR*DISSIP))*
     &                      VECT(L)*DEPLE(NDIM+1))
              ELSE
                   IF (TYALGC .EQ. 1) THEN
              MMAT(II,JJ) = -(COEFCA+(COEASP*ICOMPL*2*(JEU-ASPERI)))*
     &                      HPG*FFPR(I)*NORM(L)*FFPC(J)*JACOBI*NORM(K)
              ELSEIF (TYALGC .EQ. 2) THEN
              MMAT(II,JJ) = 0.D0
              ELSEIF (TYALGC .EQ. 3) THEN
              MMAT(II,JJ) = -(COEFCP+(COEASP*ICOMPL*2*(JEU-ASPERI)))*
     &                      HPG*FFPR(I)*NORM(L)*FFPC(J)*JACOBI*NORM(K)
              END IF
              END IF
  210       CONTINUE
  220     CONTINUE
  230   CONTINUE
  240 CONTINUE
C
C --- QUATRIEME BLOC DE LA MATRICE AU PARTIE MAITRE MAITRE
C 
      DO 280 I = 1,NNM
        DO 270 J = 1,NNM
          DO 260 K = 1,NDIM
            DO 250 L = 1,NDIM
              II = NBDM*NNE+NDIM*(I-1)+L
              JJ = NBDM*NNE+NDIM*(J-1)+K
              IF (IUSURE .EQ. 1 .AND. DISSIP .NE. 0.D0) THEN
                MMAT(II,JJ) = (COEFCA+(COEASP*ICOMPL*2*(JEU-ASPERI)))*
     &                      HPG*FFPR(I)*NORM(L)*FFPR(J)*JACOBI*
     &                      NORM(K)-(COEFCA*HPG*FFPR(I)*FFPR(J)*
     &                      JACOBI*NORM(K)*(KWEAR/(HWEAR*DISSIP))*
     &                      VECT(L)*DEPLE(NDIM+1))
              ELSE
                     IF (TYALGC .EQ. 1) THEN
                MMAT(II,JJ) = (COEFCA+(COEASP*ICOMPL*2*(JEU-ASPERI)))*
     &                      HPG*FFPR(I)*NORM(L)*FFPR(J)*JACOBI*NORM(K)
                ELSEIF (TYALGC .EQ. 2) THEN
                MMAT(II,JJ) = 0.D0
                ELSEIF (TYALGC .EQ. 3) THEN
                MMAT(II,JJ) = (COEFCP+(COEASP*ICOMPL*2*(JEU-ASPERI)))*
     &                      HPG*FFPR(I)*NORM(L)*FFPR(J)*JACOBI*NORM(K)
                END IF
              END IF
  250       CONTINUE
  260     CONTINUE
  270   CONTINUE
  280 CONTINUE   
C
C ---- MATRICE USURE COUPLAGE LAMBDA-LAMBDA
C
      IF (IUSURE .EQ. 1 .AND. DISSIP .NE. 0.D0) THEN
        DO 64 I = 1,NNE
          DO 54 J = 1,NNE
            II = NBDM*(I-1)+NDIM+1
            JJ = NBDM*(J-1)+NDIM+1
            MMAT(II,JJ) = HPG*FFPC(J)*FFPC(I)*JACOBI*
     &                   ((KWEAR/HWEAR)*DISSIP)
   54     CONTINUE
   64   CONTINUE
      END IF
      
      END
