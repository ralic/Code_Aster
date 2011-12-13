       SUBROUTINE  NMGRT2(NNO,POIDS,KPG,VFF,DEF,PFF,OPTION,AXI,R,
     &             RESI,RIGI,DSIDEP,SIGN,SIGMA,MATSYM,MATUU,VECTU)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/11/2011   AUTEUR PROIX J-M.PROIX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
       INTEGER NNO,KK,KKD,N,I,M,J,J1,KL,NMAX,KPG
       CHARACTER*16  OPTION
       REAL*8 PFF(4,NNO,NNO),DEF(4,NNO,2),DSIDEP(6,6),POIDS,VECTU(*)
       REAL*8 SIGMA(6),SIGN(6),MATUU(*),VFF(*)
       REAL*8 TMP1,TMP2,SIGG(6),SIG(6),R
       LOGICAL MATSYM,AXI,RESI,RIGI

C.......................................................................
C     BUT:  CALCUL DE LA MATRICE TANGENTE EN CONFIGURATION LAGRANGIENNE
C           OPTIONS RIGI_MECA_TANG ET FULL_MECA 
C.......................................................................
C IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
C IN  POIDS   : POIDS DES POINTS DE GAUSS
C IN  KPG     : NUMERO DU POINT DE GAUSS
C IN  VFF     : VALEUR  DES FONCTIONS DE FORME
C IN  DEF     : PRODUIT DE F PAR LA DERIVEE DES FONCTIONS DE FORME
C IN  PFF     : PRODUIT DES FONCTIONS DE FORME
C IN  OPTION  : OPTION DE CALCUL
C IN  AXI     : .TRUE. SI AXIS
C IN  R       : RAYON DU POINT DE GAUSS COURANT (EN AXI)
C IN  DSIDEP  : OPERATEUR TANGENT ISSU DU COMPORTEMENT
C IN  SIGN    : CONTRAINTES PK2 A L'INSTANT PRECEDENT (AVEC RAC2)
C IN  SIGMA   : CONTRAINTES PK2 A L'INSTANT ACTUEL    (AVEC RAC2)
C IN  MATSYM  : VRAI SI LA MATRICE DE RIGIDITE EST SYMETRIQUE
C OUT MATUU   : MATRICE DE RIGIDITE PROFIL (RIGI_MECA_TANG ET FULL_MECA)
C OUT VECTU   : VECTEUR DES FORCES INTERIEURES (RAPH_MECA ET FULL_MECA)
C.......................................................................
C
      
      IF (RIGI) THEN

         IF (OPTION(1:4).EQ.'RIGI') THEN
 
            SIGG(1)=SIGN(1)
            SIGG(2)=SIGN(2)
            SIGG(3)=SIGN(3)
            SIGG(4)=SIGN(4)
 
         ELSE
 
            SIGG(1)=SIGMA(1)
            SIGG(2)=SIGMA(2)
            SIGG(3)=SIGMA(3)
            SIGG(4)=SIGMA(4)
 
         ENDIF
      
         DO 160 N=1,NNO
         
            DO 150 I=1,2
            
               DO 151,KL=1,4
               
                  SIG(KL)=0.D0
                  SIG(KL)=SIG(KL)+DEF(1,N,I)*DSIDEP(1,KL)
                  SIG(KL)=SIG(KL)+DEF(2,N,I)*DSIDEP(2,KL)
                  SIG(KL)=SIG(KL)+DEF(3,N,I)*DSIDEP(3,KL)
                  SIG(KL)=SIG(KL)+DEF(4,N,I)*DSIDEP(4,KL)
                  
151            CONTINUE

               IF(MATSYM) THEN
                  NMAX = N
               ELSE
                  NMAX = NNO
               ENDIF
               
               DO 140 J=1,2
               
                  DO 130 M=1,NMAX

C                    RIGIDITE GEOMETRIQUE

                     TMP1 = 0.D0
                     
                     IF (I.EQ.J) THEN
                     
                        TMP1 = PFF(1,N,M)*SIGG(1)
     &                       + PFF(2,N,M)*SIGG(2)
     &                       + PFF(3,N,M)*SIGG(3)
     &                       + PFF(4,N,M)*SIGG(4)

C                       TERME DE CORRECTION AXISYMETRIQUE

                        IF (AXI .AND. I.EQ.1) THEN
                           TMP1=TMP1+VFF(N+(KPG-1)*NNO)*
     &                          VFF(M+(KPG-1)*NNO)/(R*R)*SIGG(3)
                        END IF
              
                     ENDIF

C                    RIGIDITE DE COMPORTEMENT

                     TMP2=0.D0
                     TMP2=TMP2+SIG(1)*DEF(1,M,J)
                     TMP2=TMP2+SIG(2)*DEF(2,M,J)
                     TMP2=TMP2+SIG(3)*DEF(3,M,J)
                     TMP2=TMP2+SIG(4)*DEF(4,M,J)

                     IF(MATSYM) THEN
                     
C                        STOCKAGE EN TENANT COMPTE DE LA SYMETRIE

                        IF (M.EQ.N) THEN
                         J1 = I
                        ELSE
                         J1 = 2
                        ENDIF
                        
                        IF (J.LE.J1) THEN
                           KKD = (2*(N-1)+I-1) * (2*(N-1)+I)/2
                           KK = KKD + 2*(M-1)+J
                           MATUU(KK) = MATUU(KK) + (TMP1+TMP2)*POIDS
                        END IF
                        
                     ELSE
                     
C                       STOCKAGE SANS SYMETRIE

                        KK = 2*NNO*(2*(N-1)+I-1) + 2*(M-1)+J
                        MATUU(KK) = MATUU(KK) + (TMP1+TMP2)*POIDS
                        
                     ENDIF

 130              CONTINUE
 
 140           CONTINUE
 
 150        CONTINUE
 
 160     CONTINUE
 
      ENDIF

      IF (RESI) THEN
      
         DO 230 N=1,NNO
            DO 220 I=1,2
               DO 210 KL=1,4
                  VECTU(2*(N-1)+I)=VECTU(2*(N-1)+I)+
     &                             DEF(KL,N,I)*SIGMA(KL)*POIDS
 210           CONTINUE
 220        CONTINUE
 230     CONTINUE
      
      ENDIF
      
      END
