      SUBROUTINE REFTHM(FNOEVO,DT,NNO,NNOS,NPGU,IPOIDS,IVF,IDFDE,GEOM,
     &                  B,DFDI,R,VECTU,IMATE,MECANI,PRESS1,PRESS2,
     &                  TEMPE,DIMDEF,DIMCON,NDDL,NMEC,NP1,NP2,NDIM,AXI,
     &                  NVOMAX,NNOMAX,NSOMAX,NBVOS,VOISIN,P2P1,CONTM)
      IMPLICIT  NONE
      LOGICAL   FNOEVO,AXI,P2P1
      INTEGER   NNO,NNOS,NPGU,IPOIDS,IVF,IDFDE,IMATE,DIMDEF,DIMCON
      INTEGER   MECANI(5),PRESS1(7),PRESS2(7),TEMPE(5)
      INTEGER   NDDL,NMEC,NP1,NP2,NDIM,NVOMAX,NNOMAX,NSOMAX
      INTEGER   VOISIN(NVOMAX,NNOMAX)
      INTEGER   NBVOS(NSOMAX)
      REAL*8    GEOM(NDIM,NNO),B(DIMDEF,NDDL*NNO),DFDI(NNO,3)
      REAL*8    R(1:DIMDEF),VECTU(NDDL*NNO),DT,CONTM(4)
C =====================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/05/2004   AUTEUR ROMEO R.FERNANDES 
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
C =====================================================================
C TOLE CRP_21
C =====================================================================
      INTEGER   INDICE,I,J,K,L
      INTEGER   PARSIG,PARTMP,PARBSI
      PARAMETER (PARSIG = 837 ,PARTMP = 162 ,PARBSI = 162 )
      REAL*8    SIGTM(PARSIG),FTEMP(PARTMP),BSIGM(PARBSI)
      REAL*8    R8VIDE,R8MIEM
C =====================================================================
C --- INITIALISATIONS --------------------------------------------------
C ======================================================================
        CALL R8INIR(DIMCON*NPGU,0.D0,SIGTM(1),1)
        CALL R8INIR(NDDL*NNO,   0.D0,FTEMP(1),1)
C ======================================================================
C --- TESTS DE COHERENCE -----------------------------------------------
C ======================================================================
C --- CES VERIFICATIONS ONT POUR OBJETIFS DE CONTROLER LES DIMENSIONS --
C --- DE NDDL, NNO ET DIMCON QUI DIMENSIONNENT LES VECTEURS SIGTM, -----
C --- FTEMP ET BSIGM ---------------------------------------------------
C ======================================================================
C --- LA DIMENSION MAX DE NDDL = NMEC + NP1 + NP2 + NT = 6 -------------
C --- LA DIMENSION MAX DE NNO CORRESPOND AU NOMBRE MAX DE NOEUD PAR ELT-
C --- LA DIMENSION MAX DE NPGU AU NOMBRE MAX DE PTS DE GAUSS PAR ELT ---
C --- LA DIMENSION MAX DE DIMCON (ROUTINE TE0600) ----------------------
C ======================================================================
         CALL ASSERT ( NDDL   .LE.  6 )
         CALL ASSERT ( NNO    .LE. 27 )
         CALL ASSERT ( NPGU   .LE. 27 )
         CALL ASSERT ( DIMCON .LE. 31 )
C ======================================================================
C --- CES VERIFICATIONS ONT POUR OBJECTIFS DE CONTROLER LA PRESENCE ----
C --- DES DIFFERENTS PARAMETRES DE REFERENCE ---------------------------
C ======================================================================
        IF ( MECANI(1).NE.0 ) THEN
           INDICE = 1
           IF ( CONTM(INDICE).EQ.R8VIDE() ) THEN
              CALL UTMESS('F','TE0600','IL MANQUE SIGM_REFE')
           ENDIF
        ENDIF
        IF ( PRESS1(1).NE.0 ) THEN
           INDICE = 2
           IF ( CONTM(INDICE).EQ.R8VIDE() ) THEN
              CALL UTMESS('F','TE0600','IL MANQUE RESI_HYD1_REFE')
           ENDIF
        ENDIF
        IF ( PRESS2(1).NE.0 ) THEN
           INDICE = 3
           IF ( CONTM(INDICE).EQ.R8VIDE() ) THEN
              CALL UTMESS('F','TE0600','IL MANQUE RESI_HYD2_REFE')
           ENDIF
        ENDIF
        IF ( TEMPE(1).NE.0 ) THEN
           INDICE = 4
           IF ( CONTM(INDICE).EQ.R8VIDE() ) THEN
              CALL UTMESS('F','TE0600','IL MANQUE RESI_THER_REFE')
           ENDIF
        ENDIF
C ======================================================================
C --- TESTS DE COHERENCE -----------------------------------------------
C ======================================================================
        DO 200 I = 1, NPGU
           DO 210 J = 1, DIMCON
              IF ( J.LE.MECANI(5) ) THEN
                 INDICE = 1
              ELSE IF ( J.LE.
     +                  (MECANI(5)+PRESS1(2)*PRESS1(7)) ) THEN
                 INDICE = 2
                 IF ( TEMPE(5).GT.0 ) THEN
C ======================================================================
C --- ON NE FAIT RIEN DANS LE CAS DE L'ENTHALPIE -----------------------
C ======================================================================
                    IF ( J.EQ.(MECANI(5)+PRESS1(7)) .OR.
     +                   J.EQ.(MECANI(5)+PRESS1(2)*PRESS1(7)) ) THEN
                       GO TO 210
                    ENDIF
                 ENDIF
              ELSE IF ( J.LE.
     +        (MECANI(5)+PRESS1(2)*PRESS1(7)+PRESS2(2)*PRESS2(7)) ) THEN
                 INDICE = 3
                 IF ( TEMPE(5).GT.0 ) THEN
C ======================================================================
C --- ON NE FAIT RIEN DANS LE CAS DE L'ENTHALPIE -----------------------
C ======================================================================
                    IF (
     +             J.EQ.(MECANI(5)+
     +                  PRESS1(2)*PRESS1(7)+PRESS2(7)) .OR.
     +             J.EQ.(MECANI(5)+
     +                  PRESS1(2)*PRESS1(7)+PRESS2(2)*PRESS2(7)) ) THEN
                       GO TO 210
                    ENDIF                 
                 ENDIF
              ELSE IF ( J.LE.(MECANI(5)+TEMPE(5)) ) THEN
                 INDICE = 4
              ENDIF

              IF ( CONTM(INDICE).EQ.R8VIDE() ) GOTO 210

              SIGTM(J+DIMCON*(I-1)) = CONTM(INDICE)
              CALL FNOTHM(FNOEVO,DT,NNO,NNOS,NPGU,IPOIDS,IVF,IDFDE,
     &                    GEOM,SIGTM,B,DFDI,R,BSIGM(1),IMATE,MECANI,
     &                    PRESS1,PRESS2,TEMPE,DIMDEF,DIMCON,NDDL,NMEC,
     &                    NP1,NP2,NDIM,AXI,NVOMAX,NNOMAX,NSOMAX,NBVOS,
     &                    VOISIN,P2P1)
     
              DO 220 K = 1,NDDL*NNOS
                 FTEMP(K) = FTEMP(K) + ABS(BSIGM(K))
 220          CONTINUE
 
              DO 221 K = NNOS,NNO-1
                 DO 222 L = 1,NMEC
                    FTEMP(K*NDDL+L) = FTEMP(K*NDDL+L) + 
     &                     ABS(BSIGM(K*NDDL+L))
 222            CONTINUE
C POUR TENIR COMPTE DU P2P1 (ON EST SUR LES NOEUDS MILIEUX P1) 
                IF (P2P1) THEN
                   DO 223 L = NMEC+1,NDDL
                      FTEMP(K*NDDL+L) =  FTEMP(K*NDDL+L) + 1.D0
 223               CONTINUE
                 ELSE
                    DO 224 L = NMEC+1,NDDL
                      FTEMP(K*NDDL+L) =FTEMP(K*NDDL+L) + 
     &                     ABS(BSIGM(K*NDDL+L))
 224               CONTINUE
                 ENDIF
 221          CONTINUE

              SIGTM(J+DIMCON*(I-1)) = 0.0D0

 210       CONTINUE
 200    CONTINUE
 
        CALL R8AXPY(NDDL*NNO,1.D0/NPGU,FTEMP(1),1,VECTU(1),1)

        DO 230 K = 1,NDDL*NNO
           IF ( ABS(VECTU(K)).LT.R8MIEM() ) THEN
              CALL UTMESS('F','TE0600','VECTEUR NUL ENTRAINANT '//
     +                           'UNE DIVISION PAR ZERO DANS NMCONV')
           ENDIF
 230    CONTINUE

      END
