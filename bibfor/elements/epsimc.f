      SUBROUTINE EPSIMC (OPTION,XYZ,NNO,NPG,NDIM,NBSIG,NI,EPS)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 30/03/2004   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
C.======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)
C
C      EPSIMC   -- CONSTRUCTION DU VECTEUR DES DEFORMATIONS INITIALES
C                  DEFINIES EN CHAQUE POINT D'INTEGRATION
C                  A PARTIR DES DONNEES UTILISATEUR POUR L'ELEMENT 
C                  COURANT
C
C   ARGUMENT        E/S  TYPE         ROLE
C    OPTION         IN    K16       NOM  DE L'OPTION
C    XYZ(1)         IN     R        COORDONNEES DES CONNECTIVITES
C    NNO            IN     I        NOMBRE DE NOEUDS DE L'ELEMENT
C    NPG            IN     I        NOMBRE DE POINTS D'INTEGRATION
C                                   DE L'ELEMENT
C    NDIM           IN     I        DIMENSION  DE L'ELEMENT ( 2 OU 3)
C    NBSIG          IN     I        NOMBRE DE CONTRAINTES ASSOCIE A
C                                   L'ELEMENT
C    NI(1)          IN     R        FONCTIONS DE FORME
C    EPS(1)         OUT    R        DEFORMATIONS AUX POINTS
C                                   D'INTEGRATION
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           CHARACTER*16 OPTION
           REAL*8       XYZ(1), NI(1), EPS(1)
C -----  VARIABLES LOCALES
           CHARACTER*8 NOMPAR(4)
           REAL*8      VALPAR(4)
C.========================= DEBUT DECLARATIONS NORMALISEES  JEVEUX ====
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C.========================= FIN DECLARATIONS NORMALISEES  JEVEUX ====
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     -----------------
      ZERO   = 0.0D0
      DEUX   = 2.0D0
C
      DO 10 I = 1, NBSIG*NPG
         EPS(I) = ZERO
 10   CONTINUE
C
C      -------
C ---- CAS 2D
C      -------
      IF (NDIM.EQ.2) THEN
C
C ---- RECUPERATION DES DEFORMATIONS
C      -----------------------------
        IF (OPTION(16:16).EQ.'R') THEN
C
           CALL JEVECH('PEPSINR','L',IDEFI)
           EXX=ZR(IDEFI)
           EYY=ZR(IDEFI+1)
           EZZ=ZR(IDEFI+2)
           EXY=ZR(IDEFI+3)
C
C ---  BOUCLE SUR LES POINTS D'INTEGRATION 
C      -----------------------------------
           DO 20 IGAU = 1, NPG
              EPS(1+NBSIG*(IGAU-1)) = EXX
              EPS(2+NBSIG*(IGAU-1)) = EYY
              EPS(3+NBSIG*(IGAU-1)) = EZZ
              EPS(4+NBSIG*(IGAU-1)) = EXY*DEUX
  20       CONTINUE
        ELSE
           CALL JEVECH('PEPSINF','L',IDEFI)
           CALL JEVECH('PTEMPSR','L',ITEMPS)
           NOMPAR(1) = 'X'
           NOMPAR(2) = 'Y'
           NOMPAR(3) = 'INST'
           VALPAR(3) = ZR(ITEMPS)
C
C ---  BOUCLE SUR LES POINTS D'INTEGRATION 
C      -----------------------------------
           DO 30 IGAU = 1, NPG
C
C  --      COORDONNEES DU POINT D'INTEGRATION COURANT
C          -----------------------------------------
            XGAU = ZERO
            YGAU = ZERO
C
            DO 40 I = 1, NNO
               XGAU = XGAU + NI(I+NNO*(IGAU-1))*XYZ(1+2*(I-1))
               YGAU = YGAU + NI(I+NNO*(IGAU-1))*XYZ(2+2*(I-1))
  40        CONTINUE
C
             VALPAR(1) = XGAU
             VALPAR(2) = YGAU
C
C  --        INTERPOLATION 
C            -------------
             CALL FOINTE('FM',ZK8(IDEFI  ),3,NOMPAR,VALPAR,EXX,IER)
             CALL FOINTE('FM',ZK8(IDEFI+1),3,NOMPAR,VALPAR,EYY,IER)
             CALL FOINTE('FM',ZK8(IDEFI+2),3,NOMPAR,VALPAR,EZZ,IER)
             CALL FOINTE('FM',ZK8(IDEFI+3),3,NOMPAR,VALPAR,EXY,IER)
C
             EPS(1+NBSIG*(IGAU-1)) = EXX
             EPS(2+NBSIG*(IGAU-1)) = EYY
             EPS(3+NBSIG*(IGAU-1)) = EZZ
             EPS(4+NBSIG*(IGAU-1)) = EXY*DEUX
C
  30       CONTINUE
C
        ENDIF
C
C      -------
C ---- CAS 3D
C      -------
      ELSEIF (NDIM.EQ.3) THEN
C
C ---- RECUPERATION DES DEFORMATIONS
C      -----------------------------
        IF (OPTION(16:16).EQ.'R') THEN
C
           CALL JEVECH('PEPSINR','L',IDEFI)
           EXX=ZR(IDEFI)
           EYY=ZR(IDEFI+1)
           EZZ=ZR(IDEFI+2)
           EXY=ZR(IDEFI+3)
           EXZ=ZR(IDEFI+4)
           EYZ=ZR(IDEFI+5)
C
C ---  BOUCLE SUR LES POINTS D'INTEGRATION 
C      -----------------------------------
           DO 50 IGAU = 1, NPG
              EPS(1+NBSIG*(IGAU-1)) = EXX
              EPS(2+NBSIG*(IGAU-1)) = EYY
              EPS(3+NBSIG*(IGAU-1)) = EZZ
              EPS(4+NBSIG*(IGAU-1)) = EXY*DEUX
              EPS(5+NBSIG*(IGAU-1)) = EXZ*DEUX
              EPS(6+NBSIG*(IGAU-1)) = EYZ*DEUX
  50       CONTINUE
        ELSE
           CALL JEVECH('PEPSINF','L',IDEFI)
           CALL JEVECH('PTEMPSR','L',ITEMPS)
           NOMPAR(1) = 'X'
           NOMPAR(2) = 'Y'
           NOMPAR(3) = 'Z'
           NOMPAR(4) = 'INST'
           VALPAR(4) = ZR(ITEMPS)
C
C ---  BOUCLE SUR LES POINTS D'INTEGRATION 
C      -----------------------------------
           DO 60 IGAU = 1, NPG
C
C  --      COORDONNEES DU POINT D'INTEGRATION COURANT
C          -----------------------------------------
            XGAU = ZERO
            YGAU = ZERO
            ZGAU = ZERO
C
            DO 70 I = 1, NNO
               XGAU = XGAU + NI(I+NNO*(IGAU-1))*XYZ(1+3*(I-1))
               YGAU = YGAU + NI(I+NNO*(IGAU-1))*XYZ(2+3*(I-1))
               ZGAU = ZGAU + NI(I+NNO*(IGAU-1))*XYZ(3+3*(I-1))
  70        CONTINUE
C
             VALPAR(1) = XGAU
             VALPAR(2) = YGAU
             VALPAR(3) = ZGAU
C
C  --        INTERPOLATION 
C            -------------
             CALL FOINTE('FM',ZK8(IDEFI  ),4,NOMPAR,VALPAR,EXX,IER)
             CALL FOINTE('FM',ZK8(IDEFI+1),4,NOMPAR,VALPAR,EYY,IER)
             CALL FOINTE('FM',ZK8(IDEFI+2),4,NOMPAR,VALPAR,EZZ,IER)
             CALL FOINTE('FM',ZK8(IDEFI+3),4,NOMPAR,VALPAR,EXY,IER)
             CALL FOINTE('FM',ZK8(IDEFI+4),4,NOMPAR,VALPAR,EXZ,IER)
             CALL FOINTE('FM',ZK8(IDEFI+5),4,NOMPAR,VALPAR,EYZ,IER)
C
             EPS(1+NBSIG*(IGAU-1)) = EXX
             EPS(2+NBSIG*(IGAU-1)) = EYY
             EPS(3+NBSIG*(IGAU-1)) = EZZ
             EPS(4+NBSIG*(IGAU-1)) = EXY*DEUX
             EPS(5+NBSIG*(IGAU-1)) = EXZ*DEUX
             EPS(6+NBSIG*(IGAU-1)) = EYZ*DEUX
C
  60       CONTINUE
C
        ENDIF
C
      ENDIF
C
C.============================ FIN DE LA ROUTINE ======================
      END
