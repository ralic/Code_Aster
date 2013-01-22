      SUBROUTINE VDREPE ( NOMTEZ , MATEVN , MATEVG )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 21/01/2013   AUTEUR DELMAS J.DELMAS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
C
C      VDREPE   -- DETERMINATION DES MATRICES DE PASSAGE
C                  DES REPERES INTRINSEQUES AUX NOEUDS  DE L'ELEMENT
C                  AU REPERE UTILISATEUR (MATRICE MATEVN)
C                  ET DES REPERES INTRINSEQUES AUX POINTS D'INTEGRATION
C                  DE L'ELEMENT AU REPERE UTILISATEUR (MATRICE MATEVG)
C                  POUR LES ELEMENTS DE COQUE EPAISSE 3D .
C
C   ARGUMENT        E/S   TYPE         ROLE
C    NOMTE          IN     K*       NOM DU TYPE D'ELEMENT
C    MATEVN(2,2,10) OUT    R        MATRICES DE PASSAGE DES REPERES
C                                   INTRINSEQUES AUX NOEUDS  DE
C                                   L'ELEMENT AU REPERE UTILISATEUR
C    MATEVG(2,2,10) OUT    R        MATRICES DE PASSAGE DES REPERES
C                                   INTRINSEQUES AUX POINTS
C                                   D'INTEGRATION DE L'ELEMENT AU
C                                   REPERE UTILISATEUR
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
      INCLUDE 'jeveux.h'
           CHARACTER*(*)     NOMTEZ
           REAL*8            MATEVN(2,2,1), MATEVG(2,2,1)
C -----  VARIABLES LOCALES
           CHARACTER*16      NOMTE
           REAL*8            PGL(3,3),R8BID4(4)
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
C --- INITIALISATIONS :
C     ---------------
C-----------------------------------------------------------------------
      INTEGER I ,IDEC ,IGAU ,INO ,J ,JCOQU ,K
      INTEGER LZI ,LZR ,NB2 ,NPGSR
      REAL*8 ALPHA ,BETA ,C
      REAL*8 R8DGRD ,S
C-----------------------------------------------------------------------
      NOMTE  = NOMTEZ
C
C --- RECUPERATION DES OBJETS DESCRIPTEURS DES ELEMENTS :
C     =================================================
      CALL JEVETE('&INEL.'//NOMTE(1:8)//'.DESI',' ', LZI )
C
C --- NOMBRE DE NOEUDS DE L'ELEMENT  :
C     -----------------------------
      NB2  = ZI(LZI-1+2)
C
C --- NOMBRE DE POINTS D'INTEGRATION DE L'ELEMENT (SOUS-INTEGRE) :
C     ----------------------------------------------------------
      NPGSR= ZI(LZI-1+3)
C
      CALL JEVETE('&INEL.'//NOMTE(1:8)//'.DESR',' ', LZR )
C
C --- RECUPERATION DES ANGLES DETERMINANT LE REPERE UTILISATEUR
C --- PAR RAPPORT AU REPERE GLOBAL :
C     ============================
      CALL JEVECH ('PCACOQU', 'L', JCOQU)
C
      ALPHA = ZR(JCOQU+1) * R8DGRD()
      BETA  = ZR(JCOQU+2) * R8DGRD()
C
C --- DETERMINATION DES MATRICES DE PASSAGE DES REPERES INTRINSEQUES
C --- AUX NOEUDS DE L'ELEMENT AU REPERE UTILISATEUR :
C     =============================================
C
C --- ADRESSE DES MATRICES DE PASSAGE DU REPERE GLOBAL AUX REPERES
C --- INTRINSEQUES AUX NOEUDS DE L'ELEMENT DANS LE TABLEAU .DESR :
C     ----------------------------------------------------------
      IDEC = 1090
C
C --- BOUCLE SUR LES NOEUDS DE L'ELEMENT :
C     ----------------------------------
      DO 10 INO = 1, NB2
C
C ---   RECUPERATION DE LA MATRICE DE PASSAGE AU NOEUD COURANT :
C       ------------------------------------------------------
        K = 0
        DO 20 J = 1, 3
          DO 30 I = 1, 3
            K  = K + 1
            PGL(I,J) = ZR(LZR+IDEC+(INO-1)*9+K-1)
  30      CONTINUE
  20    CONTINUE
C
C ---   DETERMINATION DE LA PROJECTION DU VECTEUR X DU REPERE
C ---   UTILISATEUR SUR LE FEUILLET TANGENT A LA COQUE AU NOEUD
C ---   COURANT :
C       -------

        CALL COQREP(PGL, ALPHA, BETA, R8BID4,R8BID4,C,S)

C       -- (C,S) N'EST PAS TOUJOURS EXACTEMENT DE NORME=1:
        C=C/SQRT(C*C+S*S)
        S=S/SQRT(C*C+S*S)

        MATEVN(1,1,INO) =  C
        MATEVN(2,1,INO) =  S
        MATEVN(1,2,INO) = -S
        MATEVN(2,2,INO) =  C
C
  10  CONTINUE
C
C --- DETERMINATION DES MATRICES DE PASSAGE DES REPERES INTRINSEQUES
C --- AUX POINTS D'INTEGRATION DE L'ELEMENT AU REPERE UTILISATEUR :
C     ===========================================================
C
C --- ADRESSE DES MATRICES DE PASSAGE DU REPERE GLOBAL AUX REPERES
C --- INTRINSEQUES AUX POINTS D'INTEGRATION DE L'ELEMENT
C --- DANS LE TABLEAU .DESR :
C     ---------------------
      IDEC = 2000
C
C --- BOUCLE SUR LES POINTS D'INTEGRATION DE L'ELEMENT (SOUS-INTEGRE) :
C     --------------------------------------------------------------
      DO 40 IGAU = 1, NPGSR
C
C ---   RECUPERATION DE LA MATRICE DE PASSAGE AU POINT D'INTEGRATION
C ---   COURANT :
C       -------
        K = 0
        DO 50 J = 1, 3
          DO 60 I = 1, 3
            K  = K + 1
            PGL(I,J) = ZR(LZR+IDEC+(IGAU-1)*9+K-1)
  60      CONTINUE
  50    CONTINUE
C
C ---   DETERMINATION DE LA PROJECTION DU VECTEUR X DU REPERE
C ---   UTILISATEUR SUR LE FEUILLET TANGENT A LA COQUE AU POINT
C ---   D'INTEGRATION COURANT :
C       ---------------------
        CALL COQREP(PGL, ALPHA, BETA, R8BID4,R8BID4,C,S)

C       -- (C,S) N'EST PAS TOUJOURS EXACTEMENT DE NORME=1:
        C=C/SQRT(C*C+S*S)
        S=S/SQRT(C*C+S*S)

        MATEVG(1,1,IGAU) =  C
        MATEVG(2,1,IGAU) =  S
        MATEVG(1,2,IGAU) = -S
        MATEVG(2,2,IGAU) =  C

  40  CONTINUE

      END
