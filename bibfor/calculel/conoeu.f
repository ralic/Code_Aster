      SUBROUTINE CONOEU(MA,CONNEX,LONCUM,COORD,NORMAL,DIME,L,CNOEUD,NNO)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 02/04/2002   AUTEUR RATEAU G.RATEAU 
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
C ----------------------------------------------------------------------
C                COORDONNEES DES NOEUDS DE LA MAILLE MA 
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C INTEGER  MA                 : NUMERO DE LA MAILLE
C INTEGER  CONNEX(*)          : CONNEXITE DES MAILLES
C INTEGER  LONCUM(*)          : LONGUEUR CUMULEE DE CONNEX
C REAL*8   COORD(3,*)         : COORDONNEES DES NOEUDS
C REAL*8   NORMAL(DIME,*)     : COORDONNEES DES NORMALES (CF LISNOR)
C INTEGER  DIME               : DIMENSION DE L'ESPACE
C INTEGER  L                  : L = 0 -> MAILLE TYPE SOLIDE
C                               L = 1 -> MAILLE TYPE COQUE (CF TMACOQ)
C                   
C VARIABLES DE SORTIE
C REAL*8    CNOEUD(DIME,NNO)  : COORD DES NOEUDS (X1, [Y1, Z1], X2, ...)
C INTEGER   NNO               : NOMBRE DE NOEUDS
C ----------------------------------------------------------------------
     
      IMPLICIT NONE

C --- VARIABLES
      INTEGER MA,CONNEX(*),LONCUM(*),DIME,L,NNO
      INTEGER NOECOQ(2,9),I,J,K1,K2,INO,P0
      REAL*8  COORD(3,*),NORMAL(DIME,*),CNOEUD(DIME,*),R,S

C --- TABLES
      INTEGER REGLE(2,63)
      REAL*8 COEF(63),TIERS
      PARAMETER (TIERS = 1.D0/3.D0)

      DATA REGLE / 
     & 1,2,3,4, 
     & 1,2,2,3,3,1,1,5,
     & 1,2,2,3,3,4,4,1,1,3,
     & 1,2,2,3,3,1,1,4,2,4,3,4, 
     & 1,2,2,3,3,1,4,5,5,6,6,4,1,8,4,11,           
     & 1,2,2,3,3,1,1,4,2,5,3,6,4,5,5,6,6,4,
     & 2,3,4,1,1,5,2,6,3,7,4,8,6,7,8,5,2,7,1,8,
     & 1,2,2,3,3,4,4,1,1,5,2,6,3,7,4,8,5,6,6,7,
     & 7,8,8,5,1,3,1,6,2,7,3,8,4,5,5,7,1,7 /
       
      DATA COEF / 
     & .5D0,.5D0,
     & .5D0,.5D0,.5D0,TIERS,
     & .5D0,.5D0,.5D0,.5D0,.5D0,
     & .5D0,.5D0,.5D0,.5D0,.5D0,.5D0,
     & .5D0,.5D0,.5D0,.5D0,.5D0,.5D0,TIERS,TIERS,
     & .5D0,.5D0,.5D0,.5D0,.5D0,.5D0,.5D0,.5D0,.5D0,
     & .5D0,.5D0,.5D0,.5D0,.5D0,.5D0,.5D0,.5D0,.5D0,.5D0,
     & .5D0,.5D0,.5D0,.5D0,.5D0,.5D0,.5D0,.5D0,.5D0,.5D0,
     & .5D0,.5D0,.5D0,.5D0,.5D0,.5D0,.5D0,.5D0,.5D0 /
  
C --- MAILLE TYPE SOLIDE SANS LA SD. MAILLAGE

      IF (MA.EQ.0) THEN

        NNO = LONCUM(1)

        DO 10 I = 1, NNO
          
          INO = CONNEX(I)
          
          DO 10 J = 1, DIME
            
            CNOEUD(J,I) = NORMAL(J,INO)

 10     CONTINUE

        IF (L.GT.NNO) THEN

          IF (DIME.EQ.2) THEN
            IF (NNO.EQ.3) THEN
              P0 = 3
            ELSEIF (L.EQ.6) THEN
              P0 = 1
            ELSE
              P0 = 7
            ENDIF
          ELSE
            IF (NNO.EQ.4) THEN
              P0 = 12
            ELSEIF (L.EQ.15) THEN
              P0 = 26
            ELSEIF (NNO.EQ.6) THEN
              P0 = 18
            ELSEIF (NNO.LT.20) THEN
              P0 = 35
            ELSE
              P0 = 45
            ENDIF
          ENDIF

          DO 20 I = I, L
            
            R = COEF(P0)
            S = 1.D0 - R
            K1 = REGLE(1,P0)
            K2 = REGLE(2,P0)
            P0 = P0 + 1
            
            DO 20 J = 1, DIME

              CNOEUD(J,I) = R*CNOEUD(J,K1) + S*CNOEUD(J,K2)
              
 20       CONTINUE
            
        ENDIF

      ELSE

        P0 = LONCUM(MA)
        NNO = LONCUM(MA+1)-P0

C --- MAILLE TYPE SOLIDE A PARTIR DE LA SD. MAILLAGE

        IF (L.EQ.0) THEN

          DO 30 I = 1, NNO
       
            INO = CONNEX(P0)
            P0 = P0 + 1

            DO 30 J = 1, DIME

              CNOEUD(J,I) = COORD(J,INO)
 
 30       CONTINUE

C --- MAILLE TYPE COQUE A PARTIR DE LA SD. MAILLAGE

        ELSEIF (L.EQ.1) THEN

          CALL NOCOQU(DIME,NNO,NOECOQ)

          DO 40 I = 1, NNO

            INO = CONNEX(P0)
            P0 = P0 + 1

            K1 = NOECOQ(1,I)
            K2 = NOECOQ(2,I)

            DO 40 J = 1, DIME

              R = COORD(J,INO)
              S = NORMAL(J,INO)

              CNOEUD(J,K1) = R - S 
              CNOEUD(J,K2) = R + S

 40       CONTINUE

        NNO = 2*NNO

        ENDIF

      ENDIF

      END
