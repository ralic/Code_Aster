      SUBROUTINE TRI(CLEF,TAB,NTAB,N)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 02/04/2002   AUTEUR RATEAU G.RATEAU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C A_UTIL
C ----------------------------------------------------------------------
C                     TRI RAPIDE (HOARE / SEDGEWICK)
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE / SORTIE
C INTEGER CLEF(N)         : VECTEUR CLEF
C INTEGER TAB(N,NTAB)     : TABLEAU A TRIER EN MEME TEMPS QUE CLEF
C                           (SI NTAB = 0, PAS PRIS EN COMPTE)
C
C VARIABLES D'ENTREE
C INTEGER NTAB            : NOMBRE DE COLONNES DE TAB
C INTEGER N               : NOMBRE DE LIGNES A TRIER
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- PARAMETRES
      INTEGER BLOCMX, NPILE
      PARAMETER (BLOCMX = 14)
      PARAMETER (NPILE = 19)

C --- VARIABLES
      INTEGER N,NTAB,CLEF(*),TAB(N,*)
      INTEGER PILE(NPILE+1),G,D,GS,DS,M,IPILE

C --- INITIALISATION

      IF (N.LE.BLOCMX) GOTO 20

      G = 1
      D = N
      IPILE = 1

 10   CONTINUE

C --- DECOUPAGE

      CALL TRIRAP(CLEF,TAB,NTAB,N,G,D,M)
      
      IF ((M-G).LT.(D-M)) THEN
        GS = G
        DS = M - 1
        G  = M + 1
      ELSE
        GS = M + 1
        DS = D
        D  = M - 1
      ENDIF

      IF ((D-G).GE.BLOCMX) THEN
        
C ----- PUSH

        IF ((DS-GS).GE.BLOCMX) THEN

          IF (IPILE.LE.NPILE) THEN
             
            PILE(IPILE) = GS
            IPILE = IPILE + 1
            PILE(IPILE) = DS
            IPILE = IPILE + 1

          ELSE

            CALL UTMESS('A','TRI','AUGMENTER LA TAILLE DE LA PILE')

          ENDIF
          
        ENDIF

        GOTO 10

      ELSE

C ----- POP

        IF (IPILE.GT.2) THEN

          IPILE = IPILE - 1
          D = PILE(IPILE)
          IPILE = IPILE - 1 
          G = PILE(IPILE)
          GOTO 10

        ENDIF

      ENDIF

C --- TRI PAR INSERTION

 20   CONTINUE

      CALL TRIINS(CLEF,TAB,NTAB,N)

      END
