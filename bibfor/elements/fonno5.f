      SUBROUTINE FONNO5 (NOMA,INDIC,NBNOFF,NOE,NA,NB,NDIM,
     %                   NBNOEL,INDR,VNOR,VDIR)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      CHARACTER*8     NOMA
      INTEGER         INDIC(4),NBNOFF,NOE(4,4),NA,NB,NDIM,NBNOEL,INDR(2)
      REAL*8          VNOR(2,3),VDIR(2,3)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C     BUT : CALCUL DES VECTEURS DE LA BASE LOCALE : 
C             - VNOR : VECTEUR NORMAL A LA SURFACE DE LA FISSURE
C             - VDIR : VECTEUR DANS LA DIRECTION DE PROPAGATION

C           RQ : CHACUN CONTIENT EN FAIT 2 VECTEURS (UN PAR LEVRE)
C     ------------------------------------------------------------------
C
C ENTREES
C     NOMA   : NOM DU MAILLAGE
C     INDIC  : INDICE DES FACES INTERNES
C     NBNOFF : NOMBRE DE NOEUD EN FOND DE FISSURE
C     NOE    : NOEUDS DES FACES CONTENANT NA et NB ET APPARTENANT AUX
C              MAILLES CONNECTEES AU NOEUD SOMMET COURANT 
C              ET AUX LEVRES
C     NA     : NUMERO DU NOEUD SOMMET COURANT
C     NB     : NUMERO DU NOEUD SOMMET SUIVANT
C
C SORTIES
C     NBNOEL : NOMBRE DE NOEUDS SOMMETS PAR ELEMENTS
C     INDR   : INDICES DES FACES LIBRES
C     VNOR   : VECTEUR NORMAL A LA SURFACE DE LA FISSURE
C     VDIR   : VECTEUR DANS LA DIRECTION DE PROPAGATION
C     
C     ----------------------------------------------------
C
      INTEGER     JCOOR,COMPTE
      INTEGER     INDICE,INP,INO1,INO2,ICO
      INTEGER     M(8)
      REAL*8      VECT1(3),VECT2(3),VECT3(3),VECT4(3),NORM1
      REAL*8      COORD(3,4)

C     -----------------------------------------------------------------
C           
      CALL JEMARQ() 
C
C     RECUPERATION DE L'ADRESSE DES COORDONNEES
      CALL JEVEUO ( NOMA//'.COORDO    .VALE', 'L', JCOOR )

      COMPTE = 0
      INDICE = 0 
      DO 150 INP=1,4
        IF ((INP.NE.INDIC(1)).AND.(INP.NE.INDIC(2)).AND.
     &         (INP.NE.INDIC(3)).AND.(INP.NE.INDIC(4))) THEN
          COMPTE=COMPTE+1
C         RENUMEROTATION LOCALE POUR AVOIR DANS LES DEUX PREMIERS 
C         NOEUDS LES NOEUDS DU FOND DE FISSURE
C         CELA PERMET DEFINIR LE VECTEUR NORMAL A L'AIDE DE GDIRE3
          IF (NDIM.EQ.3) THEN
            IF (NOE(INP,4).EQ.0) THEN
              NBNOEL = 3
              INDICE = 1
            ELSE
              NBNOEL = 4
              INDICE = 2
            ENDIF
            DO 151 INO1=1,NBNOEL
              M(INO1)        = NOE(INP,INO1)
              M(INO1+NBNOEL) = NOE(INP,INO1)
 151        CONTINUE
            DO 152 INO1=1,NBNOEL
              IF (M(INO1).EQ.NA) THEN
                IF (M(INO1+1).EQ.NB) THEN
                  DO 153 INO2=1,NBNOEL
                    NOE(INP,INO2) = M(INO1-1+INO2)
 153              CONTINUE
                ELSE
                  DO 154 INO2=1,NBNOEL
                    NOE(INP,INO2) = M(INO1+1+NBNOEL-INO2)
 154              CONTINUE
                ENDIF
              ENDIF       
 152        CONTINUE
            DO 155 ICO=1,3
              DO 156 INO1=1,NBNOEL
                COORD(ICO,INO1) = ZR(JCOOR-1 + 
     &                          (NOE(INP,INO1)-1)*3+ICO)
 156          CONTINUE
              VECT1(ICO) = COORD(ICO,2) - COORD(ICO,1)
 155        CONTINUE
            CALL NORMEV(VECT1,NORM1)
C           CALCUL DU VECTEUR DIRECTION DE PROPAGATION
            CALL GDIRE3(COORD,VECT4(1),VECT4(2),VECT4(3),INDICE)

C           CALCUL DU VECTEUR NORMAL A LA FACE
            CALL PROVEC(VECT4,VECT1,VECT3)
            CALL NORMEV(VECT3,NORM1)
            DO 157 ICO=1,3
              VNOR(COMPTE,ICO) = VECT3(ICO)
              VDIR(COMPTE,ICO) = VECT4(ICO)
 157        CONTINUE
          ELSE
C           LE NOEUD DU FOND DOIT ETRE LE PREMIER
            NBNOEL = 2 
            M(1) = NOE(INP,1)
            M(2) = NOE(INP,2)
            IF (M(1).NE.NA) THEN
              NOE(INP,1) = M(2)
              NOE(INP,2) = M(1)
            ENDIF
C           CALCUL DU VECTEUR DIRECTION DE PROPAGATION
            DO 158 ICO=1,3
              DO 159 INO1=1,2
                COORD(ICO,INO1) = ZR(JCOOR-1 + 
     &                   (NOE(INP,INO1)-1)*3+ICO)
 159          CONTINUE
              VECT1(ICO) = COORD(ICO,1) - COORD(ICO,2)
 158        CONTINUE
            CALL NORMEV(VECT1,NORM1)
C           CALCUL DU VECTEUR NORMAL A L'ARETE
            CALL VECINI(3,0.D0,VECT3)
            VECT3(3)=1.D0
            CALL PROVEC(VECT3,VECT1,VECT2)
            DO 160 ICO=1,3
              VNOR(COMPTE,ICO) = VECT2(ICO)
              VDIR(COMPTE,ICO) = VECT1(ICO)
 160        CONTINUE
          ENDIF      
          INDR(COMPTE) = INP      
        ENDIF
 150  CONTINUE
      CALL JEDEMA()
      END
