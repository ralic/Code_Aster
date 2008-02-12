      SUBROUTINE ECHMAP(NOMMAI,TYPEMA,DIME  ,CSOM  ,NSOM  ,
     &                  NOEARE,NARE  ,NOEPAN,NPAN  ,NECH  ,
     &                  NOH   ,NNOH)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 12/02/2008   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*8 NOMMAI,TYPEMA       
      INTEGER     DIME
      REAL*8      CSOM(*)
      INTEGER     NSOM
      INTEGER     NOEARE(*)
      INTEGER     NARE
      INTEGER     NOEPAN(*)
      INTEGER     NPAN
      INTEGER     NECH 
      REAL*8      NOH(*)
      INTEGER     NNOH    
C      
C ----------------------------------------------------------------------
C
C APPARIEMENT DE DEUX GROUPES DE MAILLE PAR LA METHODE
C BOITES ENGLOBANTES + ARBRE BSP
C
C ECHANTILLONNAGE PONCTUEL D'UNE FRONTIERE DE MAILLE 
C COORDONNEES DES POINTS AJOUTES 
C
C ----------------------------------------------------------------------
C
C
C IN  NOMMAI : NOM DE LA MAILLE
C IN  TYPEMA : TYPE DE LA MAILLE
C IN  DIME   : DIMENSION DE L'ESPACE
C IN  CSOM   : COORDONNEES DES SOMMETS 
C IN  NSOM   : NOMBRE DE SOMMETS (SANS LES POINTS D'ECHANTILLONNAGE)
C IN  NOEARE : CONNECTIVITE DES ARETES
C              ( NOMBRE DE NOEUDS ARETES 1, N1, N2, ...
C                NOMBRE DE NOEUDS ARETES 2, ... )
C IN  NARE   : NOMBRE D'ARETES
C IN  NOEPAN : (3D) CONNECTIVITE DES PANS
C              ( NOMBRE NOEUDS NOEPAN 1, N1, N2, ...
C                NOMBRE NOEUDS NOEPAN 2, ...)
C                     EN 3D, NB NOEUDS < 0 : LA FACE EST UN TRIANGLE
C                            NB NOEUDS > 0 : LA FACE EST UN QUADRANGLE
C IN  NPAN   : NOMBRE DE PANS
C IN  NECH   : NOMBRE DE POINTS D'ECHANTILLONNAGE 
C OUT NOH    : COORD. POINTS ECHANTILLONNANT FRONTIERE 
C                COORD. SOMMETS ORIGINAUX
C                COORD. POINTS ECHNATILLONS
C                DIME: (DIME,NNOH)
C OUT NNOH   : NOMBRE DE POINTS D'ECHANTILLONNAGE (Y COMPRIS
C                SOMMETS ORIGINAUX !)
C                DIME (2D) : NARE*(NHAPP+1)
C                DIME (3D) : NPAN*(NHAPP**2+NHAPP+1) 
C
C ----------------------------------------------------------------------
C
      CHARACTER*8 TMAP
      INTEGER     NNP
      INTEGER     P0,Q0,J,K
      REAL*8      M0(2),W0(9),HQ,HT
      INTEGER     IARE,IPAN,IECH
      INTEGER     IFM,NIV
C
C ----------------------------------------------------------------------
C
      CALL INFDBG('ARLEQUIN',IFM,NIV)
C 
C --- AFFICHAGE
C
      IF (NIV.GE.2) THEN
        WRITE(IFM,*) '<ARLEQUIN><ECH> *** ECHANTILLONNAGE '//
     &                'DE LA FRONTIERE DE LA MAILLE ',NOMMAI
        WRITE(IFM,*) '<ARLEQUIN><ECH> ... TYPE MAILLE         : ',
     &                 TYPEMA
        WRITE(IFM,*) '<ARLEQUIN><ECH> ... NBRE ECHANTILLONS   : ',
     &                 NECH
        WRITE(IFM,*) '<ARLEQUIN><ECH> ... DIMENSION  MAILLE   : ',
     &                 DIME
        WRITE(IFM,*) '<ARLEQUIN><ECH> ... NBRE SOMMETS MAILLE : ',
     &                 NSOM
        WRITE(IFM,*) '<ARLEQUIN><ECH> ... NBRE ARETES  MAILLE : ',
     &                 NARE
        IF (DIME.EQ.3) THEN
          WRITE(IFM,*) '<ARLEQUIN><ECH> ... NBRE PANS    MAILLE : ',
     &                   NPAN
        ENDIF
      ENDIF
C
C --- TAILLE DES ECHANTILLONS
C                
      IF (NECH.LT.1) THEN
        CALL ASSERT(.FALSE.)
      ELSE  
        HQ = 2.D0/NECH 
        HT = 1.D0/NECH 
      ENDIF
C 
C --- ON COMMENCE PAR RECOPIER LES COORDONNEES DES SOMMETS ORIGINAUX
C     
      CALL DCOPY(DIME*NSOM,CSOM,1,NOH,1)
C
C --- DECALAGE POUR STOCKER LES POINTS D'ECHANTILLONNAGE APRES LES 
C --- "VRAIS" SOMMETS
C       
      Q0 = NSOM*DIME + 1
C
C --- NOMBRE DE SOMMETS+NOMBRE POINTS ECHANTILLONNAGE
C
      NNOH = NSOM + NARE*(NECH - 1)
      IF (NIV.GE.2) THEN    
        WRITE(IFM,*) '<ARLEQUIN><ECH> ... NBRE DE POINTS '//
     &               'SUPPL. SUR ARETES  : ',NARE*(NECH - 1)
      ENDIF
C
C --- ECHANTILLONNAGE DES ARETES
C      
      P0   = 1    
      DO 10 IARE = 1, NARE
        NNP = NOEARE(P0)
        P0  = P0 + 1
        IF (NNP.EQ.2) THEN
          TMAP = 'SEG2'
        ELSEIF (NNP.EQ.3) THEN
          TMAP = 'SEG3'
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
        M0(1) = -1.D0
        DO 20 J = 2, NECH 
          M0(1) = M0(1) + HQ
          CALL FORME0(M0,TMAP,W0,NNP)
          IF (NNP.GT.9) THEN
            CALL ASSERT(.FALSE.)
          ENDIF
          CALL MMPROD(CSOM  ,DIME  ,0     ,DIME  ,NOEARE(P0),
     &                NNP   ,W0    ,NNP   ,0     ,0         ,
     &                1     ,NOH(Q0))
          Q0 = Q0 + DIME      
 20     CONTINUE
        P0 = P0 + NNP
 10   CONTINUE

      IF (DIME.EQ.2) GOTO 60
C
C --- ECHANTILLONNAGE DES FACES (3D)
C
      
      P0 = 1      
      DO 30 IPAN = 1, NPAN
        NNP = NOEPAN(P0)
        P0  = P0 + 1
        IF (NNP.LT.0) THEN 
          IF (NNP.EQ.-3) THEN
            TMAP = 'TRIA3'
          ELSEIF (NNP.EQ.-6) THEN
            TMAP = 'TRIA6'
          ELSEIF (NNP.EQ.-7) THEN
            TMAP = 'TRIA7'
          ELSE
            CALL ASSERT(.FALSE.)  
          ENDIF
          NNOH = NNOH + (NECH - 1)*(NECH - 2)/2
          M0(2) = 0.D0       
          DO 40 J = 3, NECH 
            M0(2) = M0(2) + HT
            M0(1) = 0.D0
            DO 41 K = J, NECH  
              M0(1) = M0(1) + HT
              CALL FORME0(M0,TMAP,W0,NNP)
              IF (NNP.GT.9) THEN
                CALL ASSERT(.FALSE.)
              ENDIF
              CALL MMPROD(CSOM,3,0,3,NOEPAN(P0),
     &                    NNP ,W0,NNP,0,0,1,NOH(Q0))
              Q0 = Q0 + 3
 41         CONTINUE 
 40       CONTINUE
        ELSE
          IF (NNP.EQ.4) THEN
            TMAP = 'QUAD4'
          ELSEIF (NNP.EQ.6) THEN
            TMAP = 'QUAD6'
          ELSEIF (NNP.EQ.8) THEN
            TMAP = 'QUAD8'
          ELSEIF (NNP.EQ.9) THEN
            TMAP = 'QUAD9'
          ELSE
            CALL ASSERT(.FALSE.)              
          ENDIF
          NNOH = NNOH + (NECH-1)*(NECH-1)
          M0(2) = -1.D0   
          DO 50 J = 2, NECH
            M0(2) = M0(2) + HQ
            M0(1) = -1.D0
            DO 51 K = 2, NECH 
              M0(1) = M0(1) + HQ
              CALL FORME0(M0,TMAP,W0,NNP)
              IF (NNP.GT.9) THEN
                CALL ASSERT(.FALSE.)
              ENDIF
              CALL MMPROD(CSOM,3,0,3,NOEPAN(P0),
     &                    NNP   ,W0,NNP,0,0,1,NOH(Q0))
              Q0 = Q0 + 3
 51         CONTINUE
 50       CONTINUE
        ENDIF
        P0 = P0 + ABS(NNP)        
 30   CONTINUE
C      
 60   CONTINUE 
C 
      IF (NIV.GE.2) THEN
        WRITE(IFM,*) '<ARLEQUIN><ECH> ... LISTE DES POINTS '  
        DO 300 IECH = 1, NNOH
          IF (DIME.EQ.2) THEN
            WRITE(IFM,*) '<ARLEQUIN><ECH> ...... POINT  <',IECH,'>  : ',
     &        NOH(2*(IECH-1) + 1),
     &        NOH(2*(IECH-1) + 2)
          ELSE
            WRITE(IFM,*) '<ARLEQUIN><ECH> ...... POINT  <',IECH,'>  : ',
     &        NOH(3*(IECH-1) + 1),
     &        NOH(3*(IECH-1) + 2),
     &        NOH(3*(IECH-1) + 3)         
          ENDIF                 
 300    CONTINUE             
        WRITE(IFM,*) '<ARLEQUIN><ECH> *** FIN ECHANTILLONNAGE '       
      ENDIF

C
      END
