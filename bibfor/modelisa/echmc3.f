      SUBROUTINE ECHMC3(NSOM  ,NOEARE,NARE  ,NOEPAN,NPAN  ,
     &                  NECH  ,OFFSOM,FS    ,NF)
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
      INTEGER     NSOM
      INTEGER     NOEARE(*)    
      INTEGER     NARE   
      INTEGER     NOEPAN(*)    
      INTEGER     NPAN       
      INTEGER     NECH  
      INTEGER     OFFSOM
      INTEGER     FS(3,*) 
      INTEGER     NF
C      
C ----------------------------------------------------------------------
C
C APPARIEMENT DE DEUX GROUPES DE MAILLE PAR LA METHODE
C BOITES ENGLOBANTES + ARBRE BSP
C
C CONNECTIVITE DE L'ECHANTILLONNAGE PRODUIT PAR LA ROUTINE ECHMAP (3D)
C
C ----------------------------------------------------------------------
C   
C CHAQUE FACE(PAN) EST DECOUPEE EN FACETTES         
C
C IN  NSOM   : NOMBRE DE SOMMETS SANS LES POINTS D'ECHANTILLONNAGE
C IN  NOEARE : NOEUDS DEFINISSANT LES ARETES DE LA MAILLE 
C              VOIR NOARE/NBARE
C                   ( NOMBRE DE NOEUDS ARETES 1, N1, N2, ...
C                     NOMBRE DE NOEUDS ARETES 2, N1, N2, ...)
C              INDICE DES NOEUDS: ORDRE DU NOEUD DANS LA MAILLE 
C                                 (EX: POUR HEXA8 NOEUDS 1 A 8)
C IN  NARE   : NOMBRE D'ARETES
C IN  NOEPAN : NOEUDS DEFINISSANT LES FACES DE LA MAILLE
C              VOIR NOPAN/NBPAN
C                   ( NOMBRE NOEUDS PAN       1, N1, N2, ...
C                     NOMBRE NOEUDS PAN       2, N1, N2, ...)
C                     EN 3D, NB NOEUDS < 0 : TRIANGLE
C                            NB NOEUDS > 0 : QUADRANGLE
C              INDICE DES NOEUDS: ORDRE DU NOEUD DANS LA MAILLE 
C                                 (EX: POUR HEXA8 NOEUDS 1 A 8)
C IN  NPAN   : NOMBRE DE FACES
C IN  NECH   : NOMBRE DE POINTS D'ECHANTILLONNAGE 
C IN  OFFSOM : DECALAGE INDICE DANS LE TABLEAU DES SOMMETS
C OUT NF     : NOMBRE DE FACETTES NF = 2*NPAN*NECH*NECH
C OUT FS     : NOEUDS DEFINISSANT LES FACETTES TRIANGULAIRES
C                (FACE1.ND1,FACE1.ND2,FACE1.ND3,
C                 FACE2.ND1,FACE2.ND2,FACE2.ND3,...)
C              INDICE DES NOEUDS: SE REFERE AU TABLEAU NOH DANS ECHMAP
C              NB: ON DECOUPE EN TRIANGLES UNIQUEMENT ! (TJS 3 NOEUDS)
C 
C ----------------------------------------------------------------------
C
      INTEGER     NNP,NA,I1,I2,I3,I4,N0,N1,N2,N3,N4,N5,N6,N7,N8,N9
      INTEGER     M0,M1,M2,M3,M4,I,J,K,P0,P1
C
C ----------------------------------------------------------------------
C
      P0 = 1
      P1 = 1
      NF = 0
C
      N0 = NSOM + 1 + OFFSOM
      M0 = N0 + (NECH-1)*NARE
C     
      DO 10 I = 1, NPAN
        NNP = NOEPAN(P0)
        P0  = P0 + 1
C
C --- FACE TRIANGULAIRE
C          
        IF (NNP.LT.0) THEN
          NA = 4
C
C --- CAS NECH = 1
C
          IF (NECH.EQ.1) THEN
            NF = NF + 1
            FS(1,NF) = NOEPAN(P0)   + OFFSOM
            FS(2,NF) = NOEPAN(P0+1) + OFFSOM
            FS(3,NF) = NOEPAN(P0+2) + OFFSOM  
            GOTO 60
          ENDIF
C
C --- POINTS D'ECHANTILLONNAGE
C
          IF (NOEARE(P1+1).GT.0) THEN
            I1 = 1
            N1 = N0 + (NECH-1)*(NOEARE(P1+1)-1)
            N2 = N1 + NECH - 3
            N3 = N2 + 1
          ELSE
            I1 = -1
            N1 = N0 - (NECH-1)*NOEARE(P1+1) - 1
            N2 = N1 + 3 - NECH
            N3 = N2 - 1
          ENDIF
C
          IF (NOEARE(P1+2).GT.0) THEN
            I2 = 1
            N4 = N0 + (NECH-1)*(NOEARE(P1+2)-1)
            N5 = N4 + NECH - 3
            N6 = N5 + 1
          ELSE
            I2 = -1
            N4 = N0 - (NECH-1)*NOEARE(P1+2) - 1
            N5 = N4 + 3 - NECH
            N6 = N5 - 1
          ENDIF
C
          IF (NOEARE(P1+3).GT.0) THEN
            I3 = 1
            N7 = N0 + (NECH-1)*(NOEARE(P1+3)-1)
            N8 = N7 + 1
            N9 = N8 + NECH - 3
          ELSE
            I3 = -1
            N7 = N0 - (NECH-1)*NOEARE(P1+3) - 1
            N8 = N7 - 1
            N9 = N8 + 3 - NECH
          ENDIF
C
C --- CAS NECH = 2
C
          IF (NECH.EQ.2) THEN
            NF = NF + 1
            FS(1,NF) = NOEPAN(P0) + OFFSOM
            FS(2,NF) = N1
            FS(3,NF) = N9  
            NF = NF + 1
            FS(1,NF) = N3
            FS(2,NF) = NOEPAN(P0+1) + OFFSOM
            FS(3,NF) = N4
            NF = NF + 1
            FS(1,NF) = N7
            FS(2,NF) = N6
            FS(3,NF) = NOEPAN(P0+2) + OFFSOM
            NF = NF + 1
            FS(1,NF) = N1
            FS(2,NF) = N4
            FS(3,NF) = N7
            GOTO 60
          ENDIF
C
C --- CAS GENERAL
C
          M1 = M0
          M2 = M0 + NECH - 3
          M3 = M0 - 1 + (NECH-1)*(NECH-2)/2 
C
C --- MAILLES PARTICULIERES
C
          NF = NF + 1
          FS(1,NF) = NOEPAN(P0) + OFFSOM
          FS(2,NF) = N1
          FS(3,NF) = N9
          NF = NF + 1
          FS(1,NF) = N1
          FS(2,NF) = M1
          FS(3,NF) = N9
          NF = NF + 1
          FS(1,NF) = N2
          FS(2,NF) = N3
          FS(3,NF) = M2
          NF = NF + 1
          FS(1,NF) = N3
          FS(2,NF) = N4
          FS(3,NF) = M2
          NF = NF + 1
          FS(1,NF) = N3
          FS(2,NF) = NOEPAN(P0+1) + OFFSOM
          FS(3,NF) = N4
          NF = NF + 1
          FS(1,NF) = M3
          FS(2,NF) = N5
          FS(3,NF) = N6
          NF = NF + 1
          FS(1,NF) = N7
          FS(2,NF) = N6
          FS(3,NF) = NOEPAN(P0+2) + OFFSOM
          NF = NF + 1
          FS(1,NF) = N8
          FS(2,NF) = M3
          FS(3,NF) = N7
          NF = NF + 1
          FS(1,NF) = M3
          FS(2,NF) = N6
          FS(3,NF) = N7
C
C --- MAILLES LIEES AUX ARETES
C
          DO 20 J = 4, NECH
            NF = NF + 1
            FS(1,NF) = N1
            FS(2,NF) = N1 + I1
            FS(3,NF) = M1
            NF = NF + 1
            FS(1,NF) = N1 + I1
            FS(2,NF) = M1 + 1
            FS(3,NF) = M1
            NF = NF + 1
            FS(1,NF) = M2
            FS(2,NF) = N4
            FS(3,NF) = N4 + I2
            NF = NF + 1
            FS(1,NF) = M2 - 1
            FS(2,NF) = M2
            FS(3,NF) = M2 + NECH + 1 - J
            NF = NF + 1
            FS(1,NF) = M2
            FS(2,NF) = N4 + I2
            FS(3,NF) = M2 + NECH + 1 - J
            NF = NF + 1
            FS(1,NF) = N8 + I3
            FS(2,NF) = M3 + 2 - J
            FS(3,NF) = N8
            NF = NF + 1
            FS(1,NF) = M3 + 2 - J
            FS(2,NF) = M3
            FS(3,NF) = N8
            N1 = N1 + I1
            N4 = N4 + I2
            N8 = N8 + I3
            M1 = M1 + 1
            M2 = M2 + NECH + 1 - J
            M3 = M3 + 2 - J
 20       CONTINUE
C
C --- MAILLES INTERIEURES DE LA FACETTE DE LA FRONTIERE
C
          M0 = M0 - 2
          DO 30 J = 5, NECH           
            M0 = M0 + 2
            DO 30 K = J, NECH
              NF = NF + 1
              FS(1,NF) = M0
              FS(2,NF) = M0 + 1
              FS(3,NF) = M0 + NECH - J + 3
              NF = NF + 1
              FS(1,NF) = M0 + 1
              FS(2,NF) = M0 + NECH - J + 4
              FS(3,NF) = M0 + NECH - J + 3
              M0 = M0 + 1      
 30       CONTINUE
C
          IF (NECH.EQ.3) THEN
            M0 = M0 + 3
          ELSE
            M0 = M0 + 5
          ENDIF
C
C --- FACE QUADRANGULAIRE
C
        ELSE

          NA = 5
C
C --- CAS NECH = 1
C
          IF (NECH.EQ.1) THEN
            NF = NF + 1
            FS(1,NF) = NOEPAN(P0) + OFFSOM
            FS(2,NF) = NOEPAN(P0+1) + OFFSOM
            FS(3,NF) = NOEPAN(P0+3) + OFFSOM       
            NF = NF + 1
            FS(1,NF) = NOEPAN(P0+1) + OFFSOM
            FS(2,NF) = NOEPAN(P0+2) + OFFSOM
            FS(3,NF) = NOEPAN(P0+3) + OFFSOM
            GOTO 60
          ENDIF
C
          IF (NOEARE(P1+1).GT.0) THEN
            I1 = 1
            N1 = N0 + (NECH-1)*(NOEARE(P1+1)-1)
            N2 = N1 + NECH - 2
          ELSE
            I1 = -1
            N1 = N0 - (NECH-1)*NOEARE(P1+1) - 1
            N2 = N1 + 2 - NECH
          ENDIF
C
          IF (NOEARE(P1+2).GT.0) THEN
            I2 = 1
            N3 = N0 + (NECH-1)*(NOEARE(P1+2)-1)
            N4 = N3 + NECH - 2
          ELSE
            I2 = -1
            N3 = N0 - (NECH-1)*NOEARE(P1+2) - 1
            N4 = N3 + 2 - NECH
          ENDIF
C
          IF (NOEARE(P1+3).GT.0) THEN
            I3 = 1
            N5 = N0 + (NECH-1)*(NOEARE(P1+3)-1)
            N6 = N5 + NECH - 2
          ELSE
            I3 = -1
            N5 = N0 - (NECH-1)*NOEARE(P1+3) - 1
            N6 = N5 + 2 - NECH
          ENDIF
C
          IF (NOEARE(P1+4).GT.0) THEN
            I4 = 1
            N7 = N0 + (NECH-1)*(NOEARE(P1+4)-1)
            N8 = N7 + NECH - 2
          ELSE
            I4 = -1
            N7 = N0 - (NECH-1)*NOEARE(P1+4) - 1
            N8 = N7 + 2 - NECH
          ENDIF
C
          M1 = M0
          M2 = M0 + NECH - 2
          M3 = M0 + NECH*(NECH-2)
          M4 = M3 + 2 - NECH
C
C --- MAILLES PARTICULIERES
C
          NF = NF + 1
          FS(1,NF) = NOEPAN(P0) + OFFSOM
          FS(2,NF) = N1
          FS(3,NF) = N8
          NF = NF + 1
          FS(1,NF) = N1
          FS(2,NF) = M1
          FS(3,NF) = N8
          NF = NF + 1
          FS(1,NF) = N2
          FS(2,NF) = NOEPAN(P0+1) + OFFSOM
          FS(3,NF) = M2
          NF = NF + 1
          FS(1,NF) = NOEPAN(P0+1) + OFFSOM
          FS(2,NF) = N3
          FS(3,NF) = M2
          NF = NF + 1
          FS(1,NF) = M3   
          FS(2,NF) = N4
          FS(3,NF) = N5
          NF = NF + 1
          FS(1,NF) = N4
          FS(2,NF) = NOEPAN(P0+2) + OFFSOM
          FS(3,NF) = N5
          NF = NF + 1
          FS(1,NF) = N7 
          FS(2,NF) = M4
          FS(3,NF) = NOEPAN(P0+3) + OFFSOM
          NF = NF + 1
          FS(1,NF) = M4
          FS(2,NF) = N6
          FS(3,NF) = NOEPAN(P0+3) + OFFSOM
C  
C --- MAILLES LIEES AUX ARETES
C           
          DO 40 J = 3, NECH         
            NF = NF + 1
            FS(1,NF) = N1
            FS(2,NF) = N1 + I1
            FS(3,NF) = M1
            NF = NF + 1
            FS(1,NF) = N1 + I1
            FS(2,NF) = M1 + 1
            FS(3,NF) = M1
            NF = NF + 1
            FS(1,NF) = M2
            FS(2,NF) = N3
            FS(3,NF) = M2 + NECH - 1
            NF = NF + 1
            FS(1,NF) = N3
            FS(2,NF) = N3 + I2
            FS(3,NF) = M2 + NECH - 1          
            NF = NF + 1
            FS(1,NF) = M3 - 1
            FS(2,NF) = M3
            FS(3,NF) = N5 + I3
            NF = NF + 1
            FS(1,NF) = M3
            FS(2,NF) = N5
            FS(3,NF) = N5 + I3             
            NF = NF + 1
            FS(1,NF) = N7 + I4
            FS(2,NF) = M4 + 1 - NECH
            FS(3,NF) = N7
            NF = NF + 1
            FS(1,NF) = M4 + 1 - NECH
            FS(2,NF) = M4
            FS(3,NF) = N7            
            N1 = N1 + I1
            N3 = N3 + I2
            N5 = N5 + I3
            N7 = N7 + I4
            M1 = M1 + 1
            M2 = M2 + NECH - 1
            M3 = M3 - 1
            M4 = M4 + 1 - NECH
 40       CONTINUE
C
C --- MAILLES INTERIEURES
C
          M0 = M0 - 1
          DO 50 J = 3, NECH
            M0 = M0 + 1           
            DO 50 K = 3, NECH            
              NF = NF + 1
              FS(1,NF) = M0
              FS(2,NF) = M0 + 1
              FS(3,NF) = M0 + NECH - 1
              NF = NF + 1
              FS(1,NF) = M0 + 1
              FS(2,NF) = M0 + NECH
              FS(3,NF) = M0 + NECH - 1
              M0 = M0 + 1             
 50       CONTINUE
          M0 = M0 + NECH
        ENDIF
 60     CONTINUE
        P0 = P0 + ABS(NNP)
        P1 = P1 + NA
 10   CONTINUE
C
      NF = NF
C
      END
