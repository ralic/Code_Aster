      SUBROUTINE ECHMC3(NSOM  ,NOEARE,NARE  ,NOEPAN,NPAN  ,
     &                  NH    ,OS    ,FS    ,NF)
C      
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/01/2007   AUTEUR ABBAS M.ABBAS 
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
      INTEGER     NH  
      INTEGER     OS 
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
C
C IN  NSOM   : NOMBRE DE SOMMETS
C IN  NOEARE : CONNECTIVITE DES ARETES (CF NOARET)
C IN  NARE   : NOMBRE D'ARETES
C IN  NOEPAN : CONNECTIVITE DES FACES (CF NOPAN)
C IN  NPAN   : NOMBRE DE FACES
C IN  NH     : NOMBRE D'ECHANTILLONNAGE 
C IN  OS     : OFFSET SOMMET
C OUT FS     : CONNECTIVITE DES FACES TRIANGULAIRES
C OUT NF     : NOMBRE DE FACETTES NF = 2*NPAN*NH**2
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
      N0 = NSOM + 1 + OS
      M0 = N0 + (NH-1)*NARE
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
C --- CAS NH = 1
C
          IF (NH.EQ.1) THEN
            NF = NF + 1
            FS(1,NF) = NOEPAN(P0) + OS
            FS(2,NF) = NOEPAN(P0+1) + OS
            FS(3,NF) = NOEPAN(P0+2) + OS  
            GOTO 60
          ENDIF
C
C --- POINTS D'ECHANTILLONNAGE
C
          IF (NOEARE(P1+1).GT.0) THEN
            I1 = 1
            N1 = N0 + (NH-1)*(NOEARE(P1+1)-1)
            N2 = N1 + NH - 3
            N3 = N2 + 1
          ELSE
            I1 = -1
            N1 = N0 - (NH-1)*NOEARE(P1+1) - 1
            N2 = N1 + 3 - NH
            N3 = N2 - 1
          ENDIF
C
          IF (NOEARE(P1+2).GT.0) THEN
            I2 = 1
            N4 = N0 + (NH-1)*(NOEARE(P1+2)-1)
            N5 = N4 + NH - 3
            N6 = N5 + 1
          ELSE
            I2 = -1
            N4 = N0 - (NH-1)*NOEARE(P1+2) - 1
            N5 = N4 + 3 - NH
            N6 = N5 - 1
          ENDIF
C
          IF (NOEARE(P1+3).GT.0) THEN
            I3 = 1
            N7 = N0 + (NH-1)*(NOEARE(P1+3)-1)
            N8 = N7 + 1
            N9 = N8 + NH - 3
          ELSE
            I3 = -1
            N7 = N0 - (NH-1)*NOEARE(P1+3) - 1
            N8 = N7 - 1
            N9 = N8 + 3 - NH
          ENDIF
C
C --- CAS NH = 2
C
          IF (NH.EQ.2) THEN
            NF = NF + 1
            FS(1,NF) = NOEPAN(P0) + OS
            FS(2,NF) = N1
            FS(3,NF) = N9  
            NF = NF + 1
            FS(1,NF) = N3
            FS(2,NF) = NOEPAN(P0+1) + OS
            FS(3,NF) = N4
            NF = NF + 1
            FS(1,NF) = N7
            FS(2,NF) = N6
            FS(3,NF) = NOEPAN(P0+2) + OS
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
          M2 = M0 + NH - 3
          M3 = M0 - 1 + (NH-1)*(NH-2)/2 
C
C --- MAILLES PARTICULIERES
C
          NF = NF + 1
          FS(1,NF) = NOEPAN(P0) + OS
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
          FS(2,NF) = NOEPAN(P0+1) + OS
          FS(3,NF) = N4
          NF = NF + 1
          FS(1,NF) = M3
          FS(2,NF) = N5
          FS(3,NF) = N6
          NF = NF + 1
          FS(1,NF) = N7
          FS(2,NF) = N6
          FS(3,NF) = NOEPAN(P0+2) + OS
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
          DO 20 J = 4, NH
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
            FS(3,NF) = M2 + NH + 1 - J
            NF = NF + 1
            FS(1,NF) = M2
            FS(2,NF) = N4 + I2
            FS(3,NF) = M2 + NH + 1 - J
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
            M2 = M2 + NH + 1 - J
            M3 = M3 + 2 - J
 20       CONTINUE
C
C --- MAILLES INTERIEURES DE LA FACETTE DE LA FRONTIERE
C
          M0 = M0 - 2
          DO 30 J = 5, NH           
            M0 = M0 + 2
            DO 30 K = J, NH
              NF = NF + 1
              FS(1,NF) = M0
              FS(2,NF) = M0 + 1
              FS(3,NF) = M0 + NH - J + 3
              NF = NF + 1
              FS(1,NF) = M0 + 1
              FS(2,NF) = M0 + NH - J + 4
              FS(3,NF) = M0 + NH - J + 3
              M0 = M0 + 1      
 30       CONTINUE
C
          IF (NH.EQ.3) THEN
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
C --- CAS NH = 1
C
          IF (NH.EQ.1) THEN
            NF = NF + 1
            FS(1,NF) = NOEPAN(P0) + OS
            FS(2,NF) = NOEPAN(P0+1) + OS
            FS(3,NF) = NOEPAN(P0+3) + OS       
            NF = NF + 1
            FS(1,NF) = NOEPAN(P0+1) + OS
            FS(2,NF) = NOEPAN(P0+2) + OS
            FS(3,NF) = NOEPAN(P0+3) + OS
            GOTO 60
          ENDIF
C
          IF (NOEARE(P1+1).GT.0) THEN
            I1 = 1
            N1 = N0 + (NH-1)*(NOEARE(P1+1)-1)
            N2 = N1 + NH - 2
          ELSE
            I1 = -1
            N1 = N0 - (NH-1)*NOEARE(P1+1) - 1
            N2 = N1 + 2 - NH
          ENDIF
C
          IF (NOEARE(P1+2).GT.0) THEN
            I2 = 1
            N3 = N0 + (NH-1)*(NOEARE(P1+2)-1)
            N4 = N3 + NH - 2
          ELSE
            I2 = -1
            N3 = N0 - (NH-1)*NOEARE(P1+2) - 1
            N4 = N3 + 2 - NH
          ENDIF
C
          IF (NOEARE(P1+3).GT.0) THEN
            I3 = 1
            N5 = N0 + (NH-1)*(NOEARE(P1+3)-1)
            N6 = N5 + NH - 2
          ELSE
            I3 = -1
            N5 = N0 - (NH-1)*NOEARE(P1+3) - 1
            N6 = N5 + 2 - NH
          ENDIF
C
          IF (NOEARE(P1+4).GT.0) THEN
            I4 = 1
            N7 = N0 + (NH-1)*(NOEARE(P1+4)-1)
            N8 = N7 + NH - 2
          ELSE
            I4 = -1
            N7 = N0 - (NH-1)*NOEARE(P1+4) - 1
            N8 = N7 + 2 - NH
          ENDIF
C
          M1 = M0
          M2 = M0 + NH - 2
          M3 = M0 + NH*(NH-2)
          M4 = M3 + 2 - NH
C
C --- MAILLES PARTICULIERES
C
          NF = NF + 1
          FS(1,NF) = NOEPAN(P0) + OS
          FS(2,NF) = N1
          FS(3,NF) = N8
          NF = NF + 1
          FS(1,NF) = N1
          FS(2,NF) = M1
          FS(3,NF) = N8
          NF = NF + 1
          FS(1,NF) = N2
          FS(2,NF) = NOEPAN(P0+1) + OS
          FS(3,NF) = M2
          NF = NF + 1
          FS(1,NF) = NOEPAN(P0+1) + OS
          FS(2,NF) = N3
          FS(3,NF) = M2
          NF = NF + 1
          FS(1,NF) = M3   
          FS(2,NF) = N4
          FS(3,NF) = N5
          NF = NF + 1
          FS(1,NF) = N4
          FS(2,NF) = NOEPAN(P0+2) + OS
          FS(3,NF) = N5
          NF = NF + 1
          FS(1,NF) = N7 
          FS(2,NF) = M4
          FS(3,NF) = NOEPAN(P0+3) + OS
          NF = NF + 1
          FS(1,NF) = M4
          FS(2,NF) = N6
          FS(3,NF) = NOEPAN(P0+3) + OS
C  
C --- MAILLES LIEES AUX ARETES
C           
          DO 40 J = 3, NH         
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
            FS(3,NF) = M2 + NH - 1
            NF = NF + 1
            FS(1,NF) = N3
            FS(2,NF) = N3 + I2
            FS(3,NF) = M2 + NH - 1          
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
            FS(2,NF) = M4 + 1 - NH
            FS(3,NF) = N7
            NF = NF + 1
            FS(1,NF) = M4 + 1 - NH
            FS(2,NF) = M4
            FS(3,NF) = N7            
            N1 = N1 + I1
            N3 = N3 + I2
            N5 = N5 + I3
            N7 = N7 + I4
            M1 = M1 + 1
            M2 = M2 + NH - 1
            M3 = M3 - 1
            M4 = M4 + 1 - NH
 40       CONTINUE
C
C --- MAILLES INTERIEURES
C
          M0 = M0 - 1
          DO 50 J = 3, NH
            M0 = M0 + 1           
            DO 50 K = 3, NH            
              NF = NF + 1
              FS(1,NF) = M0
              FS(2,NF) = M0 + 1
              FS(3,NF) = M0 + NH - 1
              NF = NF + 1
              FS(1,NF) = M0 + 1
              FS(2,NF) = M0 + NH
              FS(3,NF) = M0 + NH - 1
              M0 = M0 + 1             
 50       CONTINUE
          M0 = M0 + NH
        ENDIF
 60     CONTINUE
        P0 = P0 + ABS(NNP)
        P1 = P1 + NA
 10   CONTINUE
C
      END
