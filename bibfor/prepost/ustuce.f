      SUBROUTINE USTUCE ( DIMTUB, VOLTUB, TUBUSE, RCRAY, RCARTE, SECT,
     &                    NS, EPAIS, SINIT, ECRAY, NOMCMD )
      IMPLICIT   NONE
      INTEGER             DIMTUB, NS
      REAL*8              VOLTUB(*), TUBUSE(*), RCRAY, RCARTE, SECT(*),
     &                    EPAIS, SINIT, ECRAY
      CHARACTER*16        NOMCMD
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 28/06/2000   AUTEUR CIBHHLV L.VIVAN 
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
C TOLE CRP_6
C-----------------------------------------------------------------------
C     CALCULE LES VOLUMES USES DE TUBE (GUIDAGE CERCLE)
C ----------------------------------------------------------------------
      INTEGER  USURE(10,5), NBUSUR, I, J, L, IPAS(10,2), IFIRES, IUNIFI
      INTEGER  K, N, INDICE, INDIC2
      REAL*8   TUBE(20,3),  PAS(10,2), PARUSU(10,4), PASNU, THETA
      REAL*8   ANSINI, ANSFIN, ANGDEB, ANGMAX, ANGINI, ANGFIN
      REAL*8   VOLUME, PROFON, A1, A2, B1, B2, RHO
      REAL*8   EPSI, XPOUR, C, D, E, F, DS, DS100
      REAL*8   XF1, RAC11, RAC21, RAC31, SUSEE1
      REAL*8   XF2, RAC12, RAC22, RAC32, SUSEE2, SUSEE
C-----------------------------------------------------------------------
C
C     PAS DE DESCRIPTION POUR LES ZONES NON-USEES :
C     -------------------------------------------
      PASNU = 5.D-1
C
      PROFON = 0.D0
      IFIRES = IUNIFI('MESSAGE')
C
      DO 110 I = 1 , NS
         DO 112 J = 1 , 4
            PARUSU(I,J) = 0.D0
 112     CONTINUE
         DO 114 J = 1 , 2
            IPAS(I,J) = 0
             PAS(I,J) = 0.D0
 114     CONTINUE
         DO 116 J = 1 , 5
            USURE(I,J) = 0.D0
 116     CONTINUE
         TUBE(I,1) = SECT(I)
         TUBE(I,2) = SECT(I+1)
         TUBE(I,3) = VOLTUB(I)
 110  CONTINUE
C
      J = 0
      N = 1
 10   CONTINUE
         I = 0
 15      CONTINUE
            IF ( TUBE(N,3) .NE. 0.D0 ) THEN
               IF ( N .EQ. NS ) THEN
                  N = N + 1
                  I = I + 1
                  GOTO 20
               ENDIF
               N = N+1
               I = I+1
               IF (I.GE.3) GOTO 20
               GOTO 15
            ENDIF
            IF ( I .EQ. 0 ) THEN
               IF ( N .EQ. NS )  GOTO 25
               N = N+1
               GOTO 10
            ENDIF
 20      CONTINUE
         J = J + 1
         USURE(J,1) = J
         USURE(J,2) = I
         USURE(J,3) = N - I
         USURE(J,4) = N - 1
         IF ( N .EQ. (NS+1) )  GOTO 25
         GOTO 10        
 25   CONTINUE
      NBUSUR = J
C
C     DETERMINATION DES TYPES D'USURE :     1 : LUNULE
C     -------------------------------       
C
      DO 120 I = 1 , NBUSUR
         USURE(I,5) = 1
 120  CONTINUE
C
C     CALCUL DES PARAMETRES DES USURES :
C     --------------------------------
C
      ANGINI = 0.D0
      INDICE = 0
      IF ( (USURE(1,3).EQ.1) .AND. (USURE(NBUSUR,4).EQ.NS) ) THEN
         K = USURE(1,4)
         INDICE = 1
         NBUSUR = NBUSUR - 1
         DO 122 I = 1 , NBUSUR
            USURE(I,1)=USURE((I+1),1)-1
            DO 124 J = 2 , 5  
               USURE(I,J) = USURE((I+1),J)
 124        CONTINUE
 122     CONTINUE
         USURE(NBUSUR,2) = K + 11 - USURE(NBUSUR,3)
         USURE(NBUSUR,4) = K
      ENDIF
C
      DO 130 I = 1 , NBUSUR
         VOLUME = 0.D0
C
         DO 132 J = USURE(I,3) , USURE(I,4)
            VOLUME = VOLUME + TUBE(J,3)
 132     CONTINUE
         ANSINI = TUBE(USURE(I,3),1)
         ANSFIN = TUBE(USURE(I,4),2)
         IF ( I .EQ. NBUSUR ) THEN
            IF ( INDICE .EQ. 1 ) THEN
               VOLUME = 0.D0
               DO 134 K = USURE(I,3) , NS
                  VOLUME = VOLUME + TUBE(K,3)
 134           CONTINUE
               DO 136 K = 1 , USURE(I,4)
                  VOLUME = VOLUME + TUBE(K,3)
 136           CONTINUE
               ANSFIN = ANSFIN + 360.D0
            ENDIF
         ENDIF
         CALL LUNULE ( RCRAY, RCARTE, ANGDEB, ANGFIN, ANGMAX, ANSINI,
     &                 ANSFIN, PROFON, VOLUME, EPAIS )
C
         PARUSU(I,1) = ANGDEB
         PARUSU(I,2) = ANGMAX
         PARUSU(I,3) = ANGFIN
         PARUSU(I,4) = PROFON
C
C        CALCUL DE LA SURFACE INDICE 1 :
C        -----------------------------
C
         XF1 = PROFON*((2*RCARTE)+PROFON)/(2*(RCARTE-RCRAY+PROFON))
         RAC11 = (1.D0 - (XF1/RCRAY))
         RAC21 = (RCRAY - XF1)
         RAC31 = (XF1*(2*RCRAY - XF1))**0.5D0
         SUSEE1 = ( (RCRAY*RCRAY)*ACOS(RAC11) ) - (RAC21*RAC31)
C
C        CALCUL DE LA SURFACE INDICE 2 :
C        -----------------------------
C
         XF2 = PROFON*((2*RCRAY)-PROFON)/ (2*(RCARTE-RCRAY+PROFON))
         RAC12 = (1.D0 - (XF2/RCARTE))
         RAC22 = (RCARTE - XF2)
         RAC32 = (XF2*(2*RCARTE - XF2))**0.5D0
         SUSEE2 = ( (RCARTE*RCARTE)*ACOS(RAC12) ) - (RAC22*RAC32)
C
C        CALCUL DE LA SURFACE LUNULE :
C        ---------------------------
C
         SUSEE = SUSEE1 - SUSEE2
         WRITE(IFIRES,*)'SURFACE USEE = ',SUSEE
C
C        CALCUL DE LA PERTE DE SECTION :
C        -----------------------------
C
         DS    = (SUSEE-SINIT)/SINIT
         DS100 = (1.D0 + DS)*100.D0
         WRITE(IFIRES,*)'LA PERTE DE SECTION EST DE = ',DS100,' %'
C
C        VERIFICATION DU NON PERCEMENT :
C        -----------------------------
C
         IF ( PROFON .GE. ECRAY ) THEN
            CALL UTDEBM('A',NOMCMD,'******* PERCEMENT TUBE *******')
            CALL UTIMPR('L','LA PROFONDEUR ', 1, PROFON )
            CALL UTIMPR('S',' EST SUPERIEURE A L''EPAISSEUR ', 1, ECRAY)
            CALL UTFINM
         ENDIF 
 130  CONTINUE
C
      IF ( INDICE .EQ. 1 )  ANGINI = PARUSU(NBUSUR,3) - 360.D0
C
C     PAS DE DESCRIPTION DANS LES ZONES USEES :
C     --------------------------------------- 
C
      CALL USPAS1 ( NBUSUR, PARUSU, IPAS, PAS )
C
C     TRACE DES USURES :
C     ------------------  
C
      L = 0
      THETA = 0.D0
      INDIC2 = 0
      IF ( PARUSU(1,1) .LT. 0.D0 ) THEN
         THETA = PARUSU(1,1)
         INDIC2 = 1
         GOTO 50 
      ENDIF   
      IF ( INDICE .EQ. 1 )  THETA = ANGINI
      IF ( NBUSUR .EQ. 0 ) THEN
         L = L + 1
         TUBUSE(2*L-1) = THETA
         TUBUSE(2*L  ) = RCRAY
      ENDIF
      IF (NBUSUR.NE.0) THEN
30       CONTINUE
            L = L + 1
            TUBUSE(2*L-1) = THETA
            TUBUSE(2*L  ) = RCRAY
            THETA = THETA + PASNU
            IF ( THETA .GE. PARUSU(1,1) ) GOTO 50
         GOTO 30
      ENDIF     
50    CONTINUE
C
      EPSI = 1.D-10
      IF ( ABS(PROFON) .GT. EPSI )  XPOUR = 0.9D0*PROFON
C
      DO 150 I = 1 , NBUSUR
C
         A1 = -PARUSU(I,4) / ( PARUSU(I,2) - PARUSU(I,1) )
         B1 = RCRAY - A1*PARUSU(I,1)
         DO 152 J = 1 , IPAS(I,1)
            RHO = A1*THETA + B1
            IF ( ABS(PROFON) .GT. EPSI ) THEN
               IF ( (RCRAY-RHO) .GE. XPOUR )  RHO = TUBUSE(2*L)
            ENDIF
            L = L + 1
            IF ( THETA .LT. 0.D0 ) THEN
               TUBUSE(2*L-1) = THETA + 360.D0
            ELSEIF ( THETA .GT. 360.D0 ) THEN
               TUBUSE(2*L-1) = THETA - 360.D0
            ELSE
               TUBUSE(2*L-1) = THETA
            ENDIF
            TUBUSE(2*L  ) = RHO
            THETA = THETA + PAS(I,1)
 152     CONTINUE  
C         
         A2 = -PARUSU(I,4) / ( PARUSU(I,2) - PARUSU(I,3) )
         B2 = RCRAY - A2*PARUSU(I,3)
         DO 154 J = 1 , IPAS(I,2)
            RHO = A2*THETA + B2
            IF ( ABS(PROFON) .GT. EPSI ) THEN
               IF ( (RCRAY-RHO) .GE. XPOUR ) RHO = TUBUSE(2*L)
            ENDIF
            L = L + 1
            IF ( THETA .LT. 0.D0 ) THEN
               TUBUSE(2*L-1) = THETA + 360.D0
            ELSEIF ( THETA .GT. 360.D0 ) THEN
               TUBUSE(2*L-1) = THETA - 360.D0
            ELSE
               TUBUSE(2*L-1) = THETA
            ENDIF
            TUBUSE(2*L  ) = RHO
            THETA = THETA + PAS(I,2)
 154     CONTINUE
C
         IF ( I .NE. NBUSUR ) THEN
60          CONTINUE
               THETA = THETA + PASNU
               IF ( THETA .GE. PARUSU(I+1,1) )  GOTO 70
               L = L + 1
               IF ( THETA .LT. 0.D0 ) THEN
                  TUBUSE(2*L-1) = THETA + 360.D0
               ELSEIF ( THETA .GT. 360.D0 ) THEN
                  TUBUSE(2*L-1) = THETA - 360.D0
               ELSE
                  TUBUSE(2*L-1) = THETA
               ENDIF
               TUBUSE(2*L  ) = RCRAY
               GOTO 60
70          CONTINUE
         ENDIF
 150  CONTINUE
C
      IF ( INDICE .EQ. 1 ) GOTO 100      
 80   CONTINUE
         THETA = THETA + PASNU
         IF ( INDIC2 .EQ. 1 ) THEN
            IF ( THETA .GE. (360.D0+PARUSU(1,1)) )  GOTO 100
         ENDIF
         IF ( THETA .GE. 360.D0 ) GOTO 90
         L = L + 1
         TUBUSE(2*L-1) = THETA
         TUBUSE(2*L)   = RCRAY
      GOTO 80
 90   CONTINUE
      L = L + 1
      TUBUSE(2*L-1) = 360.D0
      TUBUSE(2*L)   = RCRAY
C
 100  CONTINUE
      DIMTUB = L
      DO 160 I = 1,DIMTUB
         C = TUBUSE(2*I-1)
         D = TUBUSE(2*I)
         DO 162 J = I+1 , DIMTUB
            IF ( TUBUSE(2*J-1).LT.C .AND. TUBUSE(2*J-1).GT.0.D0 ) THEN
               E = C
               F = D
               C = TUBUSE(2*J-1)
               D = TUBUSE(2*J)
               TUBUSE(2*J-1) = E
               TUBUSE(2*J)   = F
            ENDIF
 162     CONTINUE
         TUBUSE(2*I-1) = C
         TUBUSE(2*I)   = D
 160  CONTINUE
C
      END
