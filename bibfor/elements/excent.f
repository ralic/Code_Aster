      SUBROUTINE EXCENT(SENS,EXCEN,NBPOIN,NBCMP,LREEL,REFFIN,REFFOU,
     &                  CEFFIN,CEFFOU)
      IMPLICIT  NONE
      CHARACTER*(*) SENS
      INTEGER NBPOIN,NBCMP
      REAL*8 EXCEN,REFFIN(*),REFFOU(*)
      COMPLEX*16 CEFFIN(*),CEFFOU(*)
      LOGICAL LREEL
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 11/10/2011   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PELLET J.PELLET
C ======================================================================
C  BUT : TENIR COMPTE DE L'EXCENTREMENT D'UNE COQUE POUR CHANGER
C        LE "PLAN" DE CALCUL DES EFFORTS
C  ARGUMENTS :
C     IN   K*   SENS :
C           'MOY'  : MAIL -> MOY
C           'MAIL' : MOY  -> MAIL
C     IN   R(*) REFFIN : EFFORTS (REELS) "IN"
C     OUT  R(*) REFFOU : EFFORTS (REELS) "OUT"
C     IN   C(*) CEFFIN : EFFORTS (COMPLEXES) "IN"
C     OUT  C(*) CEFFOU : EFFORTS (COMPLEXES) "OUT"
C     IN   I    NBPOIN : NOMBRE DE POINTS DE GAUSS OU DE NOEUDS
C     IN   I    NBCMP  : NOMBRE DE CMPS DES EFFORTS (PAR POINT)
C     IN   L    LREEL  : .TRUE.  : LES EFFORTS SONT REELS
C                        .FALSE. : LES EFFORTS SONT COMPLEXES
C  REMARQUE :
C     ON PEUT APPELER CETTE ROUTINE AVEC LE MEME TABLEAU POUR EFFOU ET
C     EFFIN (IL N'Y A PAS D'EFFET DE BORD)
C     ------------------------------------------------------------------
      INTEGER K,I
      REAL*8 RSIGN
C     ------------------------------------------------------------------

      IF (SENS.EQ.'MOY') THEN
        RSIGN=-1.D0
      ELSEIF (SENS.EQ.'MAIL') THEN
        RSIGN=+1.D0
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF


      IF (LREEL) THEN
        DO 10,K=1,NBPOIN*NBCMP
          REFFOU(K)=REFFIN(K)
   10   CONTINUE

        DO 20 I=1,NBPOIN
          REFFOU((I-1)*NBCMP+4)=REFFOU((I-1)*NBCMP+4)+
     &                             RSIGN*EXCEN*REFFOU((I-1)*NBCMP+1)
          REFFOU((I-1)*NBCMP+5)=REFFOU((I-1)*NBCMP+5)+
     &                             RSIGN*EXCEN*REFFOU((I-1)*NBCMP+2)
          REFFOU((I-1)*NBCMP+6)=REFFOU((I-1)*NBCMP+6)+
     &                             RSIGN*EXCEN*REFFOU((I-1)*NBCMP+3)
   20   CONTINUE


      ELSE
        DO 30,K=1,NBPOIN*NBCMP
          CEFFOU(K)=CEFFIN(K)
   30   CONTINUE

        DO 40 I=1,NBPOIN
          CEFFOU((I-1)*NBCMP+4)=CEFFOU((I-1)*NBCMP+4)+
     &                             RSIGN*EXCEN*CEFFOU((I-1)*NBCMP+1)
          CEFFOU((I-1)*NBCMP+5)=CEFFOU((I-1)*NBCMP+5)+
     &                             RSIGN*EXCEN*CEFFOU((I-1)*NBCMP+2)
          CEFFOU((I-1)*NBCMP+6)=CEFFOU((I-1)*NBCMP+6)+
     &                             RSIGN*EXCEN*CEFFOU((I-1)*NBCMP+3)
   40   CONTINUE

      ENDIF
      END
