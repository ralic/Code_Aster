      INTEGER FUNCTION ENTCOD(NEC,MODE,K,L)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 30/01/2002   AUTEUR VABHHTS J.TESELET 
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
C-----------------------------------------------------------------------
      IMPLICIT REAL*8 (A-H,O-Z)
C     IN:
C     MODE: MODE_LOCAL DE TYPE CHNO,VECT,OU MATR.
C     NEC : NBRE D ENTIERS POUR LA GRANDEUR
C     K : NUMERO DE NOEUD ( LOCAL ) ; L : NUMERO D ENTIER CODE
C     OUT:
C     ENTCOD: KEME ENTIER CODE.
C
C-----------------------------------------------------------------------
      INTEGER MODE,MODEAV,M1,M2,CODE,CODE1
      CHARACTER*8 K8B1,K8B2,K8B3,K8B4
C
C     FONCTIONS JEVEUX
C
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C
C     COMMUNS   JEVEUX
C
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
C
C     -- REMANENCE DU MODE LOCAL:
      SAVE MODEAV,IADM1,N1
C
      IF (MODE.NE.MODEAV) THEN
         CALL JEVEUS(JEXNUM('&CATA.TE.MODELOC',MODE),'L',IADM)
         MODEAV = MODE
      ELSE
         GO TO 9998
      END IF
C
      CODE = ZI(IADM)
      IF (CODE.GT.3) THEN
         IF (CODE.EQ.4) THEN
            M1 = ZI(IADM+3)
         ELSE IF (CODE.EQ.5) THEN
            M1 = ZI(IADM+3)
            M2 = ZI(IADM+4)
            IF (M1.NE.M2) THEN
               CALL CODENT(M1,'D',K8B1)
               CALL CODENT(M2,'D',K8B2)
               CALL UTMESS('F',' ENTCOD 1',
     +                     ' MODE LIGNE '//K8B1//' /= MODE COLONNE '//
     +                     K8B2)
            END IF
         END IF
         CALL JEVEUS(JEXNUM('&CATA.TE.MODELOC',M1),'L',IADM1)
         CODE1 = ZI(IADM1)
         IF (CODE1.GT.3) THEN
            CALL CODENT(MODE,'D',K8B1)
            CALL CODENT(CODE,'D',K8B2)
            CALL CODENT(M1,'D',K8B3)
            CALL CODENT(CODE1,'D',K8B4)
            CALL UTMESS('F',' ENTCOD 2',' LE MODE '//K8B1//' DE CODE '//
     +                  K8B2//' REFERENCE LE MODE '//K8B3//
     +                  ' DONT LE CODE : '//K8B4//' > 3 ')
         END IF
      ELSE
         IADM1 = IADM
         M1 = MODE
      END IF
      N1 = ZI(IADM1+3)
C
 9998 CONTINUE
      IF (N1.GT.10000) THEN
         N2 = N1 - 10000
         IF (K.GT.N2) THEN
            CALL CODENT(M1,'D',K8B1)
            CALL CODENT(N2,'D',K8B2)
            CALL CODENT(K,'D',K8B3)
            CALL UTMESS('F',' ENTCOD 3',' POUR LE MODE '//K8B1//
     +                  ' NOMBRE DE POINTS '//K8B2//' < ARGUMENT K : '//
     +                  K8B3)
         END IF
         IAD = 4 + NEC* (K-1) + L
      ELSE
         IAD = 4 + L
      END IF
      ENTCOD = ZI(IADM1+IAD-1)
 9999 CONTINUE
      END
