      SUBROUTINE I2APPM (XP,YP,XSO,YSO,XIN,YIN,CDROI,NBC,DEDANS)
      IMPLICIT   NONE
      REAL*8     XP, YP, XSO(*), YSO(*), XIN(*), YIN(*)
      LOGICAL    DEDANS, CDROI(*)
      INTEGER    NBC
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 02/03/99   AUTEUR G8BHHXD X.DESROCHES 
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
C
      INTEGER  C, K, L, NT, I
      REAL*8   PS, XD, YD, XF, YF
C
      DEDANS = .FALSE.
      XF     = XSO(1)
      YF     = YSO(1)
      K      = 0
      L      = 0
C
      NT = NBC
      DO 10 C = 1 , NBC
C
         IF ( .NOT. CDROI(C) ) THEN
C
            NT = NT + 1
            DO 12 I = 1 , 2
               XD = XF
               YD = YF
               IF ( I .EQ. 1 ) THEN
                  XF = XIN(C)
                  YF = YIN(C)
               ELSE
                  XF = XSO(C+1)
                  YF = YSO(C+1)
               ENDIF
C 
               PS = (XP-XD)*(YD-YF) + (YP-YD)*(XF-XD)
C
               IF ( PS .GE. 0.0D0 ) THEN
                  K = K + 1
               ELSE
                  L = L + 1
               ENDIF
C
 12         CONTINUE
C
         ELSE
C
            XD = XF
            YD = YF
            XF = XSO(C+1)
            YF = YSO(C+1)
C 
            PS = (XP-XD)*(YD-YF) + (YP-YD)*(XF-XD)
C
            IF ( PS .GE. 0.0D0 ) THEN
               K = K + 1
            ELSE
               L = L + 1
            ENDIF
C
         ENDIF
C
 10   CONTINUE
C
      IF ( K .EQ. NT  .OR.  L .EQ. NT ) DEDANS = .TRUE.
C
      END
