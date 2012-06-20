      SUBROUTINE RKCAH1( COMP,Y,PAS,NVI,W,WK,H,EPS,IRET)
      IMPLICIT NONE
C     ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/06/2012   AUTEUR PROIX J-M.PROIX 
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
C     INTEGRATION DE LOIS DE COMPORTEMENT PAR  RUNGE KUTTA
C     CALCUL DU NOUVEAU PAS DE TEMPS (AUGMENTATION)
C      IN COMP    :  NOM DU MODELE DE COMPORTEMENT
C         Y       :  VARIABLES INTERNES
C         PAS     :  INTERVALLE DE TEMPS TF-TD
C     OUT H       :  PAS DE TEMPS

      INTEGER  NE,NY,NA,NVI,II,IRET
      CHARACTER*16 LOI,COMP(*)
      REAL*8 PAS,H,W,DMG0,EPS,MAXOUT,MAXDOM,WK(*),Y(*)
      PARAMETER  ( MAXDOM = 9.90D-01  )
C
      LOI=COMP(1)
      NE=0
      NY=NVI
      NA=NY+NVI
      IRET=0
      
      MAXOUT=MAXDOM-EPS
      
      IF (LOI(1:9).EQ.'VENDOCHAB') THEN
C        TRAITEMENT VENDOCHAB
C        TEST SUR LE NIVEAU DE DOMMAGE--
         IF (Y(9).GE.MAXDOM) THEN
            DMG0=(Y(9)-WK(9))-(WK(NA+9)*H)
            IF (DMG0.GE.MAXOUT) THEN
               DO 99 II=1,NVI
                  Y(II)=(Y(II)-WK(NE+II))-(WK(NA+II)*H)
   99          CONTINUE
               IRET=1
            ELSE
               H=(MAXOUT-DMG0)/((WK(NE+9)/H)+WK(NA+9))
               IF (H.GT.PAS) H=PAS
            ENDIF
         ELSE
C           FIN TEST SUR LE NIVEAU DE DOMMAGE
            W=W/ABS(EPS)
            W=MAX(W,1.0D-05)
            H=H*W**(-2.0D-01)*9.0D-01
            IF (H.GT.PAS) H=PAS
         ENDIF
C        FIN TRAITEMENT VENDOCHAB

      ELSE
C        IP.NE.1
C        CALCUL CLASSIQUE DU NOUVEAU PAS DE TEMPS (ISSU DE RK4)         
         W=W/ABS(EPS)
         W=MAX(W,1.0D-05)
C        POUR 1.0D-05, COEF=9.         
         H=H*W**(-2.0D-01)*9.0D-01
         IF (H.GT.PAS) H=PAS
         
      ENDIF

      END
