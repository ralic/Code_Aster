      SUBROUTINE ABSCVL(NDIM,JTABAR,XG,S)
      IMPLICIT NONE 

      INTEGER       NDIM,JTABAR
      REAL*8        XG(NDIM),S  

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/06/2010   AUTEUR CARON A.CARON 
C ======================================================================
C COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                      TROUVER L'ABSCISSE CURVILIGNE D'UN POINT
C                      SUR UNE ARETE QUADRATIQUE A PARTIR DE SES
C                      COORDONNEES DANS L'ELEMENT REEL
C                    
C     ENTREE
C       NDIM     : DIMENSION TOPOLOGIQUE DU MAILLAGE
C       JTABAR   : COORDONNEES DES 3 NOEUDS QUI DEFINISSENT L'ARETE
C       XG       : COORDONNEES DU POINT DANS L'ELEMENT REEL
C
C     SORTIE
C       S        : ABSCISSE CURVILIGNE DU POINT P PAR RAPPORT AU 
C                  PREMIER POINT STOCKE DANS COORSG
C     ----------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  ------------------------
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  ------------------------

      REAL*8        XGG,A,B,KSIDER
      REAL*8        TABELT(3),XE
      INTEGER       IRET,K
      CHARACTER*8   ELP      
C
C......................................................................

      CALL JEMARQ()
C     TABAR : XE2=-1  /  XE1= 1  /  XE3= 0
C     XE2 ETANT LE POINT D'ORIGINE

C      RTECHERCHE DE LA MONOTONIE SUR X ET Y
        K=0
 1      CONTINUE
        K=K+1 
        A = ZR(JTABAR-1+K)+ZR(JTABAR-1+NDIM+K)-2*ZR(JTABAR-1+2*NDIM+K)
        B = ZR(JTABAR-1+NDIM+K)-ZR(JTABAR-1+K)

        IF (ABS(A).LE.1.D-6) THEN
          IF (ABS(B).GT.1.D-6) THEN
C         JE BALANCE SUR K
          TABELT(1)=ZR(JTABAR-1+K)
          TABELT(2)=ZR(JTABAR-1+NDIM+K)
          TABELT(3)=ZR(JTABAR-1+2*NDIM+K)
          XGG      =XG(K)
          GO TO 2
          ELSEIF (ABS(B).LE.1.D-6) THEN
            IF(K.EQ.1) THEN
            GO TO 1
            ELSEIF(K.EQ.2) THEN
C             LES 3 POINTS SONT CONFONDUS!
              CALL U2MESS('F','XFEM_66')
            ENDIF
          ENDIF
        ELSEIF (ABS(A).GT.1.D-6) THEN
        KSIDER = -B/A
          IF(KSIDER.GT.-1.D0 .AND. KSIDER.LT.1.D0)THEN
            IF(K.EQ.1) THEN
            GO TO 1
            ELSEIF(K.EQ.2) THEN
C            L'ARETE EST TROP ARRONDIE :
C            IL Y A 2 SOLUTIONS SUIVANT X ET 2 SUIVANT Y
              CALL U2MESS('F','XFEM_66')
            ENDIF
          ELSEIF(KSIDER.GT.1.D0 .OR. KSIDER.LT.-1.D0)THEN
        TABELT(1)=ZR(JTABAR-1+K)
        TABELT(2)=ZR(JTABAR-1+NDIM+K)
        TABELT(3)=ZR(JTABAR+2*NDIM+K)
        XGG       =XG(K)
        GO TO 2
          ENDIF
        ENDIF

 2      CONTINUE

C     ALIAS DE L'ARETE (QUADRATIQUE)
      ELP='SE3'

C     CALCUL COORDONNEES DE REF (ETA) DE XGG SUR L'ARETE
      CALL REEREG('S',ELP,3,TABELT,XGG,1,XE,IRET)
      CALL ASSERT(XE.GE.-1 .AND. XE.LE.1)

C     CALCUL ABSCISSE CURVILIGNE (S) DE XGG
C     ---L'ORIGINE EST LE 1ER PT DE COORSG---
      CALL ABSCVF(NDIM,JTABAR,XE,S)

      CALL JEDEMA()
      END
