      SUBROUTINE  BURMAT(FAMI,KPG,KSP,MOD,IMAT,NMAT,
     &                   MATERD,MATERF,MATCST,NDT,NDI,NR,NVI)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/12/2011   AUTEUR FOUCAULT A.FOUCAULT 
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
C RESPONSABLE FOUCAULT A.FOUCAULT
C       ----------------------------------------------------------------
C       RECUPERATION DU MATERIAU A TEMPF ET TEMPD
C       IN  FAMI   :  FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
C           KPG,KSP:  NUMERO DU (SOUS)POINT DE GAUSS
C           MOD    :  TYPE DE MODELISATION
C           IMAT   :  ADRESSE DU MATERIAU CODE
C           NMAT   :  DIMENSION 1 DE MATER
C       OUT MATERD :  COEFFICIENTS MATERIAU A T    (TEMPD )
C           MATERF :  COEFFICIENTS MATERIAU A T+DT (TEMPF )
C                     MATER(*,I) = CARACTERISTIQUES MATERIAU
C                                    I = 1  CARACTERISTIQUES ELASTIQUES
C                                    I = 2  CARACTERISTIQUES VISQUEUSES
C           MATCST :  'OUI' SI  MATERIAU A T = MATERIAU A T+DT
C                     'NON' SINON OU 'NAP' SI NAPPE DANS 'VECMAT.F'
C           NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
C           NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
C           NR     :  NB DE COMPOSANTES SYSTEME NL
C           NVI    :  NB DE VARIABLES INTERNES
C       ----------------------------------------------------------------

C     NOM                         a t-                 a t+ (t-+dt)
C     -------------------------------------------------------------
C     E                           MATERD(1,1)          MATERF(1,1)
C     NU                          MATERD(2,1)          MATERF(2,1)
C     ALPHA                       MATERD(3,1)          MATERF(3,1)
C     BENDO                       MATERD(4,1)          MATERF(4,1)
C     KDESS                       MATERD(5,1)          MATERF(5,1)
C     HYGRO                       MATERD(6,1)          MATERF(6,1)

C     KRS                         MATERD(1,2)          MATERF(1,2)
C     ETARS                       MATERD(2,2)          MATERF(2,2)
C     ETAIS                       MATERD(3,2)          MATERF(3,2)
C     KRD                         MATERD(4,2)          MATERF(4,2)
C     ETARD                       MATERD(5,2)          MATERF(5,2)
C     ETAID                       MATERD(6,2)          MATERF(6,2)
C     KAPPA                       MATERD(7,2)          MATERF(7,2)
C     ETAFD                       MATERD(8,2)          MATERF(8,2)
C     ----------------------------------------------------------------
      IMPLICIT NONE
      INTEGER         KPG,KSP,IMAT,NMAT,NDT,NDI,NR,NVI
      INTEGER         CERR(14),II
      REAL*8          MATERD(NMAT,2) ,MATERF(NMAT,2)
      REAL*8          EPSI, R8PREM
      CHARACTER*3     MATCST
      CHARACTER*(*)   FAMI
      CHARACTER*8     MOD, NOMC(14)

C === ============================================
C
C     RECUPERATION PROPRIETES MATERIAU ELASTIQUE 
C
C === ============================================

      NOMC(1) = 'E        '
      NOMC(2) = 'NU       '
      NOMC(3) = 'ALPHA    '
      NOMC(4) = 'B_ENDOGE'
      NOMC(5) = 'K_DESSIC'

C === ===========
C     INSTANT T- 
C === ===========

      CALL RCVALB(FAMI,KPG,KSP,'-',IMAT,' ','ELAS',0,' ',
     &                0.D0,5,NOMC(1),MATERD(1,1),CERR(1),0)
      IF ( CERR(3) .NE. 0 ) MATERD(3,1) = 0.D0
      IF ( CERR(4) .NE. 0 ) MATERD(4,1) = 0.D0
      IF ( CERR(5) .NE. 0 ) MATERD(5,1) = 0.D0

C === ===========
C     INSTANT T+ 
C === ===========

      CALL RCVALB(FAMI,KPG,KSP,'+',IMAT,' ','ELAS',0,' ',
     &                0.D0,5,NOMC(1),MATERF(1,1),CERR(1),0)
      IF ( CERR(3) .NE. 0 ) MATERF(3,1) = 0.D0
      IF ( CERR(4) .NE. 0 ) MATERF(4,1) = 0.D0
      IF ( CERR(5) .NE. 0 ) MATERF(5,1) = 0.D0

C === =============================================
C
C     RECUPERATION PROPRIETES MATERIAU HYGROMETRIE 
C
C === =============================================

      NOMC(6)='FONC_DES'

C === ===========
C     INSTANT T- 
C === ===========

      CALL RCVALB(FAMI,KPG,KSP,'-',IMAT,' ','ELAS',0,' ',
     &            0.D0,1,NOMC(6),MATERD(6,1),CERR(6),0)
      IF ( CERR(6) .NE. 0 ) CALL U2MESS('F','ALGORITH4_94')
      
C === ===========
C     INSTANT T+ 
C === ===========

      CALL RCVALB(FAMI,KPG,KSP,'+',IMAT,' ','ELAS',0,' ',
     &            0.D0,1,NOMC(6),MATERF(6,1),CERR(6),0)
      IF ( CERR(6) .NE. 0 ) CALL U2MESS('F','ALGORITH4_94')

C === ===============================================
C
C     RECUPERATION PROPRIETES MATERIAU FLUAGE PROPRE 
C
C === ===============================================

      NOMC(7)='K_RS'
      NOMC(8)='ETA_RS'
      NOMC(9)='ETA_IS'
      NOMC(10)='K_RD'
      NOMC(11)='ETA_RD'
      NOMC(12)='ETA_ID'
      NOMC(13)='KAPPA'
      
C === =================
C     INSTANT T- ET T+ 
C === =================

      CALL RCVALB(FAMI,KPG,KSP,'+',IMAT,' ','BETON_BURGER_FP',0,' ',
     &            0.D0,7,NOMC(7),MATERD(1,2),CERR(7),0)

      DO 10 II=1,7
         MATERF(II,2) = MATERD(II,2)
 10   CONTINUE

C === ===================================================
C
C     RECUPERATION PROPRIETES MATERIAU FLUAGE DESSICATION 
C
C === ===================================================

      NOMC(14)='ETA_FD'

C === =================
C     INSTANT T- ET T+ 
C === =================

      CALL RCVALB(FAMI,KPG,KSP,'-',IMAT,' ','BETON_BURGER_FP',0,' ',
     &            0.D0,1,NOMC(14),MATERD(8,2),CERR(14),0)
      IF ( CERR(8) .NE. 0 ) THEN
        MATERD(8,2) = -1.D0
        MATERF(8,2) = -1.D0
      ELSE
        MATERF(8,2) = MATERD(8,2)
      ENDIF

C
C --- MATERIAU CONSTANT?
C
      
      MATCST = 'OUI'
      EPSI=R8PREM()
      DO 30 II = 1,NMAT
        IF (ABS(MATERD(II,1)-MATERF(II,1) ).GT.EPSI*MATERD(II,1)) THEN
        MATCST = 'NON'
        GOTO 9999
        ENDIF
 30   CONTINUE
      DO 40 II = 1,NMAT
        IF (ABS(MATERD(II,2)-MATERF(II,2) ).GT.EPSI*MATERD(II,2)) THEN
        MATCST = 'NON'
        GOTO 9999
        ENDIF
 40   CONTINUE

 9999 CONTINUE
 
C === ===================================================
C
C     RECUPERATION NOMBRE DE COMPOSANTES DES CONTRAINTES 
C                  NOMBRE DE VARIABLES INTERNES
C                  NOMBRE D'INCONNUES OBTENUES PAR NEWTON
C
C === ===================================================
      CALL BURNVI(MOD,NDT, NDI, NR, NVI )
      
      END
