      SUBROUTINE RC32R8 ( NOMRES, MATER, SYMAX )
      IMPLICIT   NONE
      REAL*8              SYMAX
      CHARACTER*8         NOMRES, MATER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/10/2006   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     ------------------------------------------------------------------
C     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
C     STOCKAGE DES RESULTATS DANS LA TABLE DE SORTIE
C     CALCUL DU ROCHET THERMIQUE
C
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32 JEXNOM,JEXNUM,JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER       IBID, NPAR1, IM, JRESU
      PARAMETER    ( NPAR1 = 7 )
      REAL*8        RBID, VALER(NPAR1), R8VIDE, VALRES
      COMPLEX*16    C16B
      CHARACTER*2   CODRET
      CHARACTER*4   LIEU(2)
      CHARACTER*8   K8B, TYPAR1(NPAR1), VALEK(2)
      CHARACTER*16  NOPAR1(NPAR1)
      CHARACTER*19  NOT19R
C     ------------------------------------------------------------------
      DATA LIEU   / 'ORIG' , 'EXTR' /
C
      DATA NOPAR1 / 'TYPE', 'LIEU', 'SY', 'SP_THER', 'SIGM_M_PRES',
     +              'VALE_MAXI_LINE', 'VALE_MAXI_PARAB' /
      DATA TYPAR1 / 'K8', 'K8', 'R', 'R', 'R', 'R', 'R' /
C DEB ------------------------------------------------------------------
C
      CALL TBAJPA ( NOMRES, NPAR1-2, NOPAR1(3), TYPAR1(3) )
C
      IF ( SYMAX .EQ. R8VIDE() ) THEN
         CALL RCVALE ( MATER, 'RCCM', 0, K8B, RBID, 1, 
     +                                'SY_02   ', VALRES, CODRET, '  ' )
         IF ( CODRET .EQ. 'OK' ) THEN
            SYMAX = VALRES
         ELSE
            CALL U2MESS('A','POSTRELE_66')
            GOTO 9999
         ENDIF
      ENDIF
C
      VALER(1) = SYMAX
      VALEK(1) = 'ROCHET'
C
      DO 10 IM = 1 , 2
C
         VALEK(2) = LIEU(IM)
C
         CALL JEVEUO ( '&&RC3200.RESULTAT  .'//LIEU(IM), 'L', JRESU)
C
         VALER(2) = ZR(JRESU+12)
         VALER(3) = ZR(JRESU+11)
C
         CALL RCMCRT ( SYMAX, VALER(3), VALER(4), VALER(5) )
C
         CALL TBAJLI ( NOMRES, NPAR1, NOPAR1, IBID,
     +                                   VALER, C16B, VALEK, 0 )
C
 10   CONTINUE
C
 9999 CONTINUE
C
      END
