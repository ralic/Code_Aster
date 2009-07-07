      SUBROUTINE RCEVSP ( CSIEX, KEMIXT, CSTEX, CSMEX, CINST, CSPO, 
     +                     CSPE, CSPTO, CSPTE , CSPMO, CSPME )
      IMPLICIT     NONE
      CHARACTER*24 CSIEX, CINST, CSPO, CSPE, CSTEX, CSMEX,
     +              CSPTO, CSPTE, CSPMO, CSPME
      LOGICAL       KEMIXT
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 06/07/2009   AUTEUR GALENNE E.GALENNE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     OPERATEUR POST_RCCM, TYPE_RESU_MECA='EVOLUTION'
C     CALCUL DU SP
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
      CHARACTER*32     JEXNOM, JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER      NCMP, JSIOE, JINST, NBINST, NBORDR, JSPO, JSPE,
     +             IND, I1, I2, ICMP, L1, L2, 
     +             JSTOE, JSMOE, JSPTO, JSPTE, JSPMO, JSPME
      PARAMETER  ( NCMP = 6 )
      REAL*8       SP1O(NCMP), SP1E(NCMP), SP2O(NCMP), SP2E(NCMP),
     +             SP12O(NCMP), SP12E(NCMP), TRESCA,
     +             SPT1O(NCMP), SPT1E(NCMP), SPT2O(NCMP), SPT2E(NCMP),
     +             SPT12O(NCMP), SPT12E(NCMP),
     +             SPM1O(NCMP), SPM1E(NCMP), SPM2O(NCMP), SPM2E(NCMP),
     +             SPM12O(NCMP), SPM12E(NCMP)
      CHARACTER*8  K8B
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      CALL JEVEUO ( CSIEX, 'L', JSIOE )
      IF (KEMIXT)  THEN
        CALL JEVEUO ( CSTEX, 'L', JSTOE )
        CALL JEVEUO ( CSMEX, 'L', JSMOE )
      ENDIF
      CALL JEVEUO ( CINST, 'L', JINST )
      CALL JELIRA ( CINST, 'LONMAX', NBINST, K8B )
C
      NBORDR = (NBINST*(NBINST+1)) / 2
      CALL WKVECT (CSPO, 'V V R', NBORDR, JSPO )
      CALL WKVECT (CSPE, 'V V R', NBORDR, JSPE )
      IF (KEMIXT) THEN
        CALL WKVECT (CSPTO, 'V V R', NBORDR, JSPTO )
        CALL WKVECT (CSPTE, 'V V R', NBORDR, JSPTE )
        CALL WKVECT (CSPMO, 'V V R', NBORDR, JSPMO )
        CALL WKVECT (CSPME, 'V V R', NBORDR, JSPME )
      ENDIF
      IND = 0
C
      DO 100 I1 = 1, NBINST
C
         DO 102 ICMP = 1, NCMP
            L1 = NCMP*(I1-1) + ICMP
            L2 = NCMP*NBINST + NCMP*(I1-1) + ICMP
            SP1O(ICMP) = ZR(JSIOE-1+L1)
            SP1E(ICMP) = ZR(JSIOE-1+L2)
            IF (KEMIXT) THEN
              SPT1O(ICMP) = ZR(JSTOE-1+L1)
              SPT1E(ICMP) = ZR(JSTOE-1+L2)
              SPM1O(ICMP) = ZR(JSMOE-1+L1)
              SPM1E(ICMP) = ZR(JSMOE-1+L2)
            ENDIF
 102     CONTINUE
         IND = IND + 1
C         
         ZR(JSPO+IND-1) = 0.D0
         ZR(JSPE+IND-1) = 0.D0
         IF (KEMIXT) THEN
           ZR(JSPTO+IND-1) = 0.D0
           ZR(JSPTE+IND-1) = 0.D0
           ZR(JSPMO+IND-1) = 0.D0
           ZR(JSPME+IND-1) = 0.D0
         ENDIF
C
         DO 110 I2 = I1+1, NBINST
C
            DO 112 ICMP = 1, NCMP
              L1 = NCMP*(I2-1) + ICMP
              L2 = NCMP*NBINST + NCMP*(I2-1) + ICMP
              SP2O(ICMP) = ZR(JSIOE-1+L1)
              SP2E(ICMP) = ZR(JSIOE-1+L2)
              IF (KEMIXT) THEN
                SPT2O(ICMP) = ZR(JSTOE-1+L1)
                SPT2E(ICMP) = ZR(JSTOE-1+L2)
                SPM2O(ICMP) = ZR(JSMOE-1+L1)
                SPM2E(ICMP) = ZR(JSMOE-1+L2)
              ENDIF
 112        CONTINUE
            IND = IND + 1
C ======================================================================
C ---       COMBINAISON DES CONTRAINTES AUX 2 INSTANTS TEMP1 ET TEMP2 :
C ======================================================================
            DO 114 ICMP = 1 , NCMP
               SP12O(ICMP) = SP1O(ICMP) - SP2O(ICMP)
               SP12E(ICMP) = SP1E(ICMP) - SP2E(ICMP)
               IF (KEMIXT) THEN
                 SPT12O(ICMP) = SPT1O(ICMP) - SPT2O(ICMP)
                 SPT12E(ICMP) = SPT1E(ICMP) - SPT2E(ICMP)
                 SPM12O(ICMP) = SPM1O(ICMP) - SPM2O(ICMP)
                 SPM12E(ICMP) = SPM1E(ICMP) - SPM2E(ICMP)
               ENDIF
 114        CONTINUE
C ======================================================================
C ---       CALCUL DE LA NORME DE TRESCA DE LA DIFFERENCE DES TENSEURS 
C ---       DE CONTRAINTES TOTALES 
C ---       SP12O = SPO(TEMP1)-SPO(TEMP2) A L'ORIGINE DU CHEMIN :
C ======================================================================
            CALL RCTRES ( SP12O, TRESCA )
            ZR(JSPO+IND-1) = TRESCA
C ======================================================================
C ---       CALCUL DE LA NORME DE TRESCA DE LA DIFFERENCE DES TENSEURS 
C ---       DE CONTRAINTES TOTALES A L'EXTREMITE DU CHEMIN :
C ======================================================================
            CALL RCTRES ( SP12E, TRESCA )
            ZR(JSPE+IND-1) = TRESCA
C
            IF (KEMIXT) THEN            
C ======================================================================
C ---       CAS KE_MIXTE : SEPARATION DES CONTRAINTES THERMIQUES
C ---       ET DES CONTRAINTES MECANIQUES
C ======================================================================
            CALL RCTRES ( SPT12O, TRESCA )
            ZR(JSPTO+IND-1) = TRESCA
C            
            CALL RCTRES ( SPT12E, TRESCA )
            ZR(JSPTE+IND-1) = TRESCA
C
            CALL RCTRES ( SPM12O, TRESCA )
            ZR(JSPMO+IND-1) = TRESCA
C            
            CALL RCTRES ( SPM12E, TRESCA )
            ZR(JSPME+IND-1) = TRESCA
C
            ENDIF
C
 110     CONTINUE
C
 100  CONTINUE
C
      CALL JEDEMA( )
      END
