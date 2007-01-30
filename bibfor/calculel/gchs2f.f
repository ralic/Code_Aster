      SUBROUTINE GCHS2F (CHAR1,CHAR2,CHAR3)
      IMPLICIT NONE
      CHARACTER*19    CHAR1,CHAR2,CHAR3
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 29/01/2007   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     BUT : TRANSFORME :
C              CHARGE 'SCALAIRE' --> CHARGE 'FONCTION' (CONSTANTE)
C
C           (ROUTINE SPECIFIQUE A L'OPERATEUR CALC_G,
C            APPELEE PAR GCHARF, DONT LE BUT EST DE 
C            FUSIONNER CHAR1 ET CHAR2)
C
C     IN       :    CHAR1  :  CHARGE 'SCALAIRE' 
C              :    CHAR2  :  CHARGE 'FONCTION'
C     IN/OUT   :    CHAR3  :  CHARGE 'FONCTION'
C
C ======================================================================
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
      CHARACTER*32     JEXNOM, JEXNUM, JEXATR
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C ----------------------------------------------------------------------
      INTEGER JDES1,JDES2,JDES3,NCMP1,NCMP2,NCMP3,I,JVAL1,JVAL3,K,IZO,
     &        JFPRO,JFVAL,KK,INDIK8,IEC,RESTE,CODE,JNCMP1,JNCMP2,J,
     &        NEC,NBEC,IOR
      REAL*8 EPSI,R8PREM
      CHARACTER*8  K8B,NOCMP1,NOMFON
      CHARACTER*19 NOMF19

      CALL JEMARQ()

      NOMFON=CHAR3
      EPSI = R8PREM()

C     DUPLICATION AVANT MISE A JOUR
      CALL JEDUPO(CHAR1//'.DESC','V',CHAR3//'.DESC',.FALSE.)
      CALL JEDUPO(CHAR1//'.NOMA','V',CHAR3//'.NOMA',.FALSE.)
      CALL JEDUPO(CHAR1//'.NOLI','V',CHAR3//'.NOLI',.FALSE.)
      CALL JEDUPO(CHAR1//'.LIMA','V',CHAR3//'.LIMA',.FALSE.)

C     DESC (MAJ 1/2)
      CALL JEVEUO(CHAR1//'.DESC','L',JDES1)
      CALL JEVEUO(CHAR2//'.DESC','L',JDES2)
      CALL JEVEUO(CHAR3//'.DESC','E',JDES3)
      ZI(JDES3)=ZI(JDES2)

      CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',ZI(JDES1)),'LONMAX',
     &            NCMP1,K8B)
      CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',ZI(JDES2)),'LONMAX',
     &            NCMP2,K8B)
      CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',ZI(JDES1)),'L',JNCMP1)
      CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',ZI(JDES2)),'L',JNCMP2) 

C     VALE
      CALL JEVEUO(CHAR1//'.VALE','L',JVAL1)
      CALL WKVECT(CHAR3//'.VALE','V V K8',NCMP2*ZI(JDES3+3-1),JVAL3)

      K=0
      KK=0
      DO 10 I = 1,NCMP1
        NOCMP1 = ZK8(JNCMP1-1+I)
        J=INDIK8(ZK8(JNCMP2),NOCMP1,1,NCMP2)
        IF(J.NE.0)THEN
            K=K+1
            IF(K.EQ.1)THEN
              IEC = (I-1)/30 + 1
              RESTE = I - 30* (IEC-1)
              CODE = 2**RESTE
            ENDIF
            DO 20 IZO=1,ZI(JDES1+3-1)
               IF(ABS(ZR(JVAL1+(IZO-1)*NCMP1+I-1)).GT.EPSI)THEN
                 KK=KK+1
                 CALL CODENT(KK,'D0',NOMFON(7:8))
                 NOMF19=NOMFON
                 CALL WKVECT(NOMF19//'.PROL','V V K16',5,JFPRO)
                 ZK16(JFPRO  )='CONSTANT'
                 ZK16(JFPRO+1)='LIN LIN'
                 ZK16(JFPRO+2)='TOUTPARA'
                 ZK16(JFPRO+3)='TOUTRESU'
                 ZK16(JFPRO+4)='CC'
                 CALL WKVECT(NOMF19//'.VALE','V V R',2,JFVAL)
                 ZR(JFVAL)=1.D0
                 ZR(JFVAL+1)=ZR(JVAL1+(IZO-1)*NCMP1+I-1)
                 ZK8(JVAL3+(IZO-1)*NCMP2+J-1)=NOMFON
               ELSE
                 ZK8(JVAL3+(IZO-1)*NCMP2+J-1)='&FOZERO'
               ENDIF
 20          CONTINUE
          ENDIF
 10   CONTINUE

C     DESC (MAJ 2/2)
      NEC = NBEC(ZI(JDES3))
      DO 30 I=1,NEC*ZI(JDES3+3-1)
        ZI(JDES3+3+2*ZI(JDES3+3-1)+I-1)=
     &       IOR(ZI(JDES3+3+2*ZI(JDES3+3-1)+I-1),CODE)
 30   CONTINUE

      CALL JEDEMA()

      END
