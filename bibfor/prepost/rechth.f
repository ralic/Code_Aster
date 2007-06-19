      SUBROUTINE RECHTH ( TEMPS, NVAL2, TBINTH, TABTHR, TEMPA, TEMPB )
C
      IMPLICIT     NONE
      INTEGER      NVAL2
      REAL*8       TEMPS, TEMPA, TEMPB
      CHARACTER*8  TABTHR
      CHARACTER*19 TBINTH
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 19/06/2007   AUTEUR VIVAN L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C ======================================================================
C --- BUT : RECUPERATION DES TEMPERATURES AUX POINTES DE LA FISSURE ----
C ======================================================================
C IN  : TEMPS  : INSTANT DE CALCUL COURANT -----------------------------
C --- : TABTHR : TABLE DES CHAMPS THERMIQUES ---------------------------
C OUT : TEMPA  : TEMPERATURE EN POINTE A -------------------------------
C --- : TEMPB  : TEMPERATURE EN POINTE B -------------------------------
C ======================================================================
C ----- DEBUT COMMUNS NORMALISES  JEVEUX  ------------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C ----- FIN COMMUNS NORMALISES  JEVEUX  --------------------------------
C ======================================================================
      INTEGER       JINSTH, IBID, ITH, JTETH1, JTETH2, NOTOT, IRET
      REAL*8        LPREC, TEMPH1, TEMPH2
      COMPLEX*16    CBID
      CHARACTER*8   LCRIT, K8B
      CHARACTER*19  TMPTH1, TMPTH2, DEFTH1, DEFTH2
      CHARACTER*24  VALK(2)
C ======================================================================
      CALL JEMARQ()
C ======================================================================
C --- INITIALISATION ---------------------------------------------------
C ======================================================================
      LCRIT  = 'RELATIF'
      LPREC  = 1.0D-06
      TMPTH1 = '&&RECHTH.TMPTH1'
      TMPTH2 = '&&RECHTH.TMPTH2'
      DEFTH1 = '&&RECHTH.DEFTH1'
      DEFTH2 = '&&RECHTH.DEFTH2'
      CALL JEVEUO (TBINTH, 'L', JINSTH)
C ======================================================================
C --- DETERMINATION DES INSTANTS THERMIQUES ----------------------------
C --- ENCADRANT L'INSTANT MECANIQUE ------------------------------------
C ======================================================================
      DO 10 ITH=2,NVAL2
         IF (TEMPS.LE.ZR(JINSTH+ITH-1)) THEN
            TEMPH1  = ZR(JINSTH+ITH-2)
            TEMPH2  = ZR(JINSTH+ITH-1)
            GOTO 20
         ENDIF
 10   CONTINUE
 20   CONTINUE
C ======================================================================
C --- RECUPERATION DES SOUS-TABLES ASSOCIEES A L'INSTANT COURANT -------
C ======================================================================
      CALL TBEXTB (TABTHR, 'V', TMPTH1, 1, 'INST', 'EQ',
     +             IBID, TEMPH1, CBID, K8B, LPREC, LCRIT, IRET )
      IF ( IRET .EQ. 10 ) THEN
         VALK(1) = 'INST'
         VALK(2) = TABTHR
         CALL U2MESK('F', 'UTILITAI7_1',2,VALK)
      ELSEIF ( IRET .EQ. 20 ) THEN
         VALK(1) = TABTHR
         VALK(2) = 'INST'
         CALL U2MESK('F', 'UTILITAI7_3',2,VALK)
      ENDIF
      CALL TBEXTB (TABTHR, 'V', TMPTH2, 1, 'INST', 'EQ',
     +             IBID, TEMPH2, CBID, K8B, LPREC, LCRIT, IRET )
      IF ( IRET .EQ. 10 ) THEN
         VALK(1) = 'INST'
         VALK(2) = TABTHR
         CALL U2MESK('F', 'UTILITAI7_1',2,VALK)
      ELSEIF ( IRET .EQ. 20 ) THEN
         VALK(1) = TABTHR
         VALK(2) = 'INST'
         CALL U2MESK('F', 'UTILITAI7_3',2,VALK)
      ENDIF
C ======================================================================
C --- RECUPERATION DE LA LISTE DE TEMPERATURE TEMPH1 -------------------
C ======================================================================
      CALL TBEXVE ( TMPTH1, 'TEMP', DEFTH1, 'V', NOTOT, K8B )
C ======================================================================
C --- RECUPERATION DE LA LISTE DE TEMPERATURE TEMPH2 -------------------
C ======================================================================
      CALL TBEXVE ( TMPTH2, 'TEMP', DEFTH2, 'V', IBID, K8B )
C ======================================================================
C --- RECUPERATION DES VECTEURS ASSOCIES -------------------------------
C ======================================================================
       CALL JEVEUO ( DEFTH1, 'L', JTETH1 )
       CALL JEVEUO ( DEFTH2, 'L', JTETH2 )
C ======================================================================
C --- CALCUL DE LA TEMPERATURE EN POINTE A, A L'INSTANT TEMPS ----------
C ======================================================================
      TEMPA = ZR(JTETH1-1+1) + ( ZR(JTETH2-1+1) - ZR(JTETH1-1+1) ) /
     +                         ( TEMPH2         - TEMPH1         ) *
     +                         ( TEMPS          - TEMPH1         )
C ======================================================================
C --- CALCUL DE LA TEMPERATURE EN POINTE B, A L'INSTANT TEMPS ----------
C ======================================================================
      TEMPB = ZR(JTETH1-1+NOTOT) +
     +                 ( ZR(JTETH2-1+NOTOT) - ZR(JTETH1-1+NOTOT) ) /
     +                 ( TEMPH2             - TEMPH1             ) *
     +                 ( TEMPS              - TEMPH1             )
C ======================================================================
C --- DESTRUCTION DES TABLES INUTILES ----------------------------------
C ======================================================================
      CALL JEDETC ('V', '&&RECHTH', 1)
C ======================================================================
      CALL JEDEMA( )
C ======================================================================
      END
