      SUBROUTINE PRCCM5 ( NOMRES, NBFT, NBORDR, PARASG, COURBE, LINTI,
     +                    INTITU, NOMMAT, PARA, SM, NCHEFF, RCCMPM,
     +                    RCCMSN, SNTHER, FATIZH, FATISP )
      IMPLICIT   NONE
      INTEGER             NBFT, NBORDR
      REAL*8              SM, PARA(*)
      LOGICAL             RCCMPM, RCCMSN, SNTHER, FATIZH, FATISP, LINTI
      CHARACTER*16        NCHEFF
      CHARACTER*(*)       NOMRES, COURBE, NOMMAT, PARASG, INTITU
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 23/01/2002   AUTEUR CIBHHLV L.VIVAN 
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
C     POST_RCCM: ON REMPLIT LA TABLE, TYPE_RESU: "VALE_INST"
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
      CHARACTER*32     JEXNUM, JEXNOM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER       NBCYCL, IOCC, N1, NBCHEF, ICHEF1, JINST, JPM, JPB,
     +              JPMPO, JPMPE, NUMORI, NUMEXT, IP, JINS2, JSNO, JSNE,
     +              JSPO, JSPE, JSNTO, JSNTE, VALOI, VALEI, NPARA,
     +              IP1, IP2, IP3, IP4, IP0, IK, NPARA0
      REAL*8        VALOR(30), VALER(30),
     +              KEO, KEE, SALTO, SALTE, NADMO, NADME, USAO, USAE
      COMPLEX*16    C16B
      CHARACTER*8   K8B, RESMEC, TYPARA(30)
      CHARACTER*16  VALOK(5), VALEK(5), TYPTAB, NOPARA(30)
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      TYPTAB = 'VALE_INST'
      NBCYCL = 1
C
      IK  = 1
      NPARA  = 1
      NOPARA(NPARA) = PARASG
      VALOK(IK) = COURBE
      VALEK(IK) = COURBE
C
      IK  = IK + 1
      NPARA  = NPARA + 1
      NOPARA(NPARA) = 'LIEU'
      VALOK(IK) = 'ORIG'
      VALEK(IK) = 'EXTR'
C
      IF ( LINTI ) THEN
         IK  = IK + 1
         NPARA = NPARA + 1
         NOPARA(NPARA) = 'INTITULE'
         VALOK(IK) = INTITU
         VALEK(IK) = INTITU
      ENDIF
C
      VALOR(1) = SM
      VALOR(2) = 3*SM
      VALOI    = NBCYCL
      NUMORI = 0
C
      VALER(1) = SM
      VALER(2) = 3*SM
      VALEI    = NBCYCL
      NUMEXT = 0
C
      IK  = IK + 1
      NPARA0 = NPARA
C
      IF ( RCCMPM .OR. RCCMSN .OR. FATISP ) THEN
C
         DO 10 IOCC = 1, NBFT
C
            IP0 = 2
            NPARA = NPARA0
C
            CALL GETVID('TRANSITOIRE','RESULTAT',IOCC,1,1,RESMEC,N1)
            VALOK(IK) = RESMEC
            VALEK(IK) = RESMEC
C
            IF ( RCCMPM ) THEN
C
               CALL PRCCM9 ( RCCMPM, .FALSE., .FALSE., .FALSE.,  
     +                      .FALSE., TYPTAB, NPARA, NOPARA, TYPARA )
               IP = IP0
               CALL JELIRA ( JEXNUM(NCHEFF//'.VALACCE',IOCC), 'LONMAX',
     +                                                     NBCHEF, K8B )
             CALL JEVEUO ( JEXNUM(NCHEFF//'.VALACCE',IOCC), 'L', JINST )
             CALL JEVEUO ( JEXNUM(NCHEFF//'.VALPM  ',IOCC), 'L', JPM   )
             CALL JEVEUO ( JEXNUM(NCHEFF//'.VALPB  ',IOCC), 'L', JPB   )
             CALL JEVEUO ( JEXNUM(NCHEFF//'.VALPMPO',IOCC), 'L', JPMPO )
             CALL JEVEUO ( JEXNUM(NCHEFF//'.VALPMPE',IOCC), 'L', JPMPE )
               IP = IP + 1
               IP1 = IP
               IP = IP + 1
               IP2 = IP
               IP = IP + 1
               IP3 = IP
               IP = IP + 1
               IP4 = IP
               DO 12 ICHEF1 = 1 , NBCHEF 
                  VALOR(IP1) = ZR(JINST+ICHEF1-1)
                  VALER(IP1) = ZR(JINST+ICHEF1-1)
                  VALOR(IP2) = ZR(JPM  +ICHEF1-1)
                  VALER(IP2) = ZR(JPM  +ICHEF1-1)
                  VALOR(IP3) = ZR(JPB  +ICHEF1-1)
                  VALER(IP3) = ZR(JPB  +ICHEF1-1)
                  VALOR(IP4) = ZR(JPMPO+ICHEF1-1)
                  VALER(IP4) = ZR(JPMPE+ICHEF1-1)
                  CALL TBAJLI ( NOMRES, NPARA, NOPARA, VALOI, VALOR,
     +                          C16B, VALOK, NUMORI )
                  CALL TBAJLI ( NOMRES, NPARA, NOPARA, VALEI, VALER,
     +                          C16B, VALEK, NUMEXT )
 12            CONTINUE
            ENDIF
C
            IF ( RCCMSN .OR. FATISP ) THEN
C
               CALL PRCCM9 ( .FALSE., RCCMSN, SNTHER, .FALSE., FATISP, 
     +                         TYPTAB, NPARA, NOPARA, TYPARA )
C
               CALL JELIRA ( JEXNUM(NCHEFF//'.INST1  ',IOCC), 'LONMAX',
     +                                                     NBCHEF, K8B )
             CALL JEVEUO ( JEXNUM(NCHEFF//'.INST1  ',IOCC), 'L', JINST )
             CALL JEVEUO ( JEXNUM(NCHEFF//'.INST2  ',IOCC), 'L', JINS2 )
             CALL JEVEUO ( JEXNUM(NCHEFF//'.VALSNO ',IOCC), 'L', JSNO  )
             CALL JEVEUO ( JEXNUM(NCHEFF//'.VALSNE ',IOCC), 'L', JSNE  )
             CALL JEVEUO ( JEXNUM(NCHEFF//'.VALSPO ',IOCC), 'L', JSPO  )
             CALL JEVEUO ( JEXNUM(NCHEFF//'.VALSPE ',IOCC), 'L', JSPE  )
               IF ( SNTHER ) THEN
             CALL JEVEUO ( JEXNUM(NCHEFF//'.VALSNTO',IOCC), 'L', JSNTO )
             CALL JEVEUO ( JEXNUM(NCHEFF//'.VALSNTE',IOCC), 'L', JSNTE )
               ENDIF
               DO 22 ICHEF1 = 1 , NBCHEF
                  IP = IP0
                  IP = IP + 1
                  VALOR(IP) = ZR(JINST+ICHEF1-1)
                  VALER(IP) = ZR(JINST+ICHEF1-1)
                  IP = IP + 1
                  VALOR(IP) = ZR(JINS2+ICHEF1-1)
                  VALER(IP) = ZR(JINS2+ICHEF1-1)
                  IP = IP + 1
                  IP1 = IP
                  VALOR(IP1) = ZR(JSNO +ICHEF1-1)
                  VALER(IP1) = ZR(JSNE +ICHEF1-1)
                  IF ( SNTHER ) THEN
                     IP = IP + 1
                     VALOR(IP) = ZR(JSNTO+ICHEF1-1)
                     VALER(IP) = ZR(JSNTE+ICHEF1-1)
                  ENDIF
                  IF ( FATISP ) THEN
                     IP = IP + 1
                     IP2 = IP
                     VALOR(IP2) = ZR(JSPO +ICHEF1-1)
                     VALER(IP2) = ZR(JSPE +ICHEF1-1)
                     CALL PRCCM3 ( NOMMAT, PARA, SM, VALOR(IP1), 
     +                          VALOR(IP2), KEO, SALTO, NADMO )
                     USAO = DBLE(NBCYCL) / NADMO
                     CALL PRCCM3 ( NOMMAT, PARA, SM, VALER(IP1), 
     +                          VALER(IP2), KEE, SALTE, NADME )
                     USAE = DBLE(NBCYCL) / NADME
                     IP = IP + 1
                     VALOR(IP) = KEO
                     VALER(IP) = KEE
                     IP = IP + 1
                     VALOR(IP) = SALTO
                     VALER(IP) = SALTE
                     IP = IP + 1
                     VALOR(IP) = NADMO
                     VALER(IP) = NADME
                     IP = IP + 1
                     VALOR(IP) = USAO
                     VALER(IP) = USAE
                  ENDIF
C
                  CALL TBAJLI ( NOMRES, NPARA, NOPARA, VALOI, VALOR,
     +                          C16B, VALOK, NUMORI )
                  CALL TBAJLI ( NOMRES, NPARA, NOPARA, VALEI, VALER,
     +                          C16B, VALEK, NUMEXT )
 22            CONTINUE
            ENDIF
 10      CONTINUE
C
      ENDIF
C
      IF ( FATIZH ) THEN
         CALL PRCCM8 ( NOMRES, NBFT, NBORDR, PARASG, COURBE, LINTI,
     +                 INTITU, SM, NCHEFF )
      ENDIF
C
      CALL JEDEMA()
      END
