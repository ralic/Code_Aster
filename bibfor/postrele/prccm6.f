      SUBROUTINE PRCCM6 ( NOMRES, NBFT, NBORDR, PARASG, COURBE, LINTI,
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
C'
C     POST_RCCM: ON REMPLIT LA TABLE POUR LE TYPE_RESU: 'VALE_MAX'
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
     +              JSPO, JSPE, JSNTO, JSNTE, VALOI, VALEI, NPARA, I, J,
     +              JPMO1, JPMO2, JPMO3, JPMO4, JPMO5, JPMO6, JVALE,
     +              JPME1, JPME2, JPME3, JPME4, JPME5, JPME6, IBID,NBVAL
      INTEGER       IP1, IP2, IP3, IP4, IP5, IP6, IP7, IP8,IP9, IP10,
     +              IP11, IP12, IP13, IP14, IP15, IP16, IP17, IP18, IK
      REAL*8        VALOR(36), VALER(36), XMAX, TMAX(6),
     +              KEO, KEE, SALTO, SALTE, NADMO, NADME, USAO, USAE
      COMPLEX*16    C16B
      CHARACTER*8   K8B, RESMEC, TYPARA(36)
      CHARACTER*16  VALOK(5), VALEK(5), TYPTAB, NOPARA(36), MPAR(6)
      CHARACTER*24  NOMJV1
C
      DATA  MPAR / 'MAX_SIXX' , 'MAX_SIYY' , 'MAX_SIZZ' , 'MAX_SIXY' , 
     +             'MAX_SIXZ' , 'MAX_SIYZ' /
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      TYPTAB = 'VALE_MAX'
      NBCYCL = 1
      VALOI = NBCYCL
      VALEI = NBCYCL
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
C
      VALER(1) = SM
      VALER(2) = 3*SM
C
      IK  = IK + 1
C
      IF ( RCCMPM .OR. RCCMSN .OR. FATISP ) THEN
C
         CALL PRCCM9 ( RCCMPM, RCCMSN, SNTHER, .FALSE., FATISP, 
     +                         TYPTAB, NPARA, NOPARA, TYPARA )
C
      DO 10 IOCC = 1, NBFT
C
         IP = 2
C
         CALL GETVID('TRANSITOIRE','RESULTAT',IOCC,1,1,RESMEC,N1)
         VALOK(IK) = RESMEC
         VALEK(IK) = RESMEC
C
         IF ( RCCMPM ) THEN
            CALL JELIRA ( JEXNUM(NCHEFF//'.LSCHEFF',IOCC), 'LONMAX',
     +                                                     NBCHEF, K8B )
            CALL JEVEUO ( JEXNUM(NCHEFF//'.VALACCE',IOCC), 'L', JINST )
            CALL JEVEUO ( JEXNUM(NCHEFF//'.VALPM  ',IOCC), 'L', JPM   )
            CALL JEVEUO ( JEXNUM(NCHEFF//'.VALPB  ',IOCC), 'L', JPB   )
            CALL JEVEUO ( JEXNUM(NCHEFF//'.VALPMPO',IOCC), 'L', JPMPO )
            CALL JEVEUO ( JEXNUM(NCHEFF//'.VALPMPE',IOCC), 'L', JPMPE )
            CALL JEVEUO ( JEXNUM(NCHEFF//'.VPMPBO1',IOCC), 'L', JPMO1 )
            CALL JEVEUO ( JEXNUM(NCHEFF//'.VPMPBO2',IOCC), 'L', JPMO2 )
            CALL JEVEUO ( JEXNUM(NCHEFF//'.VPMPBO3',IOCC), 'L', JPMO3 )
            CALL JEVEUO ( JEXNUM(NCHEFF//'.VPMPBO4',IOCC), 'L', JPMO4 )
            CALL JEVEUO ( JEXNUM(NCHEFF//'.VPMPBO5',IOCC), 'L', JPMO5 )
            CALL JEVEUO ( JEXNUM(NCHEFF//'.VPMPBO6',IOCC), 'L', JPMO6 )
            CALL JEVEUO ( JEXNUM(NCHEFF//'.VPMPBE1',IOCC), 'L', JPME1 )
            CALL JEVEUO ( JEXNUM(NCHEFF//'.VPMPBE2',IOCC), 'L', JPME2 )
            CALL JEVEUO ( JEXNUM(NCHEFF//'.VPMPBE3',IOCC), 'L', JPME3 )
            CALL JEVEUO ( JEXNUM(NCHEFF//'.VPMPBE4',IOCC), 'L', JPME4 )
            CALL JEVEUO ( JEXNUM(NCHEFF//'.VPMPBE5',IOCC), 'L', JPME5 )
            CALL JEVEUO ( JEXNUM(NCHEFF//'.VPMPBE6',IOCC), 'L', JPME6 )
            ICHEF1 = 1
            IP = IP + 1
            IP1 = IP
            VALOR(IP1) = ZR(JINST+ICHEF1-1)
            IP = IP + 1
            IP2 = IP
            VALOR(IP2) = ZR(JPM  +ICHEF1-1)
            IP = IP + 1
            IP3 = IP
            VALOR(IP3) = ZR(JINST+ICHEF1-1)
            IP = IP + 1
            IP4 = IP
            VALOR(IP4) = ZR(JPB  +ICHEF1-1)
            IP = IP + 1
            IP5 = IP
            VALOR(IP5) = ZR(JINST+ICHEF1-1)
            VALER(IP5) = ZR(JINST+ICHEF1-1)
            IP = IP + 1
            IP6 = IP
            VALOR(IP6) = ZR(JPMPO+ICHEF1-1)
            VALER(IP6) = ZR(JPMPE+ICHEF1-1)
            IP = IP + 1
            IP7 = IP
            IP13 = IP
            VALOR(IP7) = ZR(JPMO1+ICHEF1-1)
            VALER(IP13) = ZR(JPME1+ICHEF1-1)
            IP = IP + 1
            IP8 = IP
            IP14 = IP
            VALOR(IP8) = ZR(JPMO2+ICHEF1-1)
            VALER(IP14) = ZR(JPME2+ICHEF1-1)
            IP = IP + 1
            IP9 = IP
            IP15 = IP
            VALOR(IP9) = ZR(JPMO3+ICHEF1-1)
            VALER(IP15) = ZR(JPME3+ICHEF1-1)
            IP = IP + 1
            IP10 = IP
            IP16 = IP
            VALOR(IP10) = ZR(JPMO4+ICHEF1-1)
            VALER(IP16) = ZR(JPME4+ICHEF1-1)
            IP = IP + 1
            IP11 = IP
            IP17 = IP
            VALOR(IP11) = ZR(JPMO5+ICHEF1-1)
            VALER(IP17) = ZR(JPME5+ICHEF1-1)
            IP = IP + 1
            IP12 = IP
            IP18 = IP
            VALOR(IP12) = ZR(JPMO6+ICHEF1-1)
            VALER(IP18) = ZR(JPME6+ICHEF1-1)
            DO 12 ICHEF1 = 2 , NBCHEF 
               IF ( ZR(JPM  +ICHEF1-1) .GT. VALOR(IP2) ) THEN
                  VALOR(IP1) = ZR(JINST+ICHEF1-1)
                  VALOR(IP2) = ZR(JPM  +ICHEF1-1)
               ENDIF
               IF ( ZR(JPB  +ICHEF1-1) .GT. VALOR(IP4) ) THEN
                  VALOR(IP3) = ZR(JINST+ICHEF1-1)
                  VALOR(IP4) = ZR(JPB  +ICHEF1-1)
               ENDIF
               IF ( ZR(JPMPO+ICHEF1-1) .GT. VALOR(IP6) ) THEN
                  VALOR(IP5) = ZR(JINST+ICHEF1-1)
                  VALOR(IP6) = ZR(JPMPO+ICHEF1-1)
               ENDIF
               IF ( ZR(JPMPE+ICHEF1-1) .GT. VALER(IP6) ) THEN
                  VALER(IP5) = ZR(JINST+ICHEF1-1)
                  VALER(IP6) = ZR(JPMPE+ICHEF1-1)
               ENDIF
               IF ( ZR(JPMO1+ICHEF1-1) .GT. VALOR(IP7) ) THEN
                  VALOR(IP7) = ZR(JPMO1+ICHEF1-1)
               ENDIF
               IF ( ZR(JPMO2+ICHEF1-1) .GT. VALOR(IP8) ) THEN
                  VALOR(IP8) = ZR(JPMO2+ICHEF1-1)
               ENDIF
               IF ( ZR(JPMO3+ICHEF1-1) .GT. VALOR(IP9) ) THEN
                  VALOR(IP9) = ZR(JPMO3+ICHEF1-1)
               ENDIF
               IF ( ZR(JPMO4+ICHEF1-1) .GT. VALOR(IP10) ) THEN
                  VALOR(IP10) = ZR(JPMO4+ICHEF1-1)
               ENDIF
               IF ( ZR(JPMO5+ICHEF1-1) .GT. VALOR(IP11) ) THEN
                  VALOR(IP11) = ZR(JPMO5+ICHEF1-1)
               ENDIF
               IF ( ZR(JPMO6+ICHEF1-1) .GT. VALOR(IP12) ) THEN
                  VALOR(IP12) = ZR(JPMO6+ICHEF1-1)
               ENDIF
               IF ( ZR(JPME1+ICHEF1-1) .GT. VALER(IP13) ) THEN
                  VALER(IP13) = ZR(JPME1+ICHEF1-1)
               ENDIF
               IF ( ZR(JPME2+ICHEF1-1) .GT. VALER(IP14) ) THEN
                  VALER(IP14) = ZR(JPME2+ICHEF1-1)
               ENDIF
               IF ( ZR(JPME3+ICHEF1-1) .GT. VALER(IP15) ) THEN
                  VALER(IP15) = ZR(JPME3+ICHEF1-1)
               ENDIF
               IF ( ZR(JPME4+ICHEF1-1) .GT. VALER(IP16) ) THEN
                  VALER(IP16) = ZR(JPME4+ICHEF1-1)
               ENDIF
               IF ( ZR(JPME5+ICHEF1-1) .GT. VALER(IP17) ) THEN
                  VALER(IP17) = ZR(JPME5+ICHEF1-1)
               ENDIF
               IF ( ZR(JPME6+ICHEF1-1) .GT. VALER(IP18) ) THEN
                  VALER(IP18) = ZR(JPME6+ICHEF1-1)
               ENDIF
 12         CONTINUE
            VALER(IP1) = VALOR(IP1)
            VALER(IP2) = VALOR(IP2)
            VALER(IP3) = VALOR(IP3)
            VALER(IP4) = VALOR(IP4)
         ENDIF
C
         IF ( RCCMSN .OR. FATISP ) THEN
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
            ICHEF1 = 1
            IP = IP + 1
            IP1 = IP
            VALOR(IP1) = ZR(JINST+ICHEF1-1)
            IP4 = IP
            VALER(IP4) = ZR(JINST+ICHEF1-1)
            IP = IP + 1
            IP2 = IP
            VALOR(IP2) = ZR(JINS2+ICHEF1-1)
            IP5 = IP
            VALER(IP5) = ZR(JINS2+ICHEF1-1)
            IP = IP + 1
            IP3 = IP
            VALOR(IP3) = ZR(JSNO +ICHEF1-1)
            IP6 = IP
            VALER(IP6) = ZR(JSNE +ICHEF1-1)
            IF ( SNTHER ) THEN
               IP = IP + 1
               IP7 = IP
               VALOR(IP7)  = ZR(JINST+ICHEF1-1)
               IP10 = IP
               VALER(IP10) = ZR(JINST+ICHEF1-1)
               IP = IP + 1
               IP8 = IP
               VALOR(IP8)  = ZR(JINS2+ICHEF1-1)
               IP11 = IP
               VALER(IP11) = ZR(JINS2+ICHEF1-1)
               IP = IP + 1
               IP9 = IP
               VALOR(IP9)  = ZR(JSNTO+ICHEF1-1)
               IP12 = IP
               VALER(IP12) = ZR(JSNTE+ICHEF1-1)
            ENDIF
            IF ( FATISP ) THEN
               IP = IP + 1
               IP13 = IP
               VALOR(IP13) = ZR(JINST+ICHEF1-1)
               IP16 = IP
               VALER(IP16) = ZR(JINST+ICHEF1-1)
               IP = IP + 1
               IP14 = IP
               VALOR(IP14) = ZR(JINS2+ICHEF1-1)
               IP17 = IP
               VALER(IP17) = ZR(JINS2+ICHEF1-1)
               IP = IP + 1
               IP15 = IP
               VALOR(IP15) = ZR(JSPO+ICHEF1-1)
               IP18 = IP
               VALER(IP18) = ZR(JSPE+ICHEF1-1)
            ENDIF
            DO 22 ICHEF1 = 2 , NBCHEF
               IF ( ZR(JSNO +ICHEF1-1) .GT. VALOR(IP3) ) THEN
                  VALOR(IP1) = ZR(JINST+ICHEF1-1)
                  VALOR(IP2) = ZR(JINS2+ICHEF1-1)
                  VALOR(IP3) = ZR(JSNO +ICHEF1-1)
               ENDIF
               IF ( ZR(JSNE +ICHEF1-1) .GT. VALER(IP6) ) THEN
                  VALER(IP4) = ZR(JINST+ICHEF1-1)
                  VALER(IP5) = ZR(JINS2+ICHEF1-1)
                  VALER(IP6) = ZR(JSNE +ICHEF1-1)
               ENDIF
               IF ( SNTHER ) THEN
                  IF ( ZR(JSNTO+ICHEF1-1) .GT. VALOR(IP9) ) THEN
                     VALOR(IP7) = ZR(JINST+ICHEF1-1)
                     VALOR(IP8) = ZR(JINS2+ICHEF1-1)
                     VALOR(IP9) = ZR(JSNTO+ICHEF1-1)
                  ENDIF
                  IF ( ZR(JSNTE+ICHEF1-1) .GT. VALER(IP12) ) THEN
                     VALER(IP10) = ZR(JINST+ICHEF1-1)
                     VALER(IP11) = ZR(JINS2+ICHEF1-1)
                     VALER(IP12) = ZR(JSNTE+ICHEF1-1)
                  ENDIF
               ENDIF
               IF ( FATISP ) THEN
                  IF ( ZR(JSPO+ICHEF1-1) .GT. VALOR(IP15) ) THEN
                     VALOR(IP13) = ZR(JINST+ICHEF1-1)
                     VALOR(IP14) = ZR(JINS2+ICHEF1-1)
                     VALOR(IP15) = ZR(JSPO +ICHEF1-1)
                  ENDIF
                  IF ( ZR(JSPE+ICHEF1-1) .GT. VALER(IP18) ) THEN
                     VALER(IP16) = ZR(JINST+ICHEF1-1)
                     VALER(IP17) = ZR(JINS2+ICHEF1-1)
                     VALER(IP18) = ZR(JSPE +ICHEF1-1)
                  ENDIF
               ENDIF
 22         CONTINUE
            IF ( FATISP ) THEN
               CALL PRCCM3 ( NOMMAT, PARA, SM, VALOR(IP3), VALOR(IP15),
     +                       KEO, SALTO, NADMO )
               USAO = DBLE(NBCYCL) / NADMO
               CALL PRCCM3 ( NOMMAT, PARA, SM, VALER(IP6), VALER(IP18),
     +                       KEE, SALTE, NADME )
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
         ENDIF
         NUMORI = 0
         CALL TBAJLI ( NOMRES, NPARA, NOPARA, VALOI, VALOR,
     +                          C16B, VALOK, NUMORI )
         NUMEXT = 0
         CALL TBAJLI ( NOMRES, NPARA, NOPARA, VALEI, VALER,
     +                          C16B, VALEK, NUMEXT )
 10   CONTINUE
C
         IF ( RCCMPM ) THEN
            NUMORI = 0
            DO 30 I = 1, 6
               NOMJV1 = '&&PRCCM6.MAX_MAX'
               CALL TBEXVE ( NOMRES, MPAR(I), NOMJV1, 'V', NBVAL, K8B )
               CALL JEVEUO ( NOMJV1, 'L', JVALE )
               XMAX = ZR(JVALE)
               DO 32 J = 2, NBVAL
                  XMAX = MAX ( XMAX , ZR(JVALE+J-1) )
 32            CONTINUE
               TMAX(I) = XMAX
               CALL JEDETR ( NOMJV1 )
 30         CONTINUE
            CALL TBAJLI ( NOMRES, 6, MPAR, IBID, TMAX,
     +                                     C16B, K8B, NUMORI )
         ENDIF
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
