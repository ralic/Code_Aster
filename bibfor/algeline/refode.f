      SUBROUTINE REFODE ( NBCMB, ANGLE, NOMCH, NUHARM, TYHARM, COEF,
     +                                                 BASZ, CHPRES )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           NBCMB,            NUHARM(*)
      CHARACTER*(*)                 NOMCH(*),BASZ,TYHARM(*),  CHPRES
      REAL*8                  ANGLE,                    COEF(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 11/09/2002   AUTEUR VABHHTS J.PELLET 
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
C     RECOMBINAISON DE FOURIER
C     -----------------------------------------------------------------
C     IN  NBCMB  : I   : NOMBRE DE CHAMPS A RECOMBINER
C     IN  ANGLE  : R8  : SECTION OU A LIEU LA RECOMBINAISON ( EN RD )
C     IN  NOMCH  : K8  : NOM DES CHAMPS A RECOMBINER
C     IN  NUHARM : I   : NUMERO DE L'HARMONIQUE
C     IN  TYHARM : K4  : TYPE DE L'HARMONIQUE (SYME OU ANTI)
C     IN  COEF   : R8  : COEF MULTIPLICATEUR ASSOCIE A L'HARMONIQUE
C     IN  CHPRES : K19 : NOM DU CHAMP RESULTAT
C     -----------------------------------------------------------------
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
      INTEGER      IBID, NBELEM, DIGDEL, MODE, CODE
      CHARACTER*1  BASE
      CHARACTER*4  DOCU
      CHARACTER*5  REFE ,DESC,VALE
      CHARACTER*8  K8B, NOMA, NOMGD
      CHARACTER*19 CH19, LIGREL
      LOGICAL      LMECA,LTHER
C     ------------------------------------------------------------------
      CALL JEMARQ()
      BASE = BASZ
      CH19 = NOMCH(1)

      CALL JEEXIN(CH19//'.DESC',IBID)
      IF (IBID.GT.0) THEN
        CALL JELIRA(CH19//'.DESC','DOCU',IBID,DOCU)
      ELSE
        CALL JELIRA(CH19//'.CELD','DOCU',IBID,DOCU)
      END IF

      IF (DOCU.EQ.'CHNO') THEN
         DESC = '.DESC'
         REFE = '.REFE'
         VALE = '.VALE'
      ELSEIF (DOCU.EQ.'CHML') THEN
         DESC = '.CELD'
         REFE = '.CELK'
         VALE = '.CELV'
      ELSE
         CALL UTMESS('F','REFODE','ON NE TRAITE QUE DES '//
     +                                  '"CHAM_NO" OU DES "CHAM_ELEM".')
      ENDIF

      LMECA = .FALSE.
      LTHER = .FALSE.
      CALL DISMOI('F','NOM_GD',CH19,'CHAMP',IBID,NOMGD,IE)
      IF ( NOMGD.EQ.'DEPL_R' .OR. NOMGD.EQ.'SIEF_R' ) THEN
         LMECA = .TRUE.
      ELSEIF( NOMGD.EQ.'TEMP_R' .OR. NOMGD.EQ.'FLUX_R' ) THEN
         LTHER = .TRUE.
      ENDIF
      CALL DISMOI('F','NB_EC',NOMGD,'GRANDEUR',NBEC,K8B,IER)
C
C     --- CONSTRUCTION D'UN CHAMP RESULTAT SUR LE MODELE DE NOMCH(1)
C
      CALL JELIRA(CH19//DESC  ,'LONMAX',NBDESC,K8B)
      CALL JELIRA(CH19//VALE  ,'LONMAX',NBVALE,K8B)
      CALL JELIRA(CH19//REFE,'LONMAX',NBREFE,K8B)
      CALL JEVEUO(CH19//DESC  ,'L',JDESC)
      CALL JEVEUO(CH19//REFE,'L',JREFE)
C
      CH19 = CHPRES
      CALL JEEXIN(CH19//VALE,IRET)
      IF ( IRET .EQ. 0 ) THEN
        CALL WKVECT(CH19//DESC  ,BASE//' V I'  ,NBDESC,KDESC)
        CALL WKVECT(CH19//VALE  ,BASE//' V R'  ,NBVALE,KVALE)
        CALL WKVECT(CH19//REFE,BASE//' V K24',NBREFE,KREFE)
      ELSE
        CALL JEVEUO(CH19//DESC  ,'E',KDESC)
        CALL JEVEUO(CH19//VALE  ,'E',KVALE)
        CALL JEVEUO(CH19//REFE,'E',KREFE)
      ENDIF
C
      CALL JEECRA(CH19//DESC,'DOCU',IBID,DOCU)
      DO 10 I = 0,NBDESC-1
        ZI(KDESC+I) = ZI(JDESC+I)
 10   CONTINUE
      CALL JELIBE(CH19//DESC)
C

      CALL WKVECT('&&REFODE.VALE','V V R',NBVALE,LVALE)
C
      IF (DOCU.EQ.'CHNO') THEN
         CALL JEVEUO(ZK24(JREFE+1)(1:19)//'.PRNO','L',JPRNO)
         DO 20 I = 0,NBREFE-1
            ZK24(KREFE+I) = ZK24(JREFE+I)
 20      CONTINUE
         CALL JELIBE(CH19//'.REFE')
         CALL DISMOI('F','NOM_MAILLA',NOMCH(1),'CHAMP',IBID,NOMA,IE)
         CALL JELIRA(NOMA//'.NOMNOE','NOMMAX',NBNOEU,K8B)
C
C        --- BOUCLE SUR LES CHAMPS A RECOMBINER ---
C
         DO 100 IM = 1,NBCMB
            ANG = ANGLE * DBLE(NUHARM(IM))
            CH19 = NOMCH(IM)
            CALL JEVEUO(CH19//VALE,'L',JVALE)
C
            IF ( LMECA ) THEN
C
            IF (TYHARM(IM)(1:4).EQ.'SYME') THEN
C
               DO 110 INO = 1,NBNOEU
                  I = ZI(JPRNO+(INO-1)*(NBEC+2))-2
                  IF (I.NE.-2) THEN
                  ZR(LVALE+I+1) = ZR(LVALE+I+1) + COEF(IM)*COS(ANG)*
     +                                             ZR(JVALE+I+1)
                  ZR(LVALE+I+2) = ZR(LVALE+I+2) + COEF(IM)*COS(ANG)*
     +                                             ZR(JVALE+I+2)
                  ZR(LVALE+I+3) = ZR(LVALE+I+3) - COEF(IM)*SIN(ANG)*
     +                                             ZR(JVALE+I+3)
                  ENDIF
110            CONTINUE
C
            ELSEIF (TYHARM(IM)(1:4).EQ.'ANTI') THEN
C
               DO 112 INO = 1,NBNOEU
                  I = ZI(JPRNO+(INO-1)*(NBEC+2))-2
                  IF (I.NE.-2) THEN
                  ZR(LVALE+I+1) = ZR(LVALE+I+1) + COEF(IM)*SIN(ANG)*
     +                                             ZR(JVALE+I+1)
                  ZR(LVALE+I+2) = ZR(LVALE+I+2) + COEF(IM)*SIN(ANG)*
     +                                             ZR(JVALE+I+2)
                  ZR(LVALE+I+3) = ZR(LVALE+I+3) + COEF(IM)*COS(ANG)*
     +                                             ZR(JVALE+I+3)
                  ENDIF
 112           CONTINUE
C
            ELSEIF (TYHARM(IM)(1:4).EQ.'TOUS') THEN
C
               DO 114 INO = 1,NBNOEU
                  I = ZI(JPRNO+(INO-1)*(NBEC+2))-2
                  IF (I.NE.-2) THEN
                  ZR(LVALE+I+1) = ZR(LVALE+I+1)
     +                                + COEF(IM)*SIN(ANG)*ZR(JVALE+I+1)
     +                                + COEF(IM)*COS(ANG)*ZR(JVALE+I+1)
                  ZR(LVALE+I+2) = ZR(LVALE+I+2)
     +                                + COEF(IM)*SIN(ANG)*ZR(JVALE+I+2)
     +                                + COEF(IM)*COS(ANG)*ZR(JVALE+I+2)
                  ZR(LVALE+I+3) = ZR(LVALE+I+3)
     +                                + COEF(IM)*COS(ANG)*ZR(JVALE+I+3)
     +                                - COEF(IM)*SIN(ANG)*ZR(JVALE+I+3)
                  ENDIF
 114           CONTINUE
C
            ENDIF
C
            ELSEIF ( LTHER ) THEN
C
            IF (TYHARM(IM)(1:4).EQ.'SYME') THEN
C
               DO 120 INO = 1,NBNOEU
                  I = ZI(JPRNO+(INO-1)*(NBEC+2))-2
                  IF (I.NE.-2) THEN
                  ZR(LVALE+I+1) = ZR(LVALE+I+1) + COEF(IM)*COS(ANG)*
     +                                             ZR(JVALE+I+1)
                  ENDIF
 120           CONTINUE
C
            ELSEIF (TYHARM(IM)(1:4).EQ.'ANTI') THEN
C
               DO 122 INO = 1,NBNOEU
                  I = ZI(JPRNO+(INO-1)*(NBEC+2))-2
                  IF (I.NE.-2) THEN
                  ZR(LVALE+I+1) = ZR(LVALE+I+1) + COEF(IM)*SIN(ANG)*
     +                                             ZR(JVALE+I+1)
                  ENDIF
 122           CONTINUE
C
            ELSEIF (TYHARM(IM)(1:4).EQ.'TOUS') THEN
C
               DO 124 INO = 1,NBNOEU
                  I = ZI(JPRNO+(INO-1)*(NBEC+2))-2
                  IF (I.NE.-2) THEN
                  ZR(LVALE+I+1) = ZR(LVALE+I+1)
     +                                + COEF(IM)*SIN(ANG)*ZR(JVALE+I+1)
     +                                + COEF(IM)*COS(ANG)*ZR(JVALE+I+1)
                  ENDIF
 124           CONTINUE
C
            ENDIF
            ENDIF
  100    CONTINUE
C
      ELSEIF (DOCU.EQ.'CHML') THEN
         DO 22 I = 0,NBREFE-1
            ZK24(KREFE+I) = ZK24(JREFE+I)
 22      CONTINUE
         CALL JELIBE(CH19//'.CELK')
C
         DO 200 IM = 1,NBCMB
            I1 = -1
            ANG = ANGLE * DBLE(NUHARM(IM))
            CH19 = NOMCH(IM)

C           -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
            CALL CELVER(CH19,'NBVARI_CST','STOP',IBID)
            CALL CELVER(CH19,'NBSPT_1','STOP',IBID)

            CALL JEVEUO(CH19//'.CELD','L',JCELD)
            CALL JEVEUO(CH19//'.CELK','L',JCELK)
            CALL JEVEUO(CH19//'.CELV','L',JCELV)
            NBGR = ZI(JCELD-1+2)
            LIGREL = ZK24(JCELK)(1:19)
C
            DO 210 IGREL = 1,NBGR
               MODE=ZI(JCELD-1+ZI(JCELD-1+4+IGREL) +2)
               IF ( MODE .EQ. 0 ) GOTO 210
               NBSCAL = DIGDEL(MODE)
               ICOEF=MAX(1,ZI(JCELD-1+4))
               IF (ICOEF.NE.1) THEN
                  CALL UTMESS('F','REFODE','ON NE TRAITE PAS CE TYPE '//
     +                            'DE CHAM_ELEM, ICOEF DIFFERENT DE 1')
               ENDIF
               NBELGR = NBELEM(LIGREL,IGREL)
               IDECGR=ZI(JCELD-1+ZI(JCELD-1+4+IGREL)+8)
C
               IF ( LMECA ) THEN
C              --- ON EST EN AXIS, IL Y A 6 COMPOSANTES PAR POINT ---
               NBPT = NBSCAL / 6
C
               IF (TYHARM(IM)(1:4).EQ.'SYME') THEN
C
                  DO 220 K = 1,NBELGR
                     IC = -1
                     DO 222 IP = 1,NBPT
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1) + COEF(IM)*COS(ANG)*
     +                                ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1) + COEF(IM)*COS(ANG)*
     +                                ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1) + COEF(IM)*COS(ANG)*
     +                                ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1) + COEF(IM)*COS(ANG)*
     +                                ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1) - COEF(IM)*SIN(ANG)*
     +                                ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1) - COEF(IM)*SIN(ANG)*
     +                                ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
 222                 CONTINUE
 220              CONTINUE
C
               ELSEIF (TYHARM(IM)(1:4).EQ.'ANTI') THEN
C
                  DO 230 K = 1,NBELGR
                     IC = -1
                     DO 232 IP = 1,NBPT
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1) + COEF(IM)*SIN(ANG)*
     +                                ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1) + COEF(IM)*SIN(ANG)*
     +                                ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1) + COEF(IM)*SIN(ANG)*
     +                                ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1) + COEF(IM)*SIN(ANG)*
     +                                ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1) + COEF(IM)*COS(ANG)*
     +                                ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1) + COEF(IM)*COS(ANG)*
     +                                ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
 232                 CONTINUE
 230              CONTINUE
C
               ELSEIF (TYHARM(IM)(1:4).EQ.'TOUS') THEN
C
                  DO 260 K = 1,NBELGR
                     IC = -1
                     DO 262 IP = 1,NBPT
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1)
     +           + COEF(IM)*SIN(ANG)*ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
     +           + COEF(IM)*COS(ANG)*ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1)
     +           + COEF(IM)*SIN(ANG)*ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
     +           + COEF(IM)*COS(ANG)*ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1)
     +           + COEF(IM)*SIN(ANG)*ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
     +           + COEF(IM)*COS(ANG)*ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1)
     +           + COEF(IM)*SIN(ANG)*ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
     +           + COEF(IM)*COS(ANG)*ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1)
     +           + COEF(IM)*COS(ANG)*ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
     +           - COEF(IM)*SIN(ANG)*ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1)
     +           + COEF(IM)*COS(ANG)*ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
     +           - COEF(IM)*SIN(ANG)*ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
 262                 CONTINUE
 260              CONTINUE
               ENDIF
C
               ELSEIF ( LTHER ) THEN
C              --- ON EST EN AXIS, IL Y A 3 COMPOSANTES PAR POINT ---
               NBPT = NBSCAL / 3
C
               IF (TYHARM(IM)(1:4).EQ.'SYME') THEN
C
                  DO 240 K = 1,NBELGR
                     IC = -1
                     DO 242 IP = 1,NBPT
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1) + COEF(IM)*COS(ANG)*
     +                                ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1) + COEF(IM)*COS(ANG)*
     +                                ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1) - COEF(IM)*SIN(ANG)*
     +                                ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
 242                 CONTINUE
 240              CONTINUE
C
               ELSEIF (TYHARM(IM)(1:4).EQ.'ANTI') THEN
C
                  DO 250 K = 1,NBELGR
                     IC = -1
                     DO 252 IP = 1,NBPT
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1) + COEF(IM)*SIN(ANG)*
     +                                ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1) + COEF(IM)*SIN(ANG)*
     +                                ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1) + COEF(IM)*COS(ANG)*
     +                                ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
 252                 CONTINUE
 250              CONTINUE
C
               ELSEIF (TYHARM(IM)(1:4).EQ.'TOUS') THEN
C
                  DO 270 K = 1,NBELGR
                     IC = -1
                     DO 272 IP = 1,NBPT
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1)
     +           + COEF(IM)*SIN(ANG)*ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
     +           + COEF(IM)*COS(ANG)*ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1)
     +           + COEF(IM)*SIN(ANG)*ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
     +           + COEF(IM)*COS(ANG)*ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
                        I1 = I1 + 1
                        IC = IC + 1
                        ZR(LVALE+I1) = ZR(LVALE+I1)
     +           + COEF(IM)*COS(ANG)*ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
     +           - COEF(IM)*SIN(ANG)*ZR(JCELV-1+IDECGR+(K-1)*NBSCAL+IC)
 272                 CONTINUE
 270              CONTINUE
               ENDIF
               ENDIF
 210        CONTINUE
            CALL JELIBE(CH19//DESC)
            CALL JELIBE(CH19//'.CELK')
            CALL JELIBE(CH19//VALE)
C
  200    CONTINUE
      ENDIF
C
      DO 500 IVAL = 0,NBVALE-1
         ZR(KVALE+IVAL) = ZR(LVALE+IVAL)
 500  CONTINUE
      CH19 = CHPRES
      CALL JELIBE(CH19//VALE)
      CALL JEDETR('&&REFODE.VALE')
C
      CALL JEDEMA()
      END
