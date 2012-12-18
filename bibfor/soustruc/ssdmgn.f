      SUBROUTINE SSDMGN(MAG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 18/12/2012   AUTEUR SELLENET N.SELLENET 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT NONE
C     ARGUMENTS:
C     ----------
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
      CHARACTER*8 MAG
C ----------------------------------------------------------------------
C     BUT:
C        - TRAITER LE MOT CLEF "DEFI_GROUP_NO"
C          DE LA COMMANDE DEFI_MAILLAGE.
C        - CREER LES OBJETS :
C            BASE GLOBALE : .GROUPENO
C
C     IN:
C        MAG : NOM DU MAILLAGE QUE L'ON DEFINIT.
C
      CHARACTER*8  NOMACR,NOMAIL,KBID,MAL,NOSMA,PREF
      CHARACTER*24 NOMGNL,NOMGNG
      INTEGER INDI(4)
      LOGICAL UNAUN
C ----------------------------------------------------------------------
      CHARACTER*24 VALK(2)
      INTEGER      IARG
C
C-----------------------------------------------------------------------
      INTEGER I1 ,I1NOE ,IADIM2 ,IADIME ,IAGNL ,IAGNO ,IALINO
      INTEGER IANMCR ,IAWK1 ,IBID ,IED ,IGNO ,II ,INOL
      INTEGER IOCC ,IRET ,ISMA ,KK ,LGNL ,LMAIL ,LONGT
      INTEGER LONT ,LPREF ,N ,N1 ,N2 ,N3 ,NBGNO
      INTEGER NBGNO2 ,NBGNOT ,NBID ,NBNO ,NBNOEX ,NBSMA ,NOCC
      INTEGER NUSMA,INDIIS
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CALL JEVEUO(MAG//'.DIME','L',IADIME)
      CALL JEVEUO(MAG//'.DIME_2','L',IADIM2)
      CALL JEVEUO(MAG//'.NOMACR','L',IANMCR)
      NBSMA= ZI(IADIME-1+4)
C
C
C
C     --1 (SUR)DIMENSIONNEMENT :
C     --------------------------
      CALL GETFAC('DEFI_GROUP_NO',NOCC)
C
      NBGNOT=0
      LONT= 0
      DO 2, IOCC=1,NOCC
        CALL GETVIS('DEFI_GROUP_NO','INDEX',IOCC,IARG,4,INDI,N1)
        IF (N1.EQ.4) THEN
          UNAUN=.FALSE.
        ELSE
          CALL GETVTX('DEFI_GROUP_NO','GROUP_NO_FIN',IOCC,IARG,1,
     &                KBID,N2)
          CALL ASSERT(N2.NE.0)
          UNAUN=.TRUE.
        END IF
C
C
C     --1.1 CAS : INDEX, TOUT OU MAILLE :
C     -----------------------------------
        IF (.NOT.UNAUN) THEN
          CALL GETVTX('DEFI_GROUP_NO','TOUT',IOCC,IARG,1,KBID,N1)
          CALL GETVTX('DEFI_GROUP_NO','SUPER_MAILLE',IOCC,IARG,1,
     &                NOSMA,N2)
          IF (N2.EQ.1) THEN
            CALL JEEXIN(JEXNOM(MAG//'.SUPMAIL',NOSMA),IRET)
            IF (IRET.EQ.0) THEN
              VALK(1) = NOSMA
              VALK(2) = MAG
              CALL U2MESK('F','SOUSTRUC_26', 2 ,VALK)
            ENDIF
            CALL JENONU(JEXNOM(MAG//'.SUPMAIL',NOSMA),NUSMA)
          END IF
C
          DO 21, ISMA=1,NBSMA
            IF((N2.EQ.1).AND.(NUSMA.NE.ISMA)) GO TO 21
            NOMACR= ZK8(IANMCR-1+ISMA)
            CALL DISMOI('F','NOM_MAILLA',NOMACR,'MACR_ELEM_STAT'
     &                    ,IBID,MAL,IED)
            CALL JEEXIN(MAL//'.GROUPENO',IRET)
            IF (IRET.EQ.0) GO TO 21
            CALL JELIRA(MAL//'.GROUPENO','NUTIOC',NBGNO,KBID)
            NBGNOT= NBGNOT+NBGNO
            DO 211, IGNO=1,NBGNO
              CALL JELIRA(JEXNUM(MAL//'.GROUPENO',IGNO),
     &                 'LONMAX',N3,KBID)
              LONT= LONT+N3
 211        CONTINUE
 21       CONTINUE
C
C
C     --3.2 CAS : MAILLE, GROUP_NO_FIN, GROUP_NO_INIT:
C     -----------------------------------------------
        ELSE
          CALL GETVTX('DEFI_GROUP_NO','SUPER_MAILLE',IOCC,IARG,1,
     &                NOSMA,N1)
          CALL GETVTX('DEFI_GROUP_NO','GROUP_NO_INIT',IOCC,IARG,1,
     &                NOMGNL,N)
C
          CALL JENONU(JEXNOM(MAG//'.SUPMAIL',NOSMA),ISMA)
          NOMACR= ZK8(IANMCR-1+ISMA)
          CALL DISMOI('F','NOM_MAILLA',NOMACR,'MACR_ELEM_STAT'
     &                    ,IBID,MAL,IED)
          CALL JELIRA(JEXNOM(MAL//'.GROUPENO',NOMGNL),'LONUTI',N3,KBID)
          NBGNOT= NBGNOT+1
          LONT=LONT+N3
        END IF
 2    CONTINUE
C
C     -- SI LONT = 0 ON S'ARRETE LA. (PLUS BAS : BUG ????)
      IF ((LONT.EQ.0).OR.(NBGNOT.EQ.0)) GO TO 9999
C
C
C
C
C     --2 ALLOCATION:
C     ---------------
C     --ON SURDIMENSIONNE LE NOMBRE MAX D'OBJETS DE LA COLLECTION
C       DISPERSEE .GROUPENO :
      NBGNO2= 2*NBGNOT+20
      CALL JECREC(MAG//'.GROUPENO','G V I','NOM','DISPERSE',
     &            'VARIABLE',NBGNO2)
C
C     -- SI LONT = 0 ON S'ARRETE LA.
C     IF ((LONT.EQ.0).OR.(NBGNOT.EQ.0)) GO TO 9999
      CALL WKVECT('&&SSDMGN.WORK1','V V I',LONT,IAWK1)
C
C
C     --3 REMPLISSAGE:
C     ----------------
      DO 5, IOCC=1,NOCC
        UNAUN=.TRUE.
        CALL GETVIS('DEFI_GROUP_NO','INDEX',IOCC,IARG,4,INDI,N1)
        IF (N1.EQ.4) UNAUN=.FALSE.
C
C
C       --3.1 CAS : INDEX, TOUT OU MAILLE :
C       -----------------------------------
        IF (.NOT.UNAUN) THEN
          CALL GETVTX('DEFI_GROUP_NO','TOUT',IOCC,IARG,1,KBID,N1)
          CALL GETVTX('DEFI_GROUP_NO','SUPER_MAILLE',IOCC,IARG,1,
     &                NOSMA,N2)
          IF (N2.EQ.1) CALL JENONU(JEXNOM(MAG//'.SUPMAIL',NOSMA),NUSMA)
          LPREF=0
          CALL GETLTX('DEFI_GROUP_NO','PREFIXE',IOCC,8,1,LPREF,NBID)
          CALL GETVIS('DEFI_GROUP_NO','INDEX',IOCC,IARG,4,INDI,N3)
          LMAIL=INDI(2)-INDI(1)+1
          LGNL=INDI(4)-INDI(3)+1
          LMAIL=MAX(LMAIL,0)
          LGNL=MAX(LGNL,0)
          LONGT= LPREF+LMAIL+LGNL
          IF (LONGT.GT.8) CALL U2MESS('F','SOUSTRUC_61')
          IF (LPREF.GT.0)
     &    CALL GETVTX('DEFI_GROUP_NO','PREFIXE',IOCC,IARG,1,PREF,NBID)
C
          DO 51, ISMA=1,NBSMA
            IF((N2.EQ.1).AND.(NUSMA.NE.ISMA)) GO TO 51
            NOMACR= ZK8(IANMCR-1+ISMA)
            CALL JENUNO(JEXNUM(MAG//'.SUPMAIL',ISMA),NOMAIL)
            I1NOE=ZI(IADIM2-1+4*(ISMA-1)+3)
            CALL JEVEUO(NOMACR//'.LINO','L',IALINO)
            CALL JELIRA(NOMACR//'.LINO','LONUTI',NBNOEX,KBID)
            CALL DISMOI('F','NOM_MAILLA',NOMACR,'MACR_ELEM_STAT'
     &                    ,IBID,MAL,IED)
            CALL JEEXIN(MAL//'.GROUPENO',IRET)
            IF (IRET.EQ.0) THEN
              NBGNO=0
            ELSE
              CALL JELIRA(MAL//'.GROUPENO','NUTIOC',NBGNO,KBID)
            END IF
            DO 511, IGNO=1,NBGNO
              CALL JELIRA(JEXNUM(MAL//'.GROUPENO',IGNO),
     &                 'LONMAX',N3,KBID)
              CALL JEVEUO(JEXNUM(MAL//'.GROUPENO',IGNO),'L',IAGNL)
              CALL UTLISI('INTER',ZI(IALINO),NBNOEX,ZI(IAGNL),N3,
     &           ZI(IAWK1),LONT,NBNO)
C
C
              IF (NBNO.GT.0) THEN
C
C               --3.1.1 CALCUL DE NOMGNG:
C               -------------------------
                CALL JENUNO(JEXNUM(MAL//'.GROUPENO',IGNO),NOMGNL)
                I1=1
                IF (LPREF.GT.0) NOMGNG(I1:I1-1+LPREF) = PREF(1:LPREF)
                I1= I1+LPREF
                IF (LMAIL.GT.0) NOMGNG(I1:I1-1+LMAIL)
     &                         = NOMAIL(INDI(1):INDI(2))
                I1= I1+LMAIL
                IF (LGNL.GT.0) NOMGNG(I1:I1-1+LGNL)
     &                         = NOMGNL(INDI(3):INDI(4))
C
C               --3.1.2 RECOPIE DES NUMEROS DE NOEUDS:
C               --------------------------------------
                CALL JECROC(JEXNOM(MAG//'.GROUPENO',NOMGNG))
                CALL JEECRA(JEXNOM(MAG//'.GROUPENO',NOMGNG),
     &                 'LONMAX',NBNO,KBID)
                CALL JEECRA(JEXNOM(MAG//'.GROUPENO',NOMGNG),
     &                 'LONUTI',NBNO,KBID)
                CALL JEVEUO(JEXNOM(MAG//'.GROUPENO',NOMGNG),
     &                 'E',IAGNO)
                DO 5112,II=1,NBNO
                  INOL=ZI(IAWK1-1+II)
                  KK= INDIIS(ZI(IALINO),INOL,1,NBNOEX)
                  IF (KK.EQ.0) CALL ASSERT(.FALSE.)
                  ZI(IAGNO-1+II)=I1NOE+KK
C
 5112           CONTINUE
              END IF
 511        CONTINUE
 51       CONTINUE
C
C
C       --3.2 CAS : MAILLE, GROUP_NO_FIN, GROUP_NO_INIT:
C       -----------------------------------------------
        ELSE
          CALL GETVTX('DEFI_GROUP_NO','SUPER_MAILLE',IOCC,IARG,1,
     &                NOSMA,N1)
          CALL GETVTX('DEFI_GROUP_NO','GROUP_NO_INIT',IOCC,IARG,1,
     &                NOMGNL,N)
          CALL GETVTX('DEFI_GROUP_NO','GROUP_NO_FIN',IOCC,IARG,1,
     &                NOMGNG,N)
C
          CALL JENONU(JEXNOM(MAG//'.SUPMAIL',NOSMA),ISMA)
          I1NOE=ZI(IADIM2-1+4*(ISMA-1)+3)
          NOMACR= ZK8(IANMCR-1+ISMA)
          CALL JEVEUO(NOMACR//'.LINO','L',IALINO)
          CALL JELIRA(NOMACR//'.LINO','LONUTI',NBNOEX,KBID)
          CALL DISMOI('F','NOM_MAILLA',NOMACR,'MACR_ELEM_STAT'
     &                    ,IBID,MAL,IED)
          CALL JELIRA(JEXNOM(MAL//'.GROUPENO',NOMGNL),'LONUTI',N3,KBID)
          CALL JEVEUO(JEXNOM(MAL//'.GROUPENO',NOMGNL),'L',IAGNL)
          CALL UTLISI('INTER',ZI(IALINO),NBNOEX,ZI(IAGNL),N3,
     &             ZI(IAWK1),LONT,NBNO)
C
          IF (NBNO.GT.0) THEN
            CALL JECROC(JEXNOM(MAG//'.GROUPENO',NOMGNG))
            CALL JEECRA(JEXNOM(MAG//'.GROUPENO',NOMGNG),
     &             'LONMAX',NBNO,KBID)
            CALL JEECRA(JEXNOM(MAG//'.GROUPENO',NOMGNG),
     &             'LONUTI',NBNO,KBID)
            CALL JEVEUO(JEXNOM(MAG//'.GROUPENO',NOMGNG),'E',IAGNO)
            DO 52,II=1,NBNO
              INOL=ZI(IAWK1-1+II)
              KK= INDIIS(ZI(IALINO),INOL,1,NBNOEX)
              IF (KK.EQ.0) CALL ASSERT(.FALSE.)
              ZI(IAGNO-1+II)=I1NOE+KK
 52         CONTINUE
          ELSE
            CALL U2MESK('A','SOUSTRUC_62',1,NOMGNG)
          END IF
        END IF
 5    CONTINUE
C
C
      CALL JEDETR('&&SSDMGN.WORK1')
C
 9999 CONTINUE
      CALL JEDEMA()
      END
