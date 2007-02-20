      SUBROUTINE GIECAS(NFIC,NDIM,NBOBJ)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 20/02/2007   AUTEUR LEBOUVIER F.LEBOUVIER 
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
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     ARGUMENTS:
C     ----------
      INTEGER NFIC,NDIM,NBOBJ
C ----------------------------------------------------------------------
C     BUT: ECRIRE LE FICHIER DE MAILLAGE ASTER A PARTIR DES OBJETS
C          CREES PAR GILIRE ( '&&GILIRE.....')
C
C     IN : NFIC : UNITE D'ECRITURE
C          NDIM : DIMENSION DU PROBLEME (2D OU 3D)
C          NBOBJ: NOMBRE D'OBJETS (AU SENS GIBI)
C
C ----------------------------------------------------------------------
C
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR,JEXR8
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      INTEGER VALI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C ---------------- FIN COMMUNS NORMALISES  JEVEUX  --------------------
C     VARIABLES LOCALES:
      REAL*8 R8BID
      CHARACTER*50 KBID
      CHARACTER*16 K16OBJ
      CHARACTER*7  K7BID ,K7NOM(7)
      CHARACTER*8  TYMAIL,NOMOBJ,NOMNO,K8NOM(7),NOMOBG
      LOGICAL MAGOUI,TROUVE,INDIR
C
      CHARACTER*1 CBID
      DATA CBID/' '/
C
C
C     -- ON INITIALISE LA COLLECTION QUI CONTIENT LES CORRESPONDANCES
C     -- ENTRE LES NUMEROTATIONS LOCALES DES NOEUDS GIBI ET ASTER:
      CALL JEMARQ()
      CALL GIINCO()
C
      CALL JEVEUO('&&GILIRE.COORDO   ', 'L',         IACOOR)
      CALL JELIRA('&&GILIRE.COORDO   ', 'LONMAX', NCOO,KBID)
C
      CALL JEVEUO('&&GILIRE.NOMOBJ',    'L',         IANOOB)
      CALL JEVEUO('&&GILIRE.DESCOBJ',   'L',         IADSOB)
      CALL JEVEUO('&&GILIRE.CUMUL_ELE',   'L',         IACUEL)
C
      CALL JEVEUO('&&GILIRE.OBJET_NOM', 'L',         IAOBNO)
      CALL JEVEUO('&&GILIRE.OBJET_NUM', 'L',         IAOBNU)
C
      CALL JEVEUO('&&GILIRE.NUMANEW', 'L',         IANEMA)
C
C
C     -----------------------------------------------------------------
C     --ECRITURE DU TITRE:
C     -----------------------------------------------------------------
      WRITE(NFIC,*) 'TITRE'
      WRITE(NFIC,*) '%  GIBI FECIT'
      WRITE(NFIC,*) 'FINSF'
      WRITE(NFIC,*) '%'
C
C     -----------------------------------------------------------------
C     --ECRITURE DES NOEUDS:
C     -----------------------------------------------------------------
      IF (NDIM.EQ.3) THEN
         WRITE(NFIC,*) 'COOR_3D'
      ELSE IF (NDIM.EQ.2) THEN
         WRITE(NFIC,*) 'COOR_2D'
      ELSE IF (NDIM.EQ.1) THEN
         WRITE(NFIC,*) 'COOR_1D'
      ELSE
         CALL U2MESS('F','PREPOST_53')
      END IF
      INDIR =.FALSE.
      CALL JEEXIN ( '&&GILIRE.INDIRECT', IRET)
      IF (IRET.NE.0) THEN
        INDIR =.TRUE.
        CALL JELIRA('&&GILIRE.INDIRECT','LONMAX',NBNOTO,CBID)
        CALL JEVEUO ( '&&GILIRE.INDIRECT', 'L', IAPTIN )
        CALL WKVECT('&&GILIRE.NOENOM','V V I',NBNOTO,INUTRI)
        CALL JACOPO(NBNOTO,'I',IAPTIN,INUTRI)
        CALL UTTRII(ZI(INUTRI),NBNOTO)
        NBELIM = (NCOO/NDIM)-NBNOTO
        IF (NBELIM.GT.0) THEN
         VALI = NBELIM
         CALL U2MESG('I', 'PREPOST5_19',0,' ',1,VALI,0,0.D0)
        ENDIF
      ELSE
        NBNOTO=NCOO/NDIM
      ENDIF
C
      DO 1, INO=1,NBNOTO
         IF (INDIR) THEN
           NONO = ZI(INUTRI-1+INO)
         ELSE
           NONO = INO
         ENDIF
         CALL CODENT(NONO,'G',K7BID)
         WRITE(NFIC,1001) 'N'//K7BID,
     &         (ZR(IACOOR-1+NDIM*(NONO-1)+J),J=1,NDIM)
 1    CONTINUE
C
      WRITE(NFIC,*) 'FINSF'
      WRITE(NFIC,*) '%'
C
C     -----------------------------------------------------------------
C     --ECRITURE DES MAILLES:
C     -----------------------------------------------------------------
C
      CALL JELIRA('&&GILIRE.OBJET_NOM','LONMAX',NBOBNO,CBID)
      CALL WKVECT('&&GILIRE.OBJTRI_NUM','V V I',NBOBJ,ITRNU)
      CALL WKVECT('&&GILIRE.ECRIGRM','V V L',NBOBJ,IECRIT)

      CALL JEVEUO('&&GILIRE.NUMANEW','L',IANEMA)
      CALL JELIRA('&&GILIRE.NUMANEW','LONUTI',ITOT,CBID)

C   CALCUL DU NB TOT D'ELEMENTS
      CALL WKVECT('&&GILIRE.ECRMAIL','V V L',ITOT,IECRMA)


      DO 18 IL=1,NBOBJ
        ZL(IECRIT+IL-1)=.FALSE.
 18   CONTINUE
      IMB = 1
      DO 14 IMA =1,NBOBNO
        II = ZI(IAOBNU+IMA-1)
        IF(.NOT.(ZL(IECRIT+II-1))) THEN
           ZI(ITRNU+IMB-1) =  II
           IMB =IMB + 1
           ZL(IECRIT+II-1)=.TRUE.
         ENDIF
 14   CONTINUE
C
      DO 15 I=1,NBOBNO
         II = ZI(IAOBNU+I-1)
         NBSOOB = ZI(IADSOB-1+4*(II-1)+1)
         NOMOBJ=ZK8(IANOOB-1+2*(II-1)+1)
         IF (NBSOOB.NE.0) THEN
           CALL JEVEUO('&&GILIRE'//NOMOBJ//'.SOUSOB', 'L',IASOOB)
           DO 16 KK =1,NBSOOB
              JJ = ZI(IASOOB+KK-1)
              IF(.NOT.(ZL(IECRIT+JJ-1))) THEN
                ZI(ITRNU+IMB-1)= JJ
                ZL(IECRIT+JJ-1)=.TRUE.
               IMB =IMB + 1
              ENDIF
 16        CONTINUE
         ENDIF
 15    CONTINUE

C ON SUPPRIME UN IMB CAR ON EN COMPTE UN DE PLUS DANS LA FIN DE BOUCLE

      IMB =IMB-1
C
C ON TRIE LA TABLE
C
      IF (IMB.GT.1) THEN
        CALL UTTRII(ZI(ITRNU),IMB)
      ENDIF
C
      ICOMA = 0
      NBELT = 0
      NBELC = 0
C
      DO 2, I=1,NBOBJ
        TROUVE =.FALSE.
        DO 12 JJ =1,IMB
          II = ZI(ITRNU+JJ-1)
          IF (I.EQ.II) THEN
            TROUVE = .TRUE.
            GOTO 13
           ENDIF
 12      CONTINUE
 13      CONTINUE

         NBNO  =ZI(IADSOB-1+4*(I-1)+3)
         NBELE =ZI(IADSOB-1+4*(I-1)+4)
         NOMOBJ =ZK8(IANOOB-1+2*(I-1)+1)
         TYMAIL =ZK8(IANOOB-1+2*(I-1)+2)
         NBELT = NBELT+NBELE
         IF (TROUVE) NBELC = NBELC +NBELE
C
C        -- SI L'OBJET EST 1 OBJET SIMPLE , ON ECRIT SES MAILLES:
         IF (NBELE.GT.0) THEN
            CALL GIECMA(NFIC,TROUVE,NBELE,NOMOBJ,TYMAIL,NBNO,
     &        ZL(IECRMA),ICOMA)
         END IF
 2    CONTINUE
      NMELIM = NBELT - NBELC
      IF (NMELIM.GT.0) THEN
         VALI = NMELIM
         CALL U2MESG('I', 'PREPOST5_20',0,' ',1,VALI,0,0.D0)
      ENDIF
C
C     -----------------------------------------------------------------
C     --ECRITURE DES GROUP_NO:
C     -----------------------------------------------------------------
C
      CALL JEEXIN('&&GILIRE.POINT_NOM',IRET)
      IF (IRET.GT.0) THEN
        CALL JEVEUO('&&GILIRE.POINT_NOM', 'L',         IAPTNO)
        CALL JEVEUO('&&GILIRE.POINT_NUM', 'L',         IAPTNU)
        CALL JELIRA('&&GILIRE.POINT_NOM','LONMAX',NBNONO,KBID)
      ELSE
        NBNONO=0
      END IF
C
      DO 3, I=1,NBNONO
         NOMNO =ZK8(IAPTNO-1+I)
         IF(NOMNO(1:1).EQ.'#') GOTO 3
         NUMNO =ZI (IAPTNU-1+I)
         CALL CODENT (NUMNO,'G',K7BID)
         WRITE(NFIC,*) 'GROUP_NO'
         WRITE(NFIC,1002) NOMNO,'N'//K7BID
         WRITE(NFIC,*) 'FINSF'
         WRITE(NFIC,*) '%'
 3    CONTINUE
C
C     -----------------------------------------------------------------
C     --ECRITURE DES GROUP_MA:
C     -----------------------------------------------------------------
C
      CALL JELIRA('&&GILIRE.OBJET_NOM','LONMAX',NBOBNO,CBID)
      DO 4, II=1,NBOBJ
         TROUVE =.FALSE.
         DO 21 INU =1,NBOBNO
           IF (ZI(IAOBNU-1+INU).EQ.II) THEN
             TROUVE = .TRUE.
             NOMOBG=ZK8(IAOBNO-1+INU)
             IF(NOMOBG(1:1).EQ.'#') GOTO 21
             WRITE(NFIC,*) 'GROUP_MA'
             WRITE(NFIC,*) '  ',NOMOBG
             NBSOOB =ZI(IADSOB-1+4*(II-1)+1)
             IF (NBSOOB.EQ.0) THEN
C
C           -- ON FAIT COMME SI L'OBJET SE CONTENAIT LUI-MEME:
              NBSOOB=1
              MAGOUI=.TRUE.
           ELSE
              MAGOUI=.FALSE.
              NOMOBJ=ZK8(IANOOB-1+2*(II-1)+1)
              CALL JEVEUO('&&GILIRE'//NOMOBJ//'.SOUSOB','L',IASSOB)
           END IF
           DO 5,J=1,NBSOOB
C
C        -- L'OBJET EST 1 OBJET COMPOSE, ON ECRIT SES MAILLES:
            IF (MAGOUI) THEN
               JJ= II
            ELSE
               JJ= ZI(IASSOB-1+J)
            END IF
            NOMOBJ=ZK8(IANOOB-1+2*(JJ-1)+1)
            NBNO  =ZI(IADSOB-1+4*(JJ-1)+3)
            NBELE =ZI(IADSOB-1+4*(JJ-1)+4)
            NBFOIS = NBELE/7
            NBREST= NBELE-7*NBFOIS
            ICOK= ZI(IACUEL-1+JJ)
C
            DO 6, K=1,NBFOIS
               DO 7, KK=1,7
                  ICOK=ICOK+1
                  CALL CODENT(ZI(IANEMA-1+ICOK),'G',K7NOM(KK))
                  K8NOM(KK)='M'//K7NOM(KK)
 7             CONTINUE
                 WRITE(NFIC,1003) (K8NOM(L),L=1,7)
 6          CONTINUE
C
            DO 8, KK=1,NBREST
               ICOK=ICOK+1
               CALL CODENT(ZI(IANEMA-1+ICOK),'G',K7NOM(KK))
               K8NOM(KK)='M'//K7NOM(KK)
 8          CONTINUE
            WRITE(NFIC,1003) (K8NOM(L),L=1,NBREST)
C
 5           CONTINUE
            WRITE(NFIC,*) 'FINSF'
            WRITE(NFIC,*) '%'
           ENDIF
21        CONTINUE
C
 4    CONTINUE
C
C     -- ON ECRIT LE "FIN" FINAL ET ON REMBOBINE LE FICHIER:
C     ------------------------------------------------------
      WRITE(NFIC,*) 'FIN'
      REWIND(NFIC)
C
       CALL JEDETC('V','&&GILIRE',1)
 1001  FORMAT(1X,A8,1X,1PD21.14,1X,1PD21.14,1X,1PD21.14)
 1002  FORMAT(1X,A8,1X,A8)
 1003  FORMAT(7(1X,A8))
C
      CALL JEDEMA()
      END
