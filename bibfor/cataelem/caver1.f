      SUBROUTINE CAVER1()
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CATAELEM  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
      IMPLICIT REAL*8 (A-H,O-Z)

C ----------------------------------------------------------------------
C     BUT: VERIFIER LA COHERENCE DES OBJETS DES CATALOGUES.

C ----------------------------------------------------------------------

C     FONCTIONS EXTERNES:
C     -------------------

C     VARIABLES LOCALES:
C     ------------------
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*32 JEXNUM,JEXNOM
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI,OPT,TE
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL,ERROR
      CHARACTER*8 ZK8,PARA,TYPMAI
      CHARACTER*16 ZK16,NOMOPT,NOMTE
      CHARACTER*24 ZK24
      CHARACTER*24 VALK(4)
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      CHARACTER*8 KBID,GD1,GD2,TGD1(10),TGD2(10),TYPOUT,TYPOU2
C ---------------- FIN COMMUNS NORMALISES  JEVEUX  --------------------



      CALL JEMARQ()
      CALL JELIRA('&CATA.OP.NOMOPT','NOMMAX',NBOPT,KBID)
      CALL JELIRA('&CATA.TE.NOMTE','NOMMAX',NBTE,KBID)
      CALL JEVEUO('&CATA.TE.OPTTE','L',IAOPTE)
      CALL JEVEUO('&CATA.TE.NBLIGCOL','L',IANBLC)
      LGCO = ZI(IANBLC-1+1)

      IER = 0

      CALL JEVEUO('&CATA.TE.TYPEMA','L',JTYPMA)


      DO 40,OPT = 1,NBOPT
        CALL JENUNO(JEXNUM('&CATA.OP.NOMOPT',OPT),NOMOPT)
        CALL JEVEUO(JEXNUM('&CATA.OP.DESCOPT',OPT),'L',IADESC)
        CALL JEVEUO(JEXNUM('&CATA.OP.OPTPARA',OPT),'L',IAPARA)
        NBIN = ZI(IADESC-1+2)
        NBOUT = ZI(IADESC-1+3)
        NBVOL = ZI(IADESC-1+4)
        IF (NBVOL.NE.0) THEN
          CALL U2MESK('E','CATAELEM_1',1,NOMOPT)
          IER = IER + 1
        END IF


C       -- ON VERIFIE QU'UNE OPTION N'A JAMAIS 2 PARAMETRES
C          DE MEME NOM
C          -------------------------------------------------
        CALL KNDOUB(8,ZK8(IAPARA),NBIN+NBOUT,IRET)
        IF (IRET.GT.0)
     &    CALL U2MESK('E','CATAELEM_2',1,NOMOPT)


        DO 30,TE = 1,NBTE
          CALL JENUNO(JEXNUM('&CATA.TE.NOMTE',TE),NOMTE)
          IOPTTE = ZI(IAOPTE-1+ (TE-1)*LGCO+OPT)
          IF (IOPTTE.EQ.0) GO TO 30
          CALL JEVEUO(JEXNUM('&CATA.TE.OPTMOD',IOPTTE),'L',IAOPMO)
          CALL JEVEUO(JEXNUM('&CATA.TE.OPTNOM',IOPTTE),'L',IAOPNO)
          NUCALC = ZI(IAOPMO-1+1)
          NBINTE = ZI(IAOPMO-1+2)

          TYPMAI = ZK8(JTYPMA-1+TE)
          CALL JEVEUO(JEXNOM('&CATA.TM.NBNO',TYPMAI),'L',JNBNO)
          NBNO = ZI(JNBNO)

C              ON NE TRAQUE PAS LES ERREURS SI NUCALC=0,-1 OU -2
          IF ((NUCALC.LE.0) .AND. (NUCALC.GE.-2)) GO TO 30
          DO 10,IPARA = 1,NBINTE
            PARA = ZK8(IAOPNO-1+IPARA)
            IMOLO = ZI(IAOPMO-1+3+IPARA)
            IF (IMOLO.EQ.0) THEN
                 VALK(1) = PARA
                 VALK(2) = NOMOPT
                 VALK(3) = NOMTE
                 CALL U2MESK('E','CATAELEM_3', 3 ,VALK)
                IER = IER + 1
                GO TO 10
            END IF

            CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',IMOLO),'L',IAMOLO)
            IGD = ZI(IAMOLO-1+2)
            ITROU = INDIK8(ZK8(IAPARA-1+1),PARA,1,NBIN)
            IGDOP = ZI(IADESC-1+4+ITROU)
            IF ((ITROU.EQ.0) .OR. (IGDOP.NE.IGD)) THEN
              IF (ITROU.EQ.0) THEN
                 VALK(1) = PARA
                 VALK(2) = NOMOPT
                 VALK(3) = NOMTE
                 CALL U2MESK('E','CATAELEM_4', 3 ,VALK)
                IER = IER + 1
              END IF
              IF (IGDOP.NE.IGD) THEN
                 VALK(1) = PARA
                 VALK(2) = NOMOPT
                 VALK(3) = NOMTE
                 CALL U2MESK('E','CATAELEM_5', 3 ,VALK)
                IER = IER + 1
              END IF
            END IF

C              -- ON VERIFIE QUE POUR LES MODE LOCAUX AUX NOEUDS
C                 LE NOMBRE DE NOEUDS EST LE NOMBRE DE NOEUDS DE
C                 LA MAILLE SUPPORT :
C              ---------------------------------------------------
            ICODE = ZI(IAMOLO-1+1)
            NBPT2 = -1
            IF (ICODE.EQ.2) THEN
              NBPT2 = MOD(ZI(IAMOLO-1+4),10000)
            ELSE IF (ICODE.EQ.3) THEN
              NBPT1 = ZI(IAMOLO-1+4)
              IF (NBPT1.LT.0) THEN
                NBPT2 = MOD(ABS(NBPT1),10000)
              END IF
            END IF
            IF (NBPT2.GE.0) THEN
              IF (NBPT2.NE.NBNO) THEN
                 VALK(1) = PARA
                 VALK(2) = NOMOPT
                 VALK(3) = NOMTE
                 CALL U2MESK('E','CATAELEM_6', 3 ,VALK)
                IER = IER + 1
              END IF
            END IF


   10     CONTINUE


C         -- VERIFICATION DES MODES LOCAUX "OUT" DES TE/OPTIONS:
C         ------------------------------------------------------
          NBOUTE = ZI(IAOPMO-1+3)
          DO 20,IPARA = 1,NBOUTE
            PARA = ZK8(IAOPNO-1+NBINTE+IPARA)
            IMOLO = ZI(IAOPMO-1+3+NBINTE+IPARA)
            IF (IMOLO.EQ.0) THEN
                 VALK(1) = PARA
                 VALK(2) = NOMOPT
                 VALK(3) = NOMTE
                 CALL U2MESK('E','CATAELEM_3', 3 ,VALK)
                IER = IER + 1
                GO TO 20
            END IF
            CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',IMOLO),'L',IAMOLO)
            IGD = ZI(IAMOLO-1+2)
            ITROU = INDIK8(ZK8(IAPARA-1+NBIN+1),PARA,1,NBOUT)
            IGDOP = ZI(IADESC-1+4+NBIN+ITROU)
            IF ((ITROU.EQ.0) .OR. (IGDOP.NE.IGD)) THEN
              IF (ITROU.EQ.0) THEN
                 VALK(1) = PARA
                 VALK(2) = NOMOPT
                 VALK(3) = NOMTE
                 CALL U2MESK('E','CATAELEM_4', 3 ,VALK)
                IER = IER + 1
              END IF
              IF (IGDOP.NE.IGD) THEN
                 VALK(1) = PARA
                 VALK(2) = NOMOPT
                 VALK(3) = NOMTE
                 CALL U2MESK('E','CATAELEM_5', 3 ,VALK)
                IER = IER + 1
              END IF
            END IF

C           -- ON VERIFIE QUE  LE TYPE DU CHAMP LOCAL EST COHERENT
C               AVEC CELUI DECLARE DANS L'OPTION :
C           ---------------------------------------------------
            ICODE = ZI(IAMOLO-1+1)
            TYPOU2=ZK8(IAPARA-1+NBIN+NBOUT+ITROU)
            TYPOUT='????'
            IF (ICODE.GE.4) THEN
              TYPOUT='RESL__'
            ELSEIF (ICODE.EQ.3) THEN
              TYPOUT='ELGA__'
            ELSEIF (ICODE.EQ.2) THEN
              TYPOUT='ELNO__'
            ELSEIF (ICODE.EQ.1) THEN
              TYPOUT='ELEM__'
            END IF

            IF (TYPOUT.NE.TYPOU2) THEN
               VALK(1) = PARA
               VALK(2) = NOMOPT
               VALK(3) = NOMTE
               VALK(4) = TYPOU2
               CALL U2MESK('E','CATAELEM_7', 4 ,VALK)
C             IER = IER + 1
            END IF
   20     CONTINUE

   30   CONTINUE

   40 CONTINUE

C    -- ON VERIFIE QUE CERTAINES GRANDEURS SONT "PARALLELES" :
C       ELLES DOIVENT AVOIR EXACTEMENT LES MEMES CMPS :
C    ----------------------------------------------------------
      NBGD = 2
      TGD1(1) = 'DEPL_R'
      TGD2(1) = 'DEPL_C'
      TGD1(2) = 'DEPL_R'
      TGD2(2) = 'DEPL_F'
      ERROR = .FALSE.
      DO 90,K = 1,NBGD
        GD1 = TGD1(K)
        GD2 = TGD2(K)
        CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP',GD1),'LONMAX',N1,KBID)
        CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP',GD2),'LONMAX',N2,KBID)
        IF (N1.NE.N2) THEN
          ERROR = .TRUE.
        ELSE
          CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP',GD1),'L',JNOCM1)
          CALL JEVEUO(JEXNOM('&CATA.GD.NOMCMP',GD2),'L',JNOCM2)
          DO 80,KK = 1,N1
            IF (ZK8(JNOCM1-1+KK).NE.ZK8(JNOCM1-1+KK)) ERROR = .TRUE.
   80     CONTINUE
        END IF
        IF (ERROR) THEN
           VALK(1) = GD1
           VALK(2) = GD2
           CALL U2MESK('E','CATAELEM_8', 2 ,VALK)
          IER = IER + 1
        END IF
   90 CONTINUE


      IF (IER.GT.0) THEN
        CALL U2MESS('F','CATAELEM_9')
      END IF

      CALL JEDEMA()
      END
