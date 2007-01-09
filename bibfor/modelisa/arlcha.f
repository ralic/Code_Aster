      SUBROUTINE ARLCHA(DIME  ,NOMARL,CINE  ,NOM1  ,NOM2  ,
     &                  CHARGE,EQ)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/01/2007   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      INTEGER       DIME
      CHARACTER*10  NOM1,NOM2
      CHARACTER*8   CINE(3),CHARGE,NOMARL
      LOGICAL       EQ(5,*)
C      
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C TRANSFORMATION DES MATRICES ARLEQUIN MORSE EN RELATIONS LINEAIRES
C
C ----------------------------------------------------------------------
C      
C
C IN  DIME   : DIMENSION DE L'ESPACE
C IN  NOMARL : SD ARLEQUIN
C IN  NOM1   : NOM DE LA SD DE STOCKAGE MAILLES GROUP_MA_1 
C IN  NOM2   : NOM DE LA SD DE STOCKAGE MAILLES GROUP_MA_2 
C IN  CINE   : CINEMATIQUES DES GROUPES DE MAILLE
C IN  CHARGE : NOM UTILISATEUR DE LA CHARGE
C IN  EQ     : EQUATIONS DE COUPLAGE SELECTIONNEES (CF ARLCLR)
C
C SD EN SORTIE:
C
C CHARGE.CHME.LIGRE : LIGREL DE CHARGE
C CHARGE.CHME.CIMPO : CARTE COEFFICIENTS IMPOSES
C CHARGE.CHME.CMULT : CARTE COEFFICIENTS MULTIPLICATEURS
C
C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
C
      CHARACTER*32       JEXNUM , JEXNOM , JEXATR
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C      
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
C
      REAL*8        PRECBB,PREC2,ARLGER
      CHARACTER*19  LIGRE,CIMPO,CMULT
      CHARACTER*16  NOMMO1,NOMMO2
      CHARACTER*8   K8BID
      INTEGER       NNC,DDC,NL,NT,MX
      INTEGER       B0,B1,B2,C0,C1,C2,D0,D1,D2,D3,D4,D5,IRET
      INTEGER       P0,P1,I,J,I1,I2,NEL0,NEL,NDD0,NDD,NLG0,NLG
      LOGICAL       IA1,IA2,IAC
      REAL*8        R
      INTEGER       IFM,NIV 
      INTEGER       NTECMP
      PARAMETER     (NTECMP=6)    
      CHARACTER*16  TECMP(NTECMP)
      INTEGER       NU(NTECMP)
C            
      DATA TECMP / 'D_DEPL_R_DX ','D_DEPL_R_DY ','D_DEPL_R_DZ ',
     &             'D_DEPL_R_DRX','D_DEPL_R_DRY','D_DEPL_R_DRZ' /
C
C ----------------------------------------------------------------------
C 
      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)  
C
C --- INITIALISATIONS ET PARAMETRES
C
      PRECBB = ARLGER(NOMARL,'PRECBB')
      PREC2  = PRECBB**(1.D0/3.D0)
      LIGRE  = CHARGE(1:8)//'.CHME.LIGRE'
      CIMPO  = CHARGE(1:8)//'.CHME.CIMPO'
      CMULT  = CHARGE(1:8)//'.CHME.CMULT'
C
C --- NUMERO D'ASSEMBLAGE
C
      IA1 = (CINE(1).EQ.'COQUE   ')
      IA2 = (CINE(2).EQ.'COQUE   ')
      IAC = (CINE(3).EQ.'COQUE   ')
      I1 = 0
      IF (IA1) I1 = IOR(I1,1)
      IF (IAC) I1 = IOR(I1,2)
      I2 = 0
      IF (IA2) I2 = IOR(I2,1)
      IF (IAC) I2 = IOR(I2,2)
      DDC = DIME
      IF (IAC) DDC = DDC + DIME - 1
      IF ((I1.LT.0).OR.(I1.GT.3)) CALL ASSERT(.FALSE.)
      IF ((I2.LT.0).OR.(I2.GT.3)) CALL ASSERT(.FALSE.)            
C
C --- LECTURE DONNEES MATRICES MORSES
C
      NOMMO1 = NOM1(1:10)//'.MORSE'
      CALL JELIRA(NOMMO1(1:16)//'.INO','NMAXOC',NNC,ZK8)
      NL = DDC*NNC
      CALL JEVEUO(NOMMO1(1:16)//'.VALE','L',B0)
      CALL JEVEUO(NOMMO1(1:16)//'.INO','L',B1)
      CALL JEVEUO(JEXATR(NOMMO1(1:16)//'.INO','LONCUM'),'L',B2)
C      
      NOMMO2 = NOM2(1:10)//'.MORSE'
      CALL JEVEUO(NOMMO2(1:16)//'.VALE','L',C0)
      CALL JEVEUO(NOMMO2(1:16)//'.INO','L',C1)
      CALL JEVEUO(JEXATR(NOMMO2(1:16)//'.INO','LONCUM'),'L',C2)
C
C --- NUMERO DES DDLS
C
      DO 10 I = 1, NTECMP
        CALL JENONU(JEXNOM('&CATA.TE.NOMTE',TECMP(I)),NU(I))
 10   CONTINUE
C
C --- SELECTION DES EQUATIONS DE COUPLAGE
C
      CALL WKVECT('&&ARLCHA.MAX','V V R',2*NNC,MX)
C
C --- CALCUL MAXIMA TERMES POUR ADIMENSIONNEMENT
C
      CALL ARLMAX(DIME,IA1,IAC,NNC,ZR(B0),
     &            ZI(B2),ZR(MX))
      CALL ARLMAX(DIME,IA2,IAC,NNC,ZR(C0),
     &            ZI(C2),ZR(MX))
C
      P0 = MX
      R = 0.D0
      DO 30 I = 1, NNC
        IF (ZR(P0).GT.R) R = ZR(P0)
        P0 = P0 + 2
 30   CONTINUE
C
      P0 = MX-2
      R  = R*PREC2
      DO 40 I = 1, NNC
        P0 = P0 + 2
        IF (ZR(P0).GE.R) GOTO 40
        DO 50 J = 1, 5
          EQ(J,I) = .FALSE.
 50     CONTINUE
 40   CONTINUE
C
C --- COMPTE NOMBRE D'EQUATIONS DE COUPLAGE
C
      NL = 0
      DO 60 I = 1, NNC
        DO 60 J = 1, DDC
          IF (EQ(J,I)) NL = NL + 1
 60   CONTINUE
C
C --- ADIMENSIONNEMENT ET COMPTE DES TERMES DES EQUATIONS
C
      NT = 0
      CALL ARLCH0(DIME  ,IA1   ,IAC   ,NNC   ,ZI(B2),
     &            ZR(MX),PRECBB,EQ    ,ZR(B0),NT)
      CALL ARLCH0(DIME  ,IA2   ,IAC   ,NNC   ,ZI(C2),
     &            ZR(MX),PRECBB,EQ    ,ZR(C0),NT)
      CALL JEDETR('&&ARLCHA.MAX')
C
C --- ALLOCATION / AGRANDISSEMENT .CHME
C
      CALL JEEXIN(LIGRE//'.LIEL',IRET)
      IF (IRET.NE.0) THEN
        CALL JELIRA(LIGRE//'.LIEL','LONT',NEL,K8BID)
        CALL JEVEUO(LIGRE//'.LIEL','L',D0)
        NEL0 = -ZI(D0-2+NEL)
      ELSE
        NEL0 = 0
      ENDIF
C      
C --- CREATION OU EXTENSION DES CARTES .CMULT ET .CIMPO
C
      CALL CRAGCH(NT,'REEL','REEL',LIGRE)
C      
C --- CREATION OU EXTENSION DU LIGREL DE CHARGE LIGRCH      
C
      CALL CRAGLC(NT,LIGRE)
C
C --- LECTURE OBJETS CHARGE
C
      CALL JEVEUO(CIMPO(1:19)//'.DESC','E',D0)
      CALL JEVEUO(CIMPO(1:19)//'.NOLI','E',D1)
      CALL JEVEUO(CIMPO(1:19)//'.LIMA','E',D2)
C
      CALL JEVEUO(CMULT(1:19)//'.DESC','E',D3)
      CALL JEVEUO(CMULT(1:19)//'.NOLI','E',D4)
      CALL JEVEUO(CMULT(1:19)//'.LIMA','E',D5)

      NDD0 = ZI(D0+2)
      ZI(D0+2) = ZI(D0+1)
      ZI(D3+2) = ZI(D3+1)

      NDD = NDD0
      P0 = 3 + 2*NDD0
      P1 = P0 + NDD0 + 2*NT

      NEL = NEL0

      DO 70 I = 1, NT

        ZK24(D1+NDD) = LIGRE
        ZK24(D4+NDD) = LIGRE
        NDD = NDD + 1
        NEL = NEL + 1
        ZI(D0+P0  )  = -3
        ZI(D3+P0  )  = -3
        ZI(D0+P0+1)  = NDD
        ZI(D3+P0+1)  = NDD
        ZI(D0+P1  )  = 2
        ZI(D3+P1  )  = 2
        ZI(D2-1+NDD) = -NEL
        ZI(D5-1+NDD) = -NEL
        CALL JECROC(JEXNUM(CIMPO(1:19)//'.LIMA',NDD))
        CALL JEECRA(JEXNUM(CIMPO(1:19)//'.LIMA',NDD),'LONMAX',1,' ')
        CALL JECROC(JEXNUM(CMULT(1:19)//'.LIMA',NDD))
        CALL JEECRA(JEXNUM(CMULT(1:19)//'.LIMA',NDD),'LONMAX',1,' ')
        CALL JECROC(JEXNUM(LIGRE(1:19)//'.LIEL',NEL))
        CALL JEECRA(JEXNUM(LIGRE(1:19)//'.LIEL',NEL),'LONMAX',2,' ')
        CALL JECROC(JEXNUM(LIGRE(1:19)//'.NEMA',NEL))
        CALL JEECRA(JEXNUM(LIGRE(1:19)//'.NEMA',NEL),'LONMAX',4,' ')
        P0 = P0 + 2
        P1 = P1 + 1

 70   CONTINUE

      CALL JEVEUO(LIGRE(1:19)//'.NBNO','E',D0)
      NLG0 = ZI(D0)
      ZI(D0) = NLG0 + 2*NL

      CALL POSLAG('12',P0,P1)
      CALL JEVEUO(LIGRE//'.LGNS','E',D0)
      D0 = D0 + NLG0

      DO 80 I = 1, NL
        ZI(D0  ) = P0
        ZI(D0+1) = P1
        D0 = D0 + 2
 80   CONTINUE
C
C --- ECRITURE .CHME
C
      CALL JEVEUO(CIMPO(1:19)//'.VALE','E',D0)
      CALL JEVEUO(CMULT(1:19)//'.VALE','E',D1)
      CALL JEVEUO(LIGRE(1:19)//'.LIEL','E',D2)
      CALL JEVEUO(JEXNUM(LIGRE(1:19)//'.NEMA',NEL0+1),'E',D3)
      D3 = D3 - 4*NEL0
      NDD = NDD0
      NEL = NEL0
C
C --- MATRICE MORSE 1
C
      NLG = NLG0
      
      GOTO (90,100,110) I1

      CALL ARLCH1(DIME  ,NNC   ,0     ,NU    ,ZR(B0),
     &            ZI(B1),ZI(B2),PRECBB,EQ    ,NDD   ,
     &            NEL   ,NLG   ,ZR(D0),ZR(D1),ZI(D2),
     &            ZI(D3))
      GOTO 120

 90   CONTINUE
      CALL ARLCH2(DIME  ,NNC   ,0     ,NU    ,ZR(B0),
     &            ZI(B1),ZI(B2),PRECBB,EQ    ,NDD   ,
     &            NEL   ,NLG   ,ZR(D0),ZR(D1),ZI(D2),
     &            ZI(D3))
      GOTO 120

 100  CONTINUE
      CALL ARLCH3(DIME  ,NNC   ,0     ,NU    ,ZR(B0),
     &            ZI(B1),ZI(B2),PRECBB,EQ    ,NDD   ,
     &            NEL   ,NLG   ,ZR(D0),ZR(D1),ZI(D2),
     &            ZI(D3))
      GOTO 120

 110  CONTINUE
      CALL ARLCH4(DIME  ,NNC   ,0     ,NU    ,ZR(B0),
     &            ZI(B1),ZI(B2),PRECBB,EQ    ,NDD   ,
     &            NEL   ,NLG   ,ZR(D0),ZR(D1),ZI(D2),
     &            ZI(D3))

 120  CONTINUE
C
C --- MATRICE MORSE 2
C
      NLG = NLG0
      
      GOTO (130,140,150) I2

      CALL ARLCH1(DIME  ,NNC   ,1     ,NU    ,ZR(C0),
     &            ZI(C1),ZI(C2),PRECBB,EQ    ,NDD   ,
     &            NEL   ,NLG   ,ZR(D0),ZR(D1),ZI(D2),
     &            ZI(D3))
      GOTO 160

 130  CONTINUE
      CALL ARLCH2(DIME  ,NNC   ,1     ,NU    ,ZR(C0),
     &            ZI(C1),ZI(C2),PRECBB,EQ    ,NDD   ,
     &            NEL   ,NLG   ,ZR(D0),ZR(D1),ZI(D2),
     &            ZI(D3))
      GOTO 160

 140  CONTINUE
      CALL ARLCH3(DIME  ,NNC   ,1     ,NU    ,ZR(C0),
     &            ZI(C1),ZI(C2),PRECBB,EQ    ,NDD   ,
     &            NEL   ,NLG   ,ZR(D0),ZR(D1),ZI(D2),
     &            ZI(D3))
      GOTO 160

 150  CONTINUE
      CALL ARLCH4(DIME  ,NNC   ,1     ,NU    ,ZR(C0),
     &            ZI(C1),ZI(C2),PRECBB,EQ    ,NDD   ,
     &            NEL   ,NLG   ,ZR(D0),ZR(D1),ZI(D2),
     &            ZI(D3))

 160  CONTINUE
C
      CALL JEDEMA()

      END
