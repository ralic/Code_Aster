      SUBROUTINE MDITM1 ( NBM,NBMCD,NBMP,NBNL,INDIC,NBF,IMPR,
     &                    ITRANS,EPST,ICOUPL,TPFL,VECI1,LOCFL0,
     &                    DT0,TFEXM,TS,IARCH,NEXCIT,TABEXC,NUMEXC,
     &                    MASGI,AMORI,PULSI,VECR3,
     &                    PHII,PARCHO,NOECHO,INTITU,
     &                    VECR5,VECR1,VECR2,VGAP,VECR4,NBCHOC,
     &                    DEPG0,VITG0,XSI0,NBSAUV)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C TOLE  CRP_21
C-----------------------------------------------------------------------
C DESCRIPTION : CALCUL DE LA REPONSE DYNAMIQUE NON-LINEAIRE D'UNE
C -----------   STRUCTURE PAR UNE METHODE INTEGRALE
C               CREATION DES OBJETS DE TRAVAIL - PREPARATION DES DONNEES
C
C               APPELANT : MDTR74
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C -------------------------
C
C ARGUMENTS
C ---------
      INCLUDE 'jeveux.h'
      INTEGER       NBM, NBMCD, NBMP, NBNL, INDIC, NBF, IMPR, ITRANS
      REAL*8        EPST
      INTEGER       ICOUPL
      CHARACTER*8   TPFL
      INTEGER       VECI1(*)
      LOGICAL       LOCFL0(*)
      REAL*8        DT0, TFEXM, TS
      INTEGER       IARCH, NEXCIT
      CHARACTER*8   TABEXC(*)
      INTEGER       NUMEXC(*)
      REAL*8        MASGI(*), AMORI(*), PULSI(*), VECR3(*),
     &              PHII(NBNL,NBM,*), PARCHO(NBNL,*)
      CHARACTER*8   NOECHO(NBNL,*), INTITU(*)
      REAL*8        VECR5(*), VECR1(*), VECR2(*), VGAP, VECR4(*),
     &              DEPG0(*), VITG0(*), XSI0(*)
      INTEGER       NBSAUV
C
C VARIABLES LOCALES
C -----------------
      INTEGER       I, IC, IDRAYO, IDTHET, IM, IMODE, J,
     &              KFEXT, KTEXT, KINTI, KNCHO, N2, NBSEG, NBSEG0,
     &              NITMAX, NP3,NBCHOC
      INTEGER       JTRAN
      INTEGER       IAMO00, IPULD, IPUL00,
     &              IFMO0, IFMOA,
     &              IDEPG, IVITG, IACCG, IACCG0
      INTEGER       IFX, IFXS, ITX, ITXS, IFXTR, IFX0
      INTEGER       IOM, IAA, IBB, IZIN, IZITR, IZA1, IZA2, IZA3,
     &              IS0, ISR0, IZ0, IZA4, IZA5
      INTEGER       IAMO, IAMO0, IPUL, IPUL0,
     &              IFMO00, IFMOT, IFMO0T, IFEXMO, IFNLMO, IFMRES,
     &              IDEPGE, IDEPGC, IDEPGT, JDEPGT,
     &              IVITGE, IVITGC, IVITGT, JVITGT, IACCGT
      INTEGER       IKMO, ICMO, IKMO0, ICMO0, IKMO00, ICMO00,
     &              IKMOCA, ICMOCA, ICMOFA, JFLU0, JFLUC
      INTEGER       ITR, IVG, IVD, IVG0, IVD0, IVVG, IRR, IRI, IRR0,
     &              JIX, JIXF, JI1, JI2,
     &              IM1, IM2, IM6, IFTMP, IDD, IU, IW, ILOCFC, ILOC
      INTEGER       JC1, JC2, JCE, JCB, JIFN, JTYPCH, JNS, JIA,
     &              ICHO, IORIG, IALP, IBET, IGAM, IOLDF, IH,
     &              IRC, ITHE
      INTEGER       JDEP, JVIT, JACC, JDEP0, JVIT0, JACC0
      INTEGER       JDT, IBID, IRETT, NBVAL
      REAL*8        DTTR, R8BID
      COMPLEX*16    CBID
      CHARACTER*8   RESU, NOMOBJ, VECGEN, K8TYP, KBID
      CHARACTER*16  TYPRES, NOMCMD
      CHARACTER*24  NOMFON
      CHARACTER*24 VALK(2)
C
C ROUTINES EXTERNES
C -----------------
C     EXTERNAL      CHVERI, GETRES, JEDEMA, JELIRA, JEMARQ, JEVEUO,
C    &              MDITM2, WKVECT
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      CALL JEMARQ( )
C
      IF ( (NEXCIT.EQ.0).OR.(NBF.EQ.0) )
     &   CALL U2MESS('F','ALGORITH5_49')
C
      N2 = NBM + 2
      DTTR = 0.0D0
C
      CALL WKVECT('&&MDITM1.TRANS' , 'V V R8', 2*2*NBM, JTRAN  )
C
      CALL WKVECT('&&MDITM1.AMOR00', 'V V R8', NBM    , IAMO00 )
      CALL WKVECT('&&MDITM1.PULSD' , 'V V R8', NBM    , IPULD  )
      CALL WKVECT('&&MDITM1.PULS00', 'V V R8', NBM    , IPUL00 )
      DO 10 IM = 1, NBM
         ZR(IAMO00+IM-1) = AMORI(IM)/MASGI(IM)
         ZR(IPULD +IM-1) = PULSI(IM)
         ZR(IPUL00+IM-1) = PULSI(IM)
  10  CONTINUE
C
      CALL WKVECT('&&MDITM1.FMOD0' , 'V V R8', NBM    , IFMO0  )
      CALL WKVECT('&&MDITM1.FMODA' , 'V V R8', NBM    , IFMOA  )
C
      CALL WKVECT('&&MDITM1.DEPG'  , 'V V R8', NBM    , IDEPG  )
      CALL WKVECT('&&MDITM1.VITG'  , 'V V R8', NBM    , IVITG  )
      CALL WKVECT('&&MDITM1.ACCG'  , 'V V R8', NBM    , IACCG  )
      CALL WKVECT('&&MDITM1.ACCG0' , 'V V R8', NBM    , IACCG0 )
C
      CALL WKVECT('&&MDITM1.FEXT'  , 'V V R8', NBF*NBM, IFX    )
      CALL WKVECT('&&MDITM1.FEXTTS', 'V V R8', NBF*NBM, IFXS   )
      CALL WKVECT('&&MDITM1.TEXT'  , 'V V R8', NBF    , ITX    )
      CALL WKVECT('&&MDITM1.TEXTTS', 'V V R8', NBF    , ITXS   )
      CALL WKVECT('&&MDITM1.FEXTTR', 'V V R8', NBM    , IFXTR  )
      CALL WKVECT('&&MDITM1.FEXTT0', 'V V R8', NBM    , IFX0   )
      VECGEN = TABEXC(1)
      CALL JEVEUO (VECGEN//'           .VALE','L',KTEXT)
      DO 20 I = 1, NBF
         ZR(ITX+I-1) = ZR(KTEXT+I-1)
  20  CONTINUE
      DTTR = (ZR(ITX+1) - ZR(ITX)) * 1.0D-02
      DO 30 I = 1, NEXCIT
         VECGEN = TABEXC(I)
         CALL JEVEUO (VECGEN//'           .VALE','L',KFEXT)
         IMODE = NUMEXC(I)
         DO 31 J = 1, NBF
            ZR(IFX+(IMODE-1)*NBF+J-1) = ZR(KFEXT+NBF+J-1)
  31     CONTINUE
  30  CONTINUE
C
      CALL WKVECT('&&MDITM1.OMEGAF', 'V V R8', NBF    , IOM    )
      CALL WKVECT('&&MDITM1.AA'    , 'V V R8', NBF*NBM, IAA    )
      CALL WKVECT('&&MDITM1.BB'    , 'V V R8', NBF*NBM, IBB    )
      CALL WKVECT('&&MDITM1.ZIN'   , 'V V C' , NBM    , IZIN   )
      CALL WKVECT('&&MDITM1.ZITR'  , 'V V C' , NBM    , IZITR  )
      CALL WKVECT('&&MDITM1.ZA1'   , 'V V C' , NBM    , IZA1   )
      CALL WKVECT('&&MDITM1.ZA2'   , 'V V C' , NBM    , IZA2   )
      CALL WKVECT('&&MDITM1.ZA3'   , 'V V C' , NBM    , IZA3   )
      CALL WKVECT('&&MDITM1.S0'    , 'V V C' , NBM    , IS0    )
      CALL WKVECT('&&MDITM1.SR0'   , 'V V C' , NBM    , ISR0   )
      CALL WKVECT('&&MDITM1.Z0'    , 'V V C' , NBM    , IZ0    )
      CALL WKVECT('&&MDITM1.ZA4'   , 'V V C' , NBF*NBM, IZA4   )
      CALL WKVECT('&&MDITM1.ZA5'   , 'V V C' , NBF*NBM, IZA5   )
C
C-----------------------------------------------------------------------
C     EN L'ABSCENCE DE NON-LINEARITES DE CHOC :
C     -> SORTIE EN ERREUR FATALE EN L'ETAT ACTUEL
C        PREVOIR LE DEVELOPPEMENT D'UNE PROCEDURE APPROPRIEE
C-----------------------------------------------------------------------
C
      IF ( NBNL.EQ.0 ) THEN
C
C
      CALL U2MESS('F','ALGORITH5_50')
C
C-----------------------------------------------------------------------
C     EN PRESENCE DE NON-LINEARITES DE CHOC :
C     -> CREATION D'OBJETS DE TRAVAIL COMPLEMENTAIRES
C     -> APPEL DE L'ALGORITHME ITMI
C-----------------------------------------------------------------------
C
      ELSE
C
C
      NITMAX = 150
      CALL WKVECT('&&MDITM1.VECDT' , 'V V R8', NITMAX+1, JDT    )
C
      CALL WKVECT('&&MDITM1.AMOR'  , 'V V R8', NBM     , IAMO   )
      CALL WKVECT('&&MDITM1.AMOR0' , 'V V R8', NBM     , IAMO0  )
      CALL WKVECT('&&MDITM1.PULS'  , 'V V R8', NBM     , IPUL   )
      CALL WKVECT('&&MDITM1.PULS0' , 'V V R8', NBM     , IPUL0  )
      DO 40 IM = 1, NBM
         ZR(IAMO  +IM-1) = AMORI(IM)/MASGI(IM)
         ZR(IAMO0 +IM-1) = AMORI(IM)/MASGI(IM)
         ZR(IPUL  +IM-1) = PULSI(IM)
         ZR(IPUL0 +IM-1) = PULSI(IM)
  40  CONTINUE
C
      CALL WKVECT('&&MDITM1.FMOD00', 'V V R8', NBM     , IFMO00 )
      CALL WKVECT('&&MDITM1.FMODT' , 'V V R8', NBM     , IFMOT  )
      CALL WKVECT('&&MDITM1.FMOD0T', 'V V R8', NBM     , IFMO0T )
      CALL WKVECT('&&MDITM1.FEXMOD', 'V V R8', NBM     , IFEXMO )
      CALL WKVECT('&&MDITM1.FNLMOD', 'V V R8', NBM     , IFNLMO )
      CALL WKVECT('&&MDITM1.FMRES' , 'V V R8', NBM     , IFMRES )
C
      CALL WKVECT('&&MDITM1.DEPGE' , 'V V R8', NBM     , IDEPGE )
      CALL WKVECT('&&MDITM1.DEPGC' , 'V V R8', NBM     , IDEPGC )
      CALL WKVECT('&&MDITM1.DEPGT' , 'V V R8', NBM     , IDEPGT )
      CALL WKVECT('&&MDITM1.DEPG0T', 'V V R8', NBM     , JDEPGT )
      CALL WKVECT('&&MDITM1.VITGE' , 'V V R8', NBM     , IVITGE )
      CALL WKVECT('&&MDITM1.VITGC' , 'V V R8', NBM     , IVITGC )
      CALL WKVECT('&&MDITM1.VITGT' , 'V V R8', NBM     , IVITGT )
      CALL WKVECT('&&MDITM1.VITG0T', 'V V R8', NBM     , JVITGT )
      CALL WKVECT('&&MDITM1.ACCGT' , 'V V R8', NBM     , IACCGT )
C
      CALL WKVECT('&&MDITM1.KMOD'  , 'V V R8', NBM*NBM , IKMO   )
      CALL WKVECT('&&MDITM1.CMOD'  , 'V V R8', NBM*NBM , ICMO   )
      CALL WKVECT('&&MDITM1.KMOD0' , 'V V R8', NBM*NBM , IKMO0  )
      CALL WKVECT('&&MDITM1.CMOD0' , 'V V R8', NBM*NBM , ICMO0  )
      CALL WKVECT('&&MDITM1.KMOD00', 'V V R8', NBM*NBM , IKMO00 )
      CALL WKVECT('&&MDITM1.CMOD00', 'V V R8', NBM*NBM , ICMO00 )
      CALL WKVECT('&&MDITM1.KMODCA', 'V V R8', NBM*NBM , IKMOCA )
      CALL WKVECT('&&MDITM1.CMODCA', 'V V R8', NBM*NBM , ICMOCA )
      CALL WKVECT('&&MDITM1.CMODFA', 'V V R8', NBM*NBM , ICMOFA )
      CALL WKVECT('&&MDITM1.AMFLU0', 'V V R8', NBM*NBM , JFLU0  )
      CALL WKVECT('&&MDITM1.AMFLUC', 'V V R8', NBM*NBM , JFLUC  )
C
      CALL WKVECT('&&MDITM1.TTR'   , 'V V R8', N2*NBM  , ITR    )
      CALL WKVECT('&&MDITM1.VG'    , 'V V R8', NBM*NBM , IVG    )
      CALL WKVECT('&&MDITM1.VD'    , 'V V R8', NBM*NBM , IVD    )
      CALL WKVECT('&&MDITM1.VG0'   , 'V V R8', NBM*NBM , IVG0   )
      CALL WKVECT('&&MDITM1.VD0'   , 'V V R8', NBM*NBM , IVD0   )
      CALL WKVECT('&&MDITM1.VVG'   , 'V V R8', NBM*NBM , IVVG   )
      CALL WKVECT('&&MDITM1.RR'    , 'V V R8', NBM     , IRR    )
      CALL WKVECT('&&MDITM1.RI'    , 'V V R8', NBM     , IRI    )
      CALL WKVECT('&&MDITM1.RR0'   , 'V V R8', NBM     , IRR0   )
C
      CALL WKVECT('&&MDITM1.INDX'  , 'V V I' , NBM     , JIX    )
      CALL WKVECT('&&MDITM1.INDXF' , 'V V I' , NBM     , JIXF   )
      CALL WKVECT('&&MDITM1.INTGE1', 'V V I' , NBM     , JI1    )
      CALL WKVECT('&&MDITM1.INTGE2', 'V V I' , NBM     , JI2    )
      CALL WKVECT('&&MDITM1.MTMP1' , 'V V R8', NBM*NBM , IM1    )
      CALL WKVECT('&&MDITM1.MTMP2' , 'V V R8', NBM*NBM , IM2    )
      CALL WKVECT('&&MDITM1.MTMP6' , 'V V R8', 3*NBM   , IM6    )
      CALL WKVECT('&&MDITM1.FTMP'  , 'V V R8', NBM     , IFTMP  )
      CALL WKVECT('&&MDITM1.VECDD' , 'V V R8', NBM     , IDD    )
      CALL WKVECT('&&MDITM1.VECU'  , 'V V R8', NBM     , IU     )
      CALL WKVECT('&&MDITM1.VECW'  , 'V V R8', NBM     , IW     )
      CALL WKVECT('&&MDITM1.LOCFLC', 'V V L' , NBM     , ILOCFC )
      CALL WKVECT('&&MDITM1.LOC'   , 'V V L' , NBM     , ILOC   )
C
      CALL GETRES(RESU,TYPRES,NOMCMD)
      CALL JEVEUO(RESU//'           .NCHO','E' ,KNCHO)
      CALL JEVEUO(RESU//'           .INTI','E' ,KINTI)
      DO 50 IC = 1, NBNL
         ZK8(KINTI+IC-1) = INTITU(IC)
         ZK8(KNCHO+IC-1) = NOECHO(IC,1)
  50  CONTINUE
C
      CALL WKVECT('&&MDITM1.TCONF1', 'V V R8', 4*NBNL  , JC1    )
      CALL WKVECT('&&MDITM1.TCONF2', 'V V R8', 4*NBNL  , JC2    )
      CALL WKVECT('&&MDITM1.TCONFE', 'V V R8', 4*NBNL  , JCE    )
      CALL WKVECT('&&MDITM1.ICONFB', 'V V I' , NBNL    , JCB    )
      CALL WKVECT('&&MDITM1.ITFORN', 'V V I' , NBNL    , JIFN   )
      CALL WKVECT('&&MDITM1.TYPCH' , 'V V I' , NBNL    , JTYPCH )
      CALL WKVECT('&&MDITM1.NBSEG' , 'V V I' , NBNL    , JNS    )
      CALL WKVECT('&&MDITM1.OLDIA' , 'V V I' , NBNL    , JIA    )
C
      CALL WKVECT('&&MDITM1.CHOC'  , 'V V R8', 6*NBNL  , ICHO   )
      CALL WKVECT('&&MDITM1.ORIG'  , 'V V R8', 6*NBNL  , IORIG  )
      CALL WKVECT('&&MDITM1.ALPHA' , 'V V R8', 2*NBNL  , IALP   )
      CALL WKVECT('&&MDITM1.BETA'  , 'V V R8', 2*NBNL  , IBET   )
      CALL WKVECT('&&MDITM1.GAMMA' , 'V V R8', 2*NBNL  , IGAM   )
      CALL WKVECT('&&MDITM1.OLDF'  , 'V V R8', 9*NBNL  , IOLDF  )
      CALL WKVECT('&&MDITM1.NOCH'  , 'V V K8', NBNL    , IH     )
      DO 60 IC = 1, NBNL
         ZR(ICHO  + 6*(IC-1) + 1 - 1) = PARCHO(IC,2)
         ZR(ICHO  + 6*(IC-1) + 2 - 1) = PARCHO(IC,3)
         ZR(ICHO  + 6*(IC-1) + 3 - 1) = PARCHO(IC,4)
         ZR(ICHO  + 6*(IC-1) + 4 - 1) = PARCHO(IC,5)
         ZR(ICHO  + 6*(IC-1) + 5 - 1) = PARCHO(IC,6)
         ZR(ICHO  + 6*(IC-1) + 6 - 1) = PARCHO(IC,7)
         ZR(IORIG + 6*(IC-1) + 1 - 1) = PARCHO(IC,14)
         ZR(IORIG + 6*(IC-1) + 2 - 1) = PARCHO(IC,15)
         ZR(IORIG + 6*(IC-1) + 3 - 1) = PARCHO(IC,16)
         ZR(IORIG + 6*(IC-1) + 4 - 1) = PARCHO(IC,8)
         ZR(IORIG + 6*(IC-1) + 5 - 1) = PARCHO(IC,9)
         ZR(IORIG + 6*(IC-1) + 6 - 1) = PARCHO(IC,10)
         ZR(IALP  + 2*(IC-1) + 1 - 1) = PARCHO(IC,17)
         ZR(IALP  + 2*(IC-1) + 2 - 1) = PARCHO(IC,18)
         ZR(IBET  + 2*(IC-1) + 1 - 1) = PARCHO(IC,19)
         ZR(IBET  + 2*(IC-1) + 2 - 1) = PARCHO(IC,20)
         ZR(IGAM  + 2*(IC-1) + 1 - 1) = PARCHO(IC,21)
         ZR(IGAM  + 2*(IC-1) + 2 - 1) = PARCHO(IC,22)
  60  CONTINUE
C
      NBSEG0 = 1
      DO 70 IC = 1, NBNL
         ZK8(IH+IC-1) = NOECHO(IC,1)
         IF ( NOECHO(IC,9).EQ.'PLAN_Y  ' ) THEN
            ZI(JTYPCH+IC-1) = 0
            ZI(JNS+IC-1) = 1
         ELSE IF ( NOECHO(IC,9).EQ.'PLAN_Z  ' ) THEN
            ZI(JTYPCH+IC-1) = 1
            ZI(JNS+IC-1) = 1
         ELSE IF ( NOECHO(IC,9).EQ.'CERCLE  ' ) THEN
            ZI(JTYPCH+IC-1) = 2
            ZI(JNS+IC-1) = 1
         ELSE IF ( NOECHO(IC,9).EQ.'BI_CERCL' .OR.
     &             NOECHO(IC,9).EQ.'BI_PLANY' .OR.
     &             NOECHO(IC,9).EQ.'BI_PLANZ' ) THEN
             VALK(1) = NOECHO(IC,9)
             VALK(2) = NOECHO(IC,1)
             CALL U2MESK('F','ALGORITH5_51', 2 ,VALK)
         ELSE
            ZI(JTYPCH+IC-1) = 3
            NOMOBJ = NOECHO(IC,9)
            CALL TBLIVA(NOMOBJ,1,'LIEU',
     &                 IBID,R8BID,CBID,'DEFIOBST',KBID,R8BID,'FONCTION',
     &                 K8TYP,IBID,R8BID,CBID,NOMFON,IRETT)
            CALL ASSERT(IRETT.EQ.0)
            CALL JELIRA(NOMFON(1:19)//'.VALE','LONMAX',NBVAL,KBID)
            NBSEG = NBVAL/2
            ZI(JNS+IC-1) = NBSEG
            IF ( NBSEG.GT.NBSEG0 ) NBSEG0 = NBSEG
         ENDIF
  70  CONTINUE
      NP3 = NBSEG0
      CALL WKVECT('&&MDITM1.RC'    , 'V V R8', NP3*NBNL, IRC    )
      CALL WKVECT('&&MDITM1.THETA' , 'V V R8', NP3*NBNL, ITHE   )
      DO 80 IC = 1, NBNL
         IF ( ZI(JTYPCH+IC-1).EQ.3 ) THEN
            NOMOBJ = NOECHO(IC,9)
            CALL TBLIVA(NOMOBJ,1,'LIEU',
     &                 IBID,R8BID,CBID,'DEFIOBST',KBID,R8BID,'FONCTION',
     &                 K8TYP,IBID,R8BID,CBID,NOMFON,IRETT)
            CALL ASSERT(IRETT.EQ.0)
            CALL JEVEUO(NOMFON(1:19)//'.VALE', 'L', IDTHET)
            CALL JELIRA(NOMFON(1:19)//'.VALE','LONMAX',NBVAL,KBID)
            NBSEG = NBVAL/2
            IDRAYO = IDTHET + NBSEG
            DO 81 I = 1, NBSEG
               ZR(IRC+NBSEG0*(IC-1)+I-1) = ZR(IDRAYO + I - 1)
               ZR(ITHE+NBSEG0*(IC-1)+I-1) = ZR(IDTHET + I - 1)
  81        CONTINUE
         ELSE
            ZR(IRC+NBSEG0*(IC-1)) = PARCHO(IC,1)
         ENDIF
  80  CONTINUE
      CALL CHVERI( NBM,NBNL,NP3,NBM,NBM,NBNL,ZI(JTYPCH),ZI(JNS),
     &             PHII,NOECHO,ZR(IALP),ZR(IBET),ZR(IGAM),
     &             ZR(IORIG),ZR(IRC),ZR(ITHE),ZR(IDEPG))
C
      CALL WKVECT('&&MDITM1.DEP'   , 'V V R8', 3*NBNL  , JDEP   )
      CALL WKVECT('&&MDITM1.VIT'   , 'V V R8', 3*NBNL  , JVIT   )
      CALL WKVECT('&&MDITM1.ACC'   , 'V V R8', 3*NBNL  , JACC   )
      CALL WKVECT('&&MDITM1.DEP0'  , 'V V R8', 3*NBNL  , JDEP0  )
      CALL WKVECT('&&MDITM1.VIT0'  , 'V V R8', 3*NBNL  , JVIT0  )
      CALL WKVECT('&&MDITM1.ACC0'  , 'V V R8', 3*NBNL  , JACC0  )
C
      CALL MDITM2(NBNL,NP3,NBF,N2,NBM,NBMCD,NBMP,NBNL,INDIC,IMPR,ITRANS,
     &EPST,ICOUPL,TPFL,VECI1,LOCFL0,DT0,TFEXM,TS,DTTR,ZR(JDT),IARCH,
     &VITG0,DEPG0,MASGI,AMORI,PULSI,PHII,VECR5,VECR3,VECR1,VECR2,VGAP,
     &VECR4,XSI0,NBSAUV,ZI(JIX),ZI(JIXF),ZI(JI1),ZI(JI2),ZI(JCB),
     &ZR(JC1),ZR(JC2),ZR(JCE),ZI(JTYPCH),ZI(JNS),ZI(JIA),ZI(JIFN),
     &ZR(IAMO),ZR(IAMO0),ZR(IAMO00),ZR(IPUL),ZR(IPUL0),ZR(IPUL00),
     &ZR(JTRAN),ZR(IPULD),ZR(IFMO0),ZR(IFMO00),ZR(IFMOT),ZR(IFMO0T),
     &ZR(IFEXMO),ZR(IFNLMO),ZR(IFMOA),ZR(IFMRES),ZR(IDEPG),ZR(IDEPGE),
     &ZR(IDEPGC),ZR(IDEPGT),ZR(JDEPGT),ZR(IVITG),ZR(IVITGE),ZR(IVITGC),
     &ZR(IVITGT),ZR(JVITGT),ZR(IACCG),ZR(IACCG0),ZR(IACCGT),ZR(JDEP),
     &ZR(JVIT),ZR(JACC),ZR(JDEP0),ZR(JVIT0),ZR(JACC0),ZR(IKMO),ZR(ICMO),
     &ZR(IKMO0),ZR(ICMO0),ZR(IKMO00),ZR(ICMO00),ZR(IKMOCA),ZR(ICMOCA),
     &ZR(ICMOFA),ZR(JFLU0),ZR(JFLUC),ZR(IVG),ZR(IVD),ZR(ITR),ZR(IVG0),
     &ZR(IVD0),ZR(IVVG),ZR(IRR),ZR(IRI),ZR(IRR0),ZR(IM1),ZR(IM2),
     &ZR(IM6),ZR(IFTMP),ZR(IDD),ZR(IU),ZR(IW),ZR(IOM),ZR(IAA),ZR(IBB),
     &ZR(IFX),ZR(IFXS),ZR(ITX),ZR(ITXS),ZR(IFXTR),
     &ZR(IFX0),ZK8(IH),ZR(ICHO),ZR(IORIG),ZR(IRC),ZR(ITHE),ZR(IALP),
     &ZR(IBET),ZR(IGAM),ZR(IOLDF),ZL(ILOCFC),ZL(ILOC),ZC(IS0),ZC(IZ0),
     &ZC(ISR0),ZC(IZA1),ZC(IZA2),ZC(IZA3),ZC(IZA4),ZC(IZA5),ZC(IZIN),
     &ZC(IZITR),NBCHOC,PARCHO,NOECHO)

      ENDIF
C
      CALL JEDEMA( )
C
C --- FIN DE MDITM1.
      END
