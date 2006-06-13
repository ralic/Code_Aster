      SUBROUTINE RVCHLO(EPSI,SSCH19,NBCP,NBCO,NBSP,NBCM,NBSM,M,F,N,R,
     +                  VALCP)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 06/04/2004   AUTEUR DURAND C.DURAND 
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
      INTEGER      M,F(*),N,NBCP,NBCM,NBSM,NBCO,NBSP
      CHARACTER*19 SSCH19
      REAL*8       R(*),VALCP(*),EPSI
C
C**********************************************************************
C
C  OPERATION REALISEE
C  ------------------
C
C     SAISIE DES VALEURS DE CMPS EN DES POINTS DE FACES D' UNE MAILLE
C
C  ARGUMENTS EN ENTREE
C  -------------------
C
C     SSCH19 : NOM DU SOUS_CHAMP_GD
C     M      : NUMERO DE LA MAILLE
C     F      : TABLE DES FACES
C     R      : TABLE VALEUR DU PARAMETRE DE REPERAGE
C              POUR LE POINT CONSIDERE (2D : 1 PARAM, 3D : 2 PARAM)
C
C  ARGUMENTS EN SORTIE
C  -------------------
C
C     VALCP   : TABLE DES VALEURS DES CMP (DANS L' ORDRE D' APPARITION
C               DANS L' EXTRACTION)
C
C**********************************************************************
C
C  FONCTIONS EXTERNES
C  ------------------
C
      CHARACTER*32 JEXNOM,JEXNUM
C
C  DECLARATION DES COMMUNS NORMALISES JEVEUX
C  -----------------------------------------
C
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16    ZK16
      CHARACTER*24    ZK24
      CHARACTER*32    ZK32
      CHARACTER*80    ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C  FIN DES COMMUNS NORMALISES JEVEUX
C  ---------------------------------
C
C  VARIABLES LOCALES
C  -----------------
C
      CHARACTER*8  MAILLA,TYPMAI
      CHARACTER*4  DOCU
      CHARACTER*24 NPCMP,NVALE,NPADR,NPNBN,NVALCM,NNDFAC
C
      INTEGER APCMP,AVALE,APADR,APNBN,AVALCM,ANDFAC
      INTEGER ADRM,I,ADR,NBNF,J,NBF,NBNM,FAC,IATYMA
      REAL*8  ACC
      CHARACTER*1 K1BID
C
C==================== CORPS DE LA ROUTINE =============================
C
      CALL JEMARQ()
CC    WRITE(IFR,*)'%%%%%%%%%%% '
CC    WRITE(IFR,*)'RVCHLO : IN '
CC    WRITE(IFR,*)'%%%%%%%%%%% '
C
      NVALCM = '&&RVCHLO.VAL.CMP.ND.MAIL'
      NNDFAC = '&&RVCHLO.NUM.LOC.ND.FACE'
C
      CALL JEVEUO(SSCH19//'.NOMA','L',ADR)
C
      MAILLA = ZK8(ADR)
C
      NVALE = SSCH19//'.VALE'
      NPCMP = SSCH19//'.PCMP'
      NPADR = SSCH19//'.PADR'
      NPNBN = SSCH19//'.PNBN'
C
      CALL JEVEUO(NVALE,'L',AVALE)
      CALL JEVEUO(NPCMP,'L',APCMP)
      CALL JEVEUO(NPADR,'L',APADR)
      CALL JELIRA(NVALE,'DOCU',I,DOCU)
C
C  VALEUR DES CMP SUR TOUTE LA MAILLE
C  ----------------------------------
C
      IF ( DOCU .EQ. 'CHNO' ) THEN
C
         CALL JELIRA(JEXNUM(MAILLA//'.CONNEX',M),'LONMAX',NBNM,K1BID)
C
C
      ELSE IF ( DOCU .EQ. 'CHLM' ) THEN
C
         CALL JEVEUO(NPNBN,'L',APNBN)
C
         NBNM = ZI(APNBN + M-1)
C
      ELSE
C
C        /* EN CAS D' EVOLUTION ... */
C
      ENDIF
C
      CALL WKVECT(NVALCM,'V V R',NBCP*NBNM*NBCM*NBSM,AVALCM)
C
      IF ( DOCU .EQ. 'CHNO' ) THEN
C
         CALL JEVEUO(JEXNUM(MAILLA//'.CONNEX',M),'L',ADRM)
C
         DO 10, I = 1, NBNM, 1
C
            DO 11, J = 1, NBCP, 1
C
               ZR(AVALCM + (I-1)*NBCP + J-1) =
     +         ZR(AVALE + ZI(APADR + ZI(ADRM + I-1)-1)+J-2)
C
11          CONTINUE
C
10       CONTINUE
C
      ELSE IF ( DOCU .EQ. 'CHLM' ) THEN
C
         DO 12, I = 1, NBCP*NBNM*NBCM*NBSM, 1
C
               ZR(AVALCM + I-1) =
     +         ZR(AVALE + ZI(APADR + M-1)+I-2)
C
12       CONTINUE
C
CC       CALL JXVERI('RESULTAT','AP : 12')
      ELSE
C
C        /* EN CAS D' EVOLUTION ... */
C
      ENDIF
C
      DO 150, I = 1, N, 1
C
C        DESCRIPTEUR DE FACE
C        -------------------
C
         FAC = F(I)
CC       WRITE(IFR,*)'POINT : ',I,' FACE : ',FAC
C
         IF ( FAC .GT. 0 ) THEN
C
            CALL JEVEUO(MAILLA//'.TYPMAIL','L',IATYMA)
            ADR=IATYMA-1+M
            CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ADR)),TYPMAI)
C
            IF ( TYPMAI .EQ. 'POI1' ) THEN
C
               NBNF   = 1
               NBF    = 1
C
               CALL WKVECT(NNDFAC,'V V I',NBNF,ANDFAC)
C
               ZI(ANDFAC + 1-1) = 1
C
            ELSE IF ((TYPMAI .EQ. 'SEG2').OR.(TYPMAI .EQ. 'SEG3')) THEN
C
               NBNF   = 1
               NBF    = 1
C
               CALL WKVECT(NNDFAC,'V V I',NBNF,ANDFAC)
C
               ZI(ANDFAC + 1-1) = FAC
C
            ELSE IF ( TYPMAI .EQ. 'TRIA3' ) THEN
C
               NBNF   = 3
               NBF    = 3
C
               CALL WKVECT(NNDFAC,'V V I',NBNF,ANDFAC)
C
               ZI(ANDFAC + 1-1) = FAC
               ZI(ANDFAC + 2-1) = MOD(FAC,NBF) + 1
               ZI(ANDFAC + 3-1) = 0
C
            ELSE IF ( TYPMAI .EQ. 'TRIA6' ) THEN
C
               NBNF   = 3
               NBF    = 3
C
               CALL WKVECT(NNDFAC,'V V I',NBNF,ANDFAC)
C
               ZI(ANDFAC + 1-1) = FAC
               ZI(ANDFAC + 2-1) = MOD(FAC,NBF) + 1
               ZI(ANDFAC + 3-1) = FAC + NBF
C
            ELSE IF ( TYPMAI .EQ. 'QUAD4' ) THEN
C
               NBNF   = 3
               NBF    = 4
C
               CALL WKVECT(NNDFAC,'V V I',NBNF,ANDFAC)
C
               ZI(ANDFAC + 1-1) = FAC
               ZI(ANDFAC + 2-1) = MOD(FAC,NBF) + 1
               ZI(ANDFAC + 3-1) = 0
C
            ELSE IF ((TYPMAI.EQ.'QUAD8').OR.(TYPMAI.EQ.'QUAD9')) THEN
C
               NBNF   = 3
               NBF    = 4
C
               CALL WKVECT(NNDFAC,'V V I',NBNF,ANDFAC)
C
               ZI(ANDFAC + 1-1) = FAC
               ZI(ANDFAC + 2-1) = MOD(FAC,NBF) + 1
               ZI(ANDFAC + 3-1) = FAC + NBF
C
            ELSE
C
C           /* TRAITEMENT 3D */
C
            ENDIF
C
            DO 151, J = 1, NBCO, 1
C
               CALL RVECHB(EPSI,TYPMAI,ZI(ANDFAC),R(I),
     +               ZR(AVALCM + (J-1)*NBNM*NBSM*NBCP),
     +               NBCP,NBSP,NBSM,
     +               VALCP(1 + ((J-1)*N+I-1)*NBSP*NBCP))
C
CC       CALL JXVERI('RESULTAT','AP : RVECHB')
151         CONTINUE
C
            CALL JEDETR(NNDFAC)
C
         ELSE
C
            DO 155, J = 1, NBCO, 1
C
               DO 156, K = 1, NBCP*NBSP, 1
C
                  ACC = 0.0D0
                  IND = (J-1)*NBNM*NBSM*NBCP + K-1
C
                  DO 156, L = 1, NBNM, 1
C
                     ACC = ACC + ZR(AVALCM + IND + (L-1)*NBSM*NBCP )
C
157               CONTINUE
C
                  VALCP(((J-1)*N+I-1)*NBSP*NBCP+K) = ACC/NBNM
C
156            CONTINUE
C
155         CONTINUE
C
         ENDIF
C
150   CONTINUE
C
      CALL JEDETR(NVALCM)
C
      CALL JEDEMA()
      END
