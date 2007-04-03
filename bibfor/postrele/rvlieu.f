      SUBROUTINE RVLIEU ( MAILLA, TYPCO, COURBE, NLSNAC, SDLIEU )
      IMPLICIT   NONE
      CHARACTER*24        NLSNAC, SDLIEU
      CHARACTER*8         TYPCO, COURBE, MAILLA
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/04/2007   AUTEUR VIVAN L.VIVAN 
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
C     ------------------------------------------------------------------
C     GENERATION DE LA SD LIEU (MAILLAGE DU LIEU)
C     ------------------------------------------------------------------
C IN  COURBE : K : NOM DU CONCEPT COURBE
C IN  MAILLA : K : NOM DU MAILLAGE
C IN  NLSNAC : K :  NOM DU VECTEUR DES NOEUDS ACTIFS
C OUT SDLIEU : K : NOM DU VECTEUR DES NOMS DE SD LIEU
C     ------------------------------------------------------------------
C     ORGANISATION SD_LIEU
C       .REFE : S E K8 DOCU(/'SGTD'/'SGT3'/'ARCC'/'CHMM'/'LSTN')
C               <-- NOM_MAILLAGE CAS LSTN, NOM_COURBE AUTRE CAS
C       .ABSC : XD V R NB_OC = NBR_PART_CONNEXE
C               <-- ABSCISSES CURVILIGNE
C       .COOR : XD V R  NB_OC = NBR_PART_CONNEXE
C               <-- COORDONNEE X, Y ET Z
C       .DESC : CHOIX DOCU PARMI
C               -----      -----
C                 SGT3      : (XA,YA,ZA,XB,YB,ZB)
C                 SGTD      : (XA,YA,   XB,YB   )
C                 ARCC      : (XC,YC,R,S1,S2)
C                 CHMM,LSTN : LISTE DES NOMS DE NOEUDS
C               FIN_CHOIX
C               ---------
C        .NUME : S E I
C               <-- NUMERO DE PARTIE DANS LE CAS D' UNE COURBE
C     ------------------------------------------------------------------
C
      CHARACTER*32 JEXNUM
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
      CHARACTER*24 NABSC,NREFE,NDESC,LNUMND,NNUME,NCOOR,NCRB3D
      CHARACTER*19 SDCOUR
      CHARACTER*10 IDEN
      CHARACTER*4  DOCU
      INTEGER      AABSC,AREFE,ADESC,AASGT,ABSGT,ACARC,ASARC,ARARC,ACOOR
      INTEGER      ANSDL,ACNXO,ACNXE,ADR,ACHM,AOSGT,AESGT,ANUMND,ANUME
      INTEGER      NBSD,NBSGT,NBARC,ISD,NBPT,IPT,IDEB,IFIN,IOC,NBM,NBOC
      INTEGER      ASDS
      LOGICAL      SGTARC,OKCRB,SGT3D
      REAL*8       A,B,C,D,E,F,S,L,ZERO
      CHARACTER*1 K1BID
C
C====================== CORPS DE LA ROUTINE ===========================
C
      CALL JEMARQ()
      OKCRB  = .FALSE.
      SGTARC = .FALSE.
      ZERO   = 0.0D0
      LNUMND = '&&RVLIEU.LISTE.NUM.NOEUD'
      IF ( TYPCO .EQ. 'CHEMIN' ) THEN
         OKCRB = .TRUE.
         CALL JEEXIN(COURBE//'.TYPCOURBE',ADR)
         IF ( ADR .NE. 0 ) THEN
            CALL JEVEUO(COURBE//'.TYPCOURBE','L',ADR)
            SGTARC = ( ZK8(ADR) .EQ. 'SGTDARCC' )
            SGT3D  = .FALSE.
            IF ( SGTARC ) THEN
               CALL JELIRA(COURBE//'.XYASGT','LONMAX',NBSGT,K1BID)
               CALL JELIRA(COURBE//'.XYCARC','LONMAX',NBARC,K1BID)
               CALL JEVEUO(COURBE//'.XYASGT','L',AASGT)
               CALL JEVEUO(COURBE//'.XYBSGT','L',ABSGT)
               CALL JEVEUO(COURBE//'.XYCARC','L',ACARC)
               CALL JEVEUO(COURBE//'.XRARC' ,'L',ARARC)
               CALL JEVEUO(COURBE//'.XSARC' ,'L',ASARC)
               NBSGT = (NBSGT/2) - 1
               NBARC = (NBARC/2) - 1
               NBSD  =  NBARC + NBSGT
               IF ( NBARC.NE.0 .AND. NBSGT.NE.0 ) THEN
                  CALL U2MESS('F','POSTRELE_23')
               ENDIF
            ELSE
               CALL JELIRA(COURBE//'.CHEMIN','NMAXOC',NBSD,K1BID)
               CALL JEVEUO(COURBE//'.CHEMIN','L',ACHM)
            ENDIF
         ELSE
            CALL JELIRA(COURBE//'.NSDS','LONMAX',NBSD,K1BID)
            CALL JEVEUO(COURBE//'.NSDS','L',ASDS)
            SGT3D = .TRUE.
         ENDIF
      ELSE
         CALL JELIRA(NLSNAC,'LONMAX',NBPT,K1BID)
         NBSD = 1
      ENDIF
      CALL WKVECT(SDLIEU,'V V K24',NBSD,ANSDL)
      DO 100, ISD = 1, NBSD, 1
         CALL CODENT(ISD,'G',IDEN)
         SDCOUR = '&&RVLIEU.'//IDEN
         ZK24(ANSDL + ISD-1)(1:19) = SDCOUR
         NREFE = SDCOUR//'.REFE'
         NABSC = SDCOUR//'.ABSC'
         NDESC = SDCOUR//'.DESC'
         NNUME = SDCOUR//'.NUME'
         NCOOR = SDCOUR//'.COOR'
         CALL JECREO(NREFE,'V E K8')
         CALL JEVEUO(NREFE,'E',AREFE)
         CALL JECREO(NNUME,'V E I')
         CALL JEVEUO(NNUME,'E',ANUME)
         ZI(ANUME) = ISD
         IF ( OKCRB .AND. SGTARC ) THEN
            CALL JELIRA(JEXNUM(COURBE//'.CNXOR',ISD),'LONMAX',
     &                  NBOC,K1BID)
            CALL JEVEUO(JEXNUM(COURBE//'.CNXOR',ISD),'L',ACNXO)
            CALL JEVEUO(JEXNUM(COURBE//'.CNXEX',ISD),'L',ACNXE)
            CALL JEVEUO(JEXNUM(COURBE//'.ORSGT',ISD),'L',AOSGT)
            CALL JEVEUO(JEXNUM(COURBE//'.EXSGT',ISD),'L',AESGT)
            CALL JECREC(NABSC,'V V R','NU','DISPERSE','VARIABLE',NBOC)
            CALL JECREC(NCOOR,'V V R','NU','DISPERSE','VARIABLE',NBOC)
            ZK8(AREFE) = COURBE
            IF ( ISD .LE. NBSGT ) THEN
               DOCU = 'SGTD'
               CALL WKVECT(NDESC,'V V R',4,ADESC)
               A               = ZR(AASGT + 2*ISD + 1-1)
               B               = ZR(AASGT + 2*ISD + 2-1)
               C               = ZR(ABSGT + 2*ISD + 1-1)
               D               = ZR(ABSGT + 2*ISD + 2-1)
               ZR(ADESC + 1-1) = A
               ZR(ADESC + 2-1) = B
               ZR(ADESC + 3-1) = C
               ZR(ADESC + 4-1) = D
               C               = C - A
               D               = D - B
               L               = SQRT(C*C+D*D)
               DO 110, IOC = 1, NBOC, 1
                  IDEB = ZI(ACNXO + IOC-1)
                  IFIN = ZI(ACNXE + IOC-1)
                  NBPT = IFIN - IDEB + 2
                  CALL JECROC(JEXNUM(NABSC,IOC))
                  CALL JEECRA(JEXNUM(NABSC,IOC),'LONMAX',NBPT,' ')
                  CALL JEVEUO(JEXNUM(NABSC,IOC),'E',AABSC)
                  CALL JECROC(JEXNUM(NCOOR,IOC))
                  CALL JEECRA(JEXNUM(NCOOR,IOC),'LONMAX',3*NBPT,' ')
                  CALL JEVEUO(JEXNUM(NCOOR,IOC),'E',ACOOR)
                  DO 10, IPT = 1, NBPT-1, 1
                     S = ZR(AOSGT + IDEB-1 + IPT-1)
                     ZR(AABSC + IPT-1)           = S*L
                     ZR(ACOOR + 3*(IPT-1) + 1-1) = A + S*C
                     ZR(ACOOR + 3*(IPT-1) + 2-1) = B + S*D
                     ZR(ACOOR + 3*(IPT-1) + 3-1) = ZERO
10                CONTINUE
                  S = ZR(AESGT + IFIN-1)
                  ZR(AABSC + NBPT-1) = S*L
                  ZR(ACOOR + 3*(IPT-1) + 1-1) = A + S*C
                  ZR(ACOOR + 3*(IPT-1) + 2-1) = B + S*D
                  ZR(ACOOR + 3*(IPT-1) + 3-1) = ZERO
110            CONTINUE
            ELSE
               DOCU = 'ARCC'
               CALL WKVECT(NDESC,'V V R',5,ADESC)
               A = ZR(ACARC + 2*(ISD-NBSGT) + 1-1)
               B = ZR(ACARC + 2*(ISD-NBSGT) + 2-1)
               C = ZR(ARARC +   (ISD-NBSGT) + 1-1)
               D = ZR(ASARC + 2*(ISD-NBSGT) + 1-1)
               E = ZR(ASARC + 2*(ISD-NBSGT) + 2-1)
               ZR(ADESC + 1-1) = A
               ZR(ADESC + 2-1) = B
               ZR(ADESC + 3-1) = C
               ZR(ADESC + 4-1) = D
               ZR(ADESC + 5-1) = E
               DO 111, IOC = 1, NBOC, 1
                  IDEB = ZI(ACNXO + IOC-1)
                  IFIN = ZI(ACNXE + IOC-1)
                  NBPT = IFIN - IDEB + 2
                  CALL JECROC(JEXNUM(NABSC,IOC))
                  CALL JEECRA(JEXNUM(NABSC,IOC),'LONMAX',NBPT,' ')
                  CALL JEVEUO(JEXNUM(NABSC,IOC),'E',AABSC)
                  CALL JECROC(JEXNUM(NCOOR,IOC))
                  CALL JEECRA(JEXNUM(NCOOR,IOC),'LONMAX',3*NBPT,' ')
                  CALL JEVEUO(JEXNUM(NCOOR,IOC),'E',ACOOR)
                  DO 11, IPT = 1, NBPT-1, 1
                     S = ZR(AOSGT + IDEB-1 + IPT-1)
                     ZR(AABSC + IPT-1)           = S*C
                     ZR(ACOOR + 3*(IPT-1) + 1-1) = A + C*COS(S)
                     ZR(ACOOR + 3*(IPT-1) + 2-1) = B + C*SIN(S)
                     ZR(ACOOR + 3*(IPT-1) + 3-1) = ZERO
11                CONTINUE
                  S                           = ZR(AESGT + IFIN-1)
                  ZR(AABSC + IPT-1)           = S*C
                  ZR(ACOOR + 3*(IPT-1) + 1-1) = A + C*COS(S)
                  ZR(ACOOR + 3*(IPT-1) + 2-1) = B + C*SIN(S)
                  ZR(ACOOR + 3*(IPT-1) + 3-1) = ZERO
111            CONTINUE
            ENDIF
         ELSE IF ( OKCRB .AND. SGT3D ) THEN
            NCRB3D     =  ZK24(ASDS + ISD-1)
            DOCU       = 'SGT3'
            ZK8(AREFE) =  COURBE
            CALL JELIRA(NCRB3D(1:13)//'.CONEX.ORIG','LONMAX',
     &                  NBOC,K1BID)
            CALL JEVEUO(NCRB3D(1:13)//'.SGTEL.ORIG','L',AOSGT)
            CALL JEVEUO(NCRB3D(1:13)//'.SGTEL.EXTR','L',AESGT)
            CALL JEVEUO(NCRB3D(1:13)//'.CONEX.ORIG','L',ACNXO)
            CALL JEVEUO(NCRB3D(1:13)//'.CONEX.EXTR','L',ACNXE)
            CALL JEVEUO(NCRB3D(1:13)//'.DESC'      ,'L',AASGT)
            CALL JECREC(NABSC,'V V R','NU','DISPERSE','VARIABLE',NBOC)
            CALL JECREC(NCOOR,'V V R','NU','DISPERSE','VARIABLE',NBOC)
            CALL WKVECT(NDESC,'V V R',6,ADESC)
            A = ZR(AASGT + 1-1)
            B = ZR(AASGT + 2-1)
            C = ZR(AASGT + 3-1)
            D = ZR(AASGT + 4-1)
            E = ZR(AASGT + 5-1)
            F = ZR(AASGT + 6-1)
            ZR(ADESC + 1-1) = A
            ZR(ADESC + 2-1) = B
            ZR(ADESC + 3-1) = C
            ZR(ADESC + 4-1) = D
            ZR(ADESC + 5-1) = E
            ZR(ADESC + 6-1) = F
            D = D - A
            E = E - B
            F = F - C
            L = SQRT(D*D+E*E+F*F)
            DO 210, IOC = 1, NBOC, 1
               IDEB = ZI(ACNXO + IOC-1)
               IFIN = ZI(ACNXE + IOC-1)
               NBPT = IFIN - IDEB + 2
               CALL JECROC(JEXNUM(NABSC,IOC))
               CALL JEECRA(JEXNUM(NABSC,IOC),'LONMAX',NBPT,' ')
               CALL JEVEUO(JEXNUM(NABSC,IOC),'E',AABSC)
               CALL JECROC(JEXNUM(NCOOR,IOC))
               CALL JEECRA(JEXNUM(NCOOR,IOC),'LONMAX',3*NBPT,' ')
               CALL JEVEUO(JEXNUM(NCOOR,IOC),'E',ACOOR)
               DO 20, IPT = 1, NBPT-1, 1
                  S = ZR(AOSGT + IDEB-1 + IPT-1)
                  ZR(AABSC + IPT-1)           = S*L
                  ZR(ACOOR + 3*(IPT-1) + 1-1) = A + S*D
                  ZR(ACOOR + 3*(IPT-1) + 2-1) = B + S*E
                  ZR(ACOOR + 3*(IPT-1) + 3-1) = C + S*F
20             CONTINUE
               S = ZR(AESGT + IFIN-1)
               ZR(AABSC + NBPT-1) = S*L
               ZR(ACOOR + 3*(IPT-1) + 1-1) = A + S*D
               ZR(ACOOR + 3*(IPT-1) + 2-1) = B + S*E
               ZR(ACOOR + 3*(IPT-1) + 3-1) = C + S*F
210         CONTINUE
         ELSE
            IF ( OKCRB ) THEN
               ZK8(AREFE) = COURBE
               DOCU       = 'CHMM'
               CALL JEVEUO(JEXNUM(COURBE//'.CHEMIN',ISD),'L',ACHM)
               CALL JELIRA(JEXNUM(COURBE//'.CHEMIN',ISD),
     &                    'LONMAX',NBM,K1BID)
               NBM = NBM - 1
               CALL RVNCHM(MAILLA,ZI(ACHM),NBM,LNUMND,NDESC)
               CALL JELIRA(NDESC,'LONMAX',NBPT,K1BID)
               CALL JEVEUO(LNUMND,'L',ANUMND)
            ELSE
               ZK8(AREFE) =  MAILLA
               DOCU       = 'LSTN'
               CALL JELIRA(NLSNAC,'LONMAX',NBPT,K1BID)
               CALL JEVEUO(NLSNAC,'L',ANUMND)
               CALL WKVECT(NDESC,'V V K8',NBPT,ADESC)
               DO 30, IPT = 1, NBPT, 1
                  CALL JENUNO(JEXNUM(MAILLA//'.NOMNOE',ZI(ANUMND+IPT-1))
     &                        ,ZK8(ADESC + IPT-1))
30             CONTINUE
            ENDIF
            CALL JECREC(NABSC,'V V R','NU','DISPERSE','VARIABLE',1)
            CALL JECROC(JEXNUM(NABSC,1))
            CALL JEECRA(JEXNUM(NABSC,1),'LONMAX',NBPT,' ')
            CALL JEVEUO(JEXNUM(NABSC,1),'E',AABSC)
            CALL JECREC(NCOOR,'V V R','NU','DISPERSE','VARIABLE',1)
            CALL JECROC(JEXNUM(NCOOR,1))
            CALL JEECRA(JEXNUM(NCOOR,1),'LONMAX',3*NBPT,' ')
            CALL JEVEUO(JEXNUM(NCOOR,1),'E',ACOOR)
            CALL RVABSC(MAILLA,ZI(ANUMND),NBPT,ZR(AABSC),ZR(ACOOR))
            CALL JEEXIN(LNUMND,ADR)
            IF ( ADR .NE. 0 ) THEN
               CALL JEDETR(LNUMND)
            ENDIF
         ENDIF
         CALL JEECRA(NREFE,'DOCU',ADR,DOCU)
100   CONTINUE
      CALL JEDEMA()
      END
