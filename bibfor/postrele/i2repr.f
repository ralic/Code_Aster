      SUBROUTINE I2REPR (CONEC,TYPE,MAILLE,CHEMIN,PTCHM,NBCHM,M1,M2)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 14/01/98   AUTEUR VABHHTS J.PELLET 
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
C**********************************************************************
C
C     RECHERCHE DES MAILLES SURFACIQUES SUR LESQUELLES S' APPUIT
C     UN ENSEMBLE DE MAILLES SURFACIQUES
C
C       CONEC  (IN)  : NOM DE L' OBJET CONNECTIVITE DU MAILLAGE
C
C       TYPE   (IN)  : NOM DE L' OBJET CONTENANT LE TYPE DES MAILLES
C
C       MAILLE (IN)  : TABLE DES MAILLES DE L' ENSEMBLE TRAITE
C
C       CHEMIN (IN)  : TABLE DES CHEMINS DE L' ENSEMBLE
C
C       PTCHM  (IN)  : TABLE D' ACCES A CHEMIN
C
C       NBCHM  (IN)  : NBR DE CHEMINS
C
C       M1     (OUT) : TABLE DES PREMIERES MAILLES D' APPUI
C
C       M2     (OUT) : TABLE DES SECONDES MAILLES D' APPUI
C
C**********************************************************************
C
      CHARACTER*24 TYPE, CONEC
      INTEGER      MAILLE(*),CHEMIN(*),PTCHM(*),NBCHM,M1(*),M2(*)
C
      INTEGER      NBM,NBC,NBS,NBN
      INTEGER      NCD,NCG,NSD,NDG
      INTEGER      M,C,S,DEBCHM,FINCHM,SGTEFF,SFINI
      INTEGER      ATYPM,CHM,AFINI,I,ADRS,ANSG,ANSD
      CHARACTER*8  TYPM
C
      CHARACTER*32 JEXNUM
C
C------------COMMUNS NORMALISES JEVEUX-------------------------------
C
      INTEGER              ZI
      COMMON      /IVARJE/ ZI(1)
      REAL*8               ZR
      COMMON      /RVARJE/ ZR(1)
      COMPLEX*16           ZC
      COMMON      /CVARJE/ ZC(1)
      LOGICAL              ZL
      COMMON      /LVARJE/ ZL(1)
      CHARACTER*8          ZK8
      CHARACTER*16         ZK16
      CHARACTER*24         ZK24
      CHARACTER*32         ZK32
      CHARACTER*80         ZK80
      COMMON      /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*1 K1BID
C
C
C
      CALL JEMARQ()
      CALL JELIRA(CONEC(1:15),'NMAXOC',NBM,K1BID)
C
      NBS    = PTCHM(NBCHM+1) - 1
      SGTEFF = NBS - NBCHM
      SFINI  = 0
      M      = 0
      S      = 0
      C      = 0
      NBN    = 0
      NBC    = 0
      NBN    = 0
      NCD    = 0
      NCG    = 0
      NSD    = 0
      NSG    = 0
      DEBCHM = 0
      FINCHM = 0
      ATYPM  = 0
      CHM    = 0
      AFINI  = 0
      TYPM   = ' '
      ADRS   = 0
      ANSD   = 0
      ANSG   = 0
C
      CALL JECREO('&INTFINI','V V L')
      CALL JEECRA('&INTFINI','LONMAX',NBS,' ')
      CALL JEVEUO('&INTFINI','E',AFINI)
C
      CALL JECREO('&INTNSG','V V I')
      CALL JEECRA('&INTNSG','LONMAX',SGTEFF,' ')
      CALL JEVEUO('&INTNSG','E',ANSG)
C
      CALL JECREO('&INTNSD','V V I')
      CALL JEECRA('&INTNSD','LONMAX',SGTEFF,' ')
      CALL JEVEUO('&INTNSD','E',ANSD)
C
      DO 10, I = 1, NBS, 1
C
         ZL(AFINI + I-1) = .FALSE.
C
10    CONTINUE
C
      DO 20, I = 1, NBCHM
C
         ZL(AFINI + PTCHM(CHM+1)-2) = .TRUE.
C
20    CONTINUE
C
      DO 30, I = 1, SGTEFF, 1
C
         CALL JEVEUO (JEXNUM(CONEC(1:15),MAILLE(I)),'L',ADRS)
C
         ZI(ANSG + I-1) = ZI(ADRS)
         ZI(ANSD + I-1) = ZI(ADRS + 1)
C
30    CONTINUE
C
      DO 31, I = 1, SGTEFF, 1
C
C
31    CONTINUE
C
500   CONTINUE
      IF ( (SFINI .LT. SGTEFF) .AND. (M .LT. NBM) ) THEN
C
         M = M + 1
C
         CALL JEVEUO(TYPE,'L',IATYMA)
         ATYPM=IATYMA-1+M
         CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ZI(ATYPM)),TYPM)
C
         IF ( (TYPM .NE. 'POI1') .AND. (TYPM .NE. 'SEG2') .AND.
     +        (TYPM .NE. 'SEG3') ) THEN
C
            CALL JELIRA(JEXNUM(CONEC(1:15),M),'LONMAX',NBN,K1BID)
            CALL I2NBRF(NBN,NBC)
C
            DO 100, C = 1, NBC, 1
C
               CALL I2EXTF(M,C,CONEC(1:15),TYPE(1:16),NCG,NCD)
C
               DO 110 CHM = 1, NBCHM, 1
C
                  DEBCHM = PTCHM(CHM)
                  FINCHM = PTCHM(CHM+1) - 2
C
                  DO 120, S = DEBCHM, FINCHM, 1
C
                     IF ( .NOT. ZL(AFINI + S-1) ) THEN
C
C
                        NSG = ZI(ANSG + CHEMIN(S)-1)
                        NSD = ZI(ANSD + CHEMIN(S)-1)
C
                        IF ( ( (NCD .EQ. NSD) .AND. (NCG .EQ. NSG) )
     +                       .OR.
     +                       ( (NCD .EQ. NSG) .AND. (NCG .EQ. NSD) )
     +                     ) THEN
C
                           IF ( M1(CHEMIN(S)) .EQ. 0 ) THEN
C
                              M1(CHEMIN(S)) = M
C
                           ELSE
C
                              M2(CHEMIN(S)) = M
C
                              SFINI = SFINI + 1
C
                              ZL(AFINI + S-1) = .TRUE.
C
                           ENDIF
C
                        ENDIF
C
                     ENDIF
C
120               CONTINUE
C
110            CONTINUE
C
100         CONTINUE
C
         ENDIF
C
         GOTO 500
C
      ENDIF
C
      CALL JEDETR('&INTFINI')
      CALL JEDETR('&INTNSG')
      CALL JEDETR('&INTNSD')
C
      CALL JEDEMA()
      END
