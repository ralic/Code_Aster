      SUBROUTINE IREDSU (MACR,FORM,IFC,VERSIO)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER IFC,VERSIO
      CHARACTER*(*) MACR,FORM
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 25/06/2012   AUTEUR ABBAS M.ABBAS 
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
C
C     BUT:
C       IMPRESSION D'UN CONCEPT MACR_ELEM_DYNA AU FORMAT "IDEAS"
C       ATTENTION: le dataset 481 est en minuscules.
C
C
C     ARGUMENTS:
C     ----------
C
C      ENTREE :
C-------------
C IN   MACR      : NOM DU CONCEPT MACR_ELEM_DYNA
C IN   FORM      : FORMAT D'ECRITURE
C IN   IFC       : UNITE LOGIQUE D'ECRITURE
C IN   VERSIO    : VERSION D'IMPRESSION
C
C      SORTIE :
C-------------
C
C ......................................................................
C
C
C
C
      INTEGER NIVE
      INTEGER I, IBID, ICOL, IDRX, IDRY, IDRZ, IDX, IDY, IDZ
      INTEGER IE, IERO, IFOR, IM, IMAT
      INTEGER IN, IND, INOE, INOEU, IORD, IRET, IS, IS2, ITYP, I2
      INTEGER J, K, M2, NBORDR, NSTAT
      INTEGER JMASG, JMASJ, JMST, JORDR, JNOEU, JPARS, JPARI
      INTEGER JREFE, JRIGJ, JRIGG
      INTEGER KNOEU, KMASS, KRIGI
      INTEGER NBNOEU, NBMODT, NBMODE, NBMODS

      REAL*8 ZERO

      CHARACTER*1  B, CECR
      CHARACTER*8  K8B, MACREL, NOMA, NOEU, CMP, FORMAR
      CHARACTER*16 NOMSYM
      CHARACTER*19 BASEMO, NOCH19
      CHARACTER*24 MANONO
      CHARACTER*80 TITRE

      LOGICAL      F,LBID
C
C-----------------------------------------------------------------------
C
      CALL JEMARQ ( )

      ZERO = 0.D0
      IERO = 0
      CECR = 'L'
      B    = ' '
      F    = .FALSE.
      MACREL = MACR
      FORMAR = '1PE12.5'
      NIVE   = 3
C
      CALL JEVEUO(MACREL//'.MAEL_REFE','L',JREFE)
      BASEMO = ZK24(JREFE)
      NOMA   = ZK24(JREFE+1)
      MANONO = NOMA//'.NOMNOE'
      CALL DISMOI('F','NB_NO_MAILLA',NOMA,'MAILLAGE',NBNOEU,K8B,IE)
      CALL RSLIPA(BASEMO,'NOEUD_CMP','&&IREDSU.LINOEU',JNOEU,NBMODT)

      DO 10 IM = 1 , NBMODT
         IF ( ZK16(JNOEU+IM-1) .NE. ' ' ) GOTO 12
 10   CONTINUE
 12   CONTINUE
      NBMODE = IM - 1
      NBMODS = NBMODT - NBMODE
C
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C                    --- IMPRESSION DES DDL DE JONCTION ---
C
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      IF ( NBMODS .NE. 0 ) THEN
        CALL WKVECT('&&IREDSU.NOEUDS','V V K8',NBNOEU,KNOEU)
        INOEU = 1
        ZK8(KNOEU) = ZK16(JNOEU+NBMODE)
        DO 20 IM = 2 , NBMODS
           NOEU = ZK16(JNOEU+NBMODE+IM-1)
           DO 22 J = 1 , INOEU
              IF ( NOEU .EQ. ZK8(KNOEU+J-1) ) GOTO 20
 22        CONTINUE
           INOEU = INOEU + 1
           ZK8(KNOEU+INOEU-1) = NOEU
 20     CONTINUE
        IF ( VERSIO .EQ. 5 ) THEN
          WRITE (IFC,'(A)') '    -1'
          WRITE (IFC,'(A)') '   481'
          WRITE (IFC,'(I10)') 1
          WRITE (IFC,'(40A2)') 'Ju', 'nc'
          WRITE (IFC,'(A)') '    -1'
          IND  = 1
          ICOL = 7
          WRITE (IFC,'(A)') '    -1'
          WRITE (IFC,'(A)') '   757'
          WRITE (IFC,'(2I10)') IND
          WRITE (IFC,'(A)') 'DDL JONCTION'
          DO 24 IN = 1 , INOEU
            NOEU = ZK8(KNOEU+IN-1)
            CALL JENONU(JEXNOM(MANONO,NOEU),INOE)
            IDX  = 0
            IDY  = 0
            IDZ  = 0
            IDRX = 0
            IDRY = 0
            IDRZ = 0
            DO 26 IM = 1 , NBMODS
              IF ( NOEU .EQ. ZK16(JNOEU+NBMODE+IM-1)(1:8) ) THEN
                 CMP = ZK16(JNOEU+NBMODE+IM-1)(9:16)
                 IF (     CMP .EQ. 'DX      ' ) THEN
                    IDX = 1
                 ELSEIF ( CMP .EQ. 'DY      ' ) THEN
                    IDY = 1
                 ELSEIF ( CMP .EQ. 'DZ      ' ) THEN
                    IDZ = 1
                 ELSEIF ( CMP .EQ. 'DRX     ' ) THEN
                    IDRX = 1
                 ELSEIF ( CMP .EQ. 'DRY     ' ) THEN
                    IDRY = 1
                 ELSEIF ( CMP .EQ. 'DRZ     ' ) THEN
                    IDRZ = 1
                 ENDIF
              ENDIF
 26         CONTINUE
            WRITE (IFC,'(2I10,6I2)')
     >             INOE, ICOL, IDX, IDY, IDZ, IDRX, IDRY, IDRZ
 24       CONTINUE
          WRITE (IFC,'(A)') '    -1'
        ENDIF
      ENDIF
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C                 --- IMPRESSION DES MODES DYNAMIQUES ---
C                 --- IMPRESSION DES MODES STATIQUES ---
C
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      NOMSYM = 'DEPL'
      CALL JEVEUO(BASEMO//'.ORDR','L',JORDR)
      CALL JELIRA(BASEMO//'.ORDR','LONMAX',NBORDR,K8B)
      CALL WKVECT('&&IREDSU.MODE_STAT','V V K24',NBORDR,JMST)
      NSTAT = 0
      DO 100 I = 1 , NBORDR
        IORD = ZI(JORDR+I-1)
        IF (ZK16(JNOEU+I-1).NE.' ') THEN
          CALL RSEXCH ( BASEMO,'DEPL',IORD,NOCH19,IRET)
          IF ( IRET .EQ. 0 ) THEN
            NSTAT = NSTAT + 1
            ZK24(JMST+NSTAT-1) = NOCH19
          ENDIF
        ELSE
          WRITE (IFC,'(A)') '    -1'
          WRITE (IFC,'(A)') '   481'
          WRITE (IFC,'(I10)') 1
          WRITE (IFC,'(40A2)') 'Ph', 'i_', 'a '
          WRITE (IFC,'(A)') '    -1'
          TITRE = 'MODE DYNAMIQUE'
          CALL IRECRI ( BASEMO,FORM,IFC,TITRE,LBID,
     >                 1,'DEPL',' ',IERO,K8B, 
     &                 1,IORD,.TRUE.,B,IERO,
     >                 CECR,K8B,F,IERO,IBID,
     >                 IERO,IBID,IERO,K8B,F,
     >                 ZERO,F,ZERO,F,F,
     >                 FORMAR,NIVE,VERSIO)
        ENDIF
 100  CONTINUE
      IF ( NSTAT .NE. 0 ) THEN
         WRITE (IFC,'(A)') '    -1'
         WRITE (IFC,'(A)') '   481'
         WRITE (IFC,'(I10)') 1
         WRITE (IFC,'(40A2)') 'Ps', 'i_', 'a '
         WRITE (IFC,'(A)') '    -1'
         CALL IRMAD0 ( IFC, VERSIO, NSTAT, ZK24(JMST), NOMSYM )
      ENDIF
      CALL JEDETR ( '&&IREDSU.MODE_STAT' )
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C                 --- IMPRESSION DES MATRICES MODALES ---
C
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
      CALL WKVECT('&&IREDSU.MASS_GENE','V V R',NBMODE*NBMODE,JMASG)
      CALL WKVECT('&&IREDSU.RIGI_GENE','V V R',NBMODE*NBMODE,JRIGG)
      IF ( NBMODS .NE. 0 ) THEN
        CALL WKVECT('&&IREDSU.MASS_JONC','V V R',NBMODS*NBMODS,JMASJ)
        CALL WKVECT('&&IREDSU.RIGI_JONC','V V R',NBMODS*NBMODS,JRIGJ)
        CALL WKVECT('&&IREDSU.PART_SUPE','V V R',NBMODE*NBMODS,JPARS)
        CALL WKVECT('&&IREDSU.PART_INFE','V V R',NBMODE*NBMODS,JPARI)
      ENDIF
C
      CALL JEVEUO(MACREL//'.MAEL_MASS_VALE','L',KMASS)
      CALL JEVEUO(MACREL//'.MAEL_RAID_VALE','L',KRIGI)
      DO 200 IM = 1 , NBMODE
      DO 200 I = 1,IM
         K =IM*(IM-1)/2 + I
         ZR(JMASG+I-1+(IM-1)*NBMODE) = ZR(KMASS+K-1)
         ZR(JMASG+IM-1+(I-1)*NBMODE) = ZR(KMASS+K-1)
         ZR(JRIGG+I-1+(IM-1)*NBMODE) = ZR(KRIGI+K-1)
         ZR(JRIGG+IM-1+(I-1)*NBMODE) = ZR(KRIGI+K-1)
 200  CONTINUE
      DO 210 IS = NBMODE+1,NBMODT
         DO 220 IM = 1,NBMODE
           K = IS*(IS-1)/2 + IM
           IS2 = IS - NBMODE
           ZR(JPARS+IS2-1+(IM-1)*NBMODS) = ZR(KMASS+K-1)
           ZR(JPARI+IS2-1+(IM-1)*NBMODS) = ZR(KMASS+K-1)
 220     CONTINUE
         DO 230 I = NBMODE+1,IS
           K = IS*(IS-1)/2 + I
           I2 = I - NBMODE
           IS2 = IS - NBMODE
           ZR(JMASJ+I2-1+(IS2-1)*NBMODS) = ZR(KMASS+K-1)
           ZR(JMASJ+IS2-1+(I2-1)*NBMODS) = ZR(KMASS+K-1)
           ZR(JRIGJ+I2-1+(IS2-1)*NBMODS) = ZR(KRIGI+K-1)
           ZR(JRIGJ+IS2-1+(I2-1)*NBMODS) = ZR(KRIGI+K-1)
 230     CONTINUE
 210  CONTINUE
C
      M2 = NBMODE * NBMODE
      IF ( VERSIO .EQ. 5 ) THEN
         ITYP = 4
         IFOR = 1
         ICOL = 2
C
C        --- MASSE GENERALISEE ---
         WRITE (IFC,'(A)') '    -1'
         WRITE (IFC,'(A)') '   481'
         WRITE (IFC,'(I10)') 1
         WRITE (IFC,'(40A2)') 'Mg', 'en', '_a'
         WRITE (IFC,'(A)') '    -1'
         IMAT = 131
         WRITE (IFC,'(A)') '    -1'
         WRITE (IFC,'(A)') '   252'
         WRITE (IFC,'(I10)') IMAT
         WRITE (IFC,'(5I10)') ITYP, IFOR, NBMODE, NBMODE, ICOL
         WRITE (IFC,1000) (ZR(JMASG+I) , I= 0, M2-1 )
         WRITE (IFC,'(A)') '    -1'
C
C        --- RAIDEUR GENERALISEE ---
         WRITE (IFC,'(A)') '    -1'
         WRITE (IFC,'(A)') '   481'
         WRITE (IFC,'(I10)') 1
         WRITE (IFC,'(40A2)') 'Kg', 'en', '_a'
         WRITE (IFC,'(A)') '    -1'
         IMAT = 139
         WRITE (IFC,'(A)') '    -1'
         WRITE (IFC,'(A)') '   252'
         WRITE (IFC,'(I10)') IMAT
         WRITE (IFC,'(5I10)') ITYP, IFOR, NBMODE, NBMODE, ICOL
         WRITE (IFC,1000) (ZR(JRIGG+I) , I= 0, M2-1 )
         WRITE (IFC,'(A)') '    -1'
C
         IF ( NBMODS .NE. 0 ) THEN
           M2 = NBMODS * NBMODS
C
C          --- MASSE CONDENSEE A LA JONCTION ---
           WRITE (IFC,'(A)') '    -1'
           WRITE (IFC,'(A)') '   481'
           WRITE (IFC,'(I10)') 1
           WRITE (IFC,'(40A2)') 'Mb', 'ar', '_a'
           WRITE (IFC,'(A)') '    -1'
           IMAT = 134
           WRITE (IFC,'(A)') '    -1'
           WRITE (IFC,'(A)') '   252'
           WRITE (IFC,'(I10)') IMAT
           WRITE (IFC,'(5I10)') ITYP, IFOR, NBMODS, NBMODS, ICOL
           WRITE (IFC,1000) (ZR(JMASJ+I) , I= 0, M2-1 )
           WRITE (IFC,'(A)') '    -1'
C
C          --- RIGIDITE CONDENSEE A LA JONCTION ---
           WRITE (IFC,'(A)') '    -1'
           WRITE (IFC,'(A)') '   481'
           WRITE (IFC,'(I10)') 1
           WRITE (IFC,'(40A2)') 'Kb', 'ar', '_a'
           WRITE (IFC,'(A)') '    -1'
           IMAT = 142
           WRITE (IFC,'(A)') '    -1'
           WRITE (IFC,'(A)') '   252'
           WRITE (IFC,'(I10)') IMAT
           WRITE (IFC,'(5I10)') ITYP, IFOR, NBMODS, NBMODS, ICOL
           WRITE (IFC,1000) (ZR(JRIGJ+I) , I= 0, M2-1 )
           WRITE (IFC,'(A)') '    -1'
C
           M2 = NBMODE * NBMODS
C
C          --- FACTEUR DE PARTICIPATION INFERIEUR ---
           WRITE (IFC,'(A)') '    -1'
           WRITE (IFC,'(A)') '   481'
           WRITE (IFC,'(I10)') 1
           WRITE (IFC,'(40A2)') 'Lm', 'at', '_a'
           WRITE (IFC,'(A)') '    -1'
           IMAT = 132
           WRITE (IFC,'(A)') '    -1'
           WRITE (IFC,'(A)') '   252'
           WRITE (IFC,'(I10)') IMAT
           WRITE (IFC,'(5I10)') ITYP, IFOR, NBMODE, NBMODS, ICOL
           WRITE (IFC,1000) (ZR(JPARI+I) , I= 0, M2-1 )
           WRITE (IFC,'(A)') '    -1'
C
         ENDIF
      ENDIF
C
 1000 FORMAT( 1P, 4D20.12 )
      CALL JEDETC ( 'V' , '&&IREDSU' , 1 )
C
      CALL JEDEMA ( )
      END
