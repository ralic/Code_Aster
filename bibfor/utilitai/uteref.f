      SUBROUTINE UTEREF ( CHANOM, TYPECH, TYELAS, NOMTE,
     &                    NOMFPG, NNOS, NNO, NBPG, NDIM,
     &                    REFCOO, GSCOO, WG,
     &                    CODRET )
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C RESPONSABLE GNICOLAS G.NICOLAS
C-----------------------------------------------------------------------
C     UTILITAIRE - ELEMENT DE REFERENCE
C     --           -          ---
C-----------------------------------------------------------------------
C     ENTREES :
C       CHANOM : NOM ASTER DU CHAMP
C       TYPECH : TYPE DU CHAMP ('ELGA')
C       TYELAS : TYPE D'ELEMENT ASTER
C       NOMTE  : NOM DE L'ELEMENT FINI A EXAMINER
C     SORTIES :
C       NOMFPG : NOM DE LA FAMILLE DES POINTS DE GAUSS
C       NNOS   : NOMBRE DE NOEUDS SOMMETS
C       NNO    : NOMBRE DE NOEUDS TOTAL
C       NBPG   : NOMBRE DE POINTS DE GAUSS
C       NDIM   : DIMENSION DE L'ELEMENT
C       REFCOO : COORDONNEES DES NNO NOEUDS
C       GSCOO  : COORDONNEES DES POINTS DE GAUSS, SI CHAMP ELGA
C       WG     : POIDS DES POINTS DE GAUSS, SI CHAMP ELGA
C       CODRET : CODE DE RETOUR
C                0 : PAS DE PB
C                1 : LE CHAMP N'EST PAS DEFINI SUR CE TYPE D'ELEMENT
C     REMARQUE :
C     ON DOIT RETOURNER LES COORDONNEES SOUS LA FORME :
C     . ELEMENT 1D : X1 X2 ... ... XN
C     . ELEMENT 2D : X1 Y1 X2 Y2 ... ... XN YN
C     . ELEMENT 3D : X1 Y1 Z1 X2 Y2 Z2 ... ... XN YN ZN
C     C'EST CE QUE MED APPELLE LE MODE ENTRELACE
C     ON DOIT RETOURNER LES POIDS SOUS LA FORME :
C     WG1 WG2 ... ... WGN
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
C
C 0.1. ==> ARGUMENTS
C
      INTEGER TYELAS
      INTEGER NNOS, NNO, NBPG, NDIM
C
      REAL*8 REFCOO(*), GSCOO(*), WG(*)
C
      CHARACTER*8 TYPECH
      CHARACTER*16 NOMTE
      CHARACTER*16 NOMFPG
      CHARACTER*19 CHANOM
C
      INTEGER CODRET

C 0.2. ==> COMMUNS
C --------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*32 JEXNUM
      INTEGER         ZI
      COMMON /IVARJE/ ZI(1)
      REAL*8          ZR
      COMMON /RVARJE/ ZR(1)
      COMPLEX*16      ZC
      COMMON /CVARJE/ ZC(1)
      LOGICAL         ZL
      COMMON /LVARJE/ ZL(1)
      CHARACTER*8     ZK8
      CHARACTER*16           ZK16
      CHARACTER*24                   ZK24
      CHARACTER*32                           ZK32
      CHARACTER*80                                   ZK80
      COMMON /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C --------------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------

C 0.3. ==> VARIABLES LOCALES
C
C
      INTEGER NBPG00(10),IMOLO,NEC,KFPG,NBFPG,IFAM,INDIK8
      INTEGER ITYPE,NB1,NBELR,JMOLO,IBID
      INTEGER IFM, NIVINF,JCELK,JCELD,IGREL
      INTEGER IERD,REPI,JLIEL,NBGREL
      INTEGER IAUX
C
      CHARACTER*4 TYCH
      CHARACTER*8 ELREFE,LIREFE(10),FAPG(10),NOMGD,FAMIL,KBID
      CHARACTER*19 LIGREL
C
      REAL*8 VOL
C-----------------------------------------------------------------------


C     1- PREALABLES
C     ---------------
      CALL JEMARQ()
      CODRET = 0

      CALL INFNIV ( IFM, NIVINF )
      IF ( NIVINF.GT.1 ) THEN
        CALL U2MESS('I','UTILITAI5_39')
        WRITE (IFM,10001) TYELAS, NOMTE
10001   FORMAT('ELEMENT FINI NUMERO',I6,', DE NOM : ',A16)
      ENDIF
      CALL ASSERT ( TYPECH.EQ.'ELGA' )
C
C     2- DETERMINATION DE ELREFE :
C     -----------------------------
C
      IF ( CODRET.EQ.0 ) THEN
C
      CALL ELREF2(NOMTE,10,LIREFE,NBELR)
      CALL ASSERT(NBELR.GT.0)
      ELREFE = LIREFE(1)


      CALL DISMOI('F','TYPE_CHAMP',CHANOM,'CHAMP',REPI,TYCH,IERD)
      CALL ASSERT(TYCH.EQ.TYPECH)
      CALL JEVEUO(CHANOM//'.CELK','L',JCELK)
      CALL JEVEUO(CHANOM//'.CELD','L',JCELD)
      LIGREL = ZK24(JCELK-1+1)(1:19)
      CALL ASSERT(ZK24(JCELK-1+3)(1:19).EQ.TYPECH)
      NBGREL = ZI(JCELD-1+2)
C
      ENDIF
C
C     3- DETERMINATION DU MODE_LOCAL (IMOLO) ASSOCIE AU CHAMP POUR
C        LE TYPE D'ELEMENT TYELAS :
C     -------------------------------------------------------------
C
      IF ( CODRET.EQ.0 ) THEN
C
      DO 31 ,IGREL=1,NBGREL
C
         CALL JEVEUO(JEXNUM(LIGREL//'.LIEL',IGREL),'L',JLIEL)
         CALL JELIRA(JEXNUM(LIGREL//'.LIEL',IGREL),'LONMAX',NB1,KBID)
         ITYPE = ZI(JLIEL-1+NB1)
C
         IF ( ITYPE.EQ.TYELAS ) THEN
            IMOLO = ZI(JCELD-1+ZI(JCELD-1+4+IGREL)+2)
            IF ( IMOLO.EQ.0) THEN
              CODRET = 1
              IF ( NIVINF.GT.1 ) THEN
                WRITE (IFM,*)
     &          '==> LE CHAMP N''EST PAS DEFINI SUR CE TYPE D''ELEMENT'
              ENDIF
            ENDIF
            GO TO 32
         END IF
C
   31 CONTINUE
C
      CALL ASSERT(.FALSE.)
C
   32 CONTINUE
C
      ENDIF
C
C     4- DETERMINATION DE LA FAMILLE DE POINTS DE GAUSS :
C     -------------------------------------------------------------
C
      IF ( CODRET.EQ.0 ) THEN
C
      CALL JEVEUO(JEXNUM('&CATA.TE.MODELOC',IMOLO),'L',JMOLO)
      CALL DISMOI('F','NOM_GD',CHANOM,'CHAMP',IBID,NOMGD,IERD)
      CALL DISMOI('F','NB_EC',NOMGD,'GRANDEUR',NEC,KBID,IERD)
      KFPG = ZI(JMOLO-1+4+NEC+1)
      CALL JENUNO(JEXNUM('&CATA.TM.NOFPG',KFPG),NOMFPG)
      CALL ASSERT(ELREFE.EQ.NOMFPG(1:8))
      FAMIL = NOMFPG(9:16)
C
      ENDIF
C
C     5- APPEL AUX ROUTINES ELRACA ET ELRAGA DE DESCRIPTION DES ELREFE:
C     ----------------------------------------------------------------
C
      IF ( CODRET.EQ.0 ) THEN
C
      CALL ELRACA(ELREFE,NDIM,NNO,NNOS,NBFPG,FAPG,NBPG00,REFCOO,VOL)
C
      CALL ASSERT(NBFPG.LE.20)
      CALL ASSERT(NNO.LE.27)
      IFAM = INDIK8(FAPG,FAMIL,1,NBFPG)
      CALL ASSERT(IFAM.GT.0)
C
      CALL ELRAGA(ELREFE,FAMIL,NDIM,NBPG,GSCOO,WG)
C
      CALL ASSERT(NBPG.LE.27)
C
      ENDIF
C
C     6- IMPRESSION EVENTUELLE SUR LE FICHIER DE MESSAGES
C     ----------------------------------------------------------------
C
      IF ( CODRET.EQ.0 ) THEN
C
      IF ( NIVINF.GT.1 ) THEN
C
        WRITE (IFM,61000) 'FAMILLE DE POINTS DE GAUSS', NOMFPG
        WRITE (IFM,61001) 'NOMBRE DE SOMMETS        ', NNOS
        WRITE (IFM,61001) 'NOMBRE DE NOEUDS         ', NNO
        WRITE (IFM,61001) 'NOMBRE DE POINTS DE GAUSS', NBPG
C
C     6.1. DIMENSION 1
C
        IF ( NDIM.EQ.1 ) THEN
C                            123456789012345
          WRITE (IFM,60001) 'NOEUDS         '
          DO 6011 , IAUX = 1 , NNO
            WRITE (IFM,60011) IAUX,REFCOO(IAUX)
 6011     CONTINUE
          WRITE (IFM,60021)
          WRITE (IFM,60001) 'POINTS DE GAUSS'
          DO 6021 , IAUX = 1 , NBPG
            WRITE (IFM,60011) IAUX,GSCOO(IAUX)
 6021     CONTINUE
          WRITE (IFM,60021)
C
C     6.2. DIMENSION 2
C
        ELSEIF ( NDIM.EQ.2 ) THEN
          WRITE (IFM,60002) 'NOEUDS         '
          DO 6012 , IAUX = 1 , NNO
            WRITE (IFM,60012) IAUX, REFCOO(NDIM*(IAUX-1)+1),
     &                              REFCOO(NDIM*(IAUX-1)+2)
 6012     CONTINUE
          WRITE (IFM,60022)
          WRITE (IFM,60002) 'POINTS DE GAUSS'
          DO 6022 , IAUX = 1 , NBPG
            WRITE (IFM,60012) IAUX, GSCOO(NDIM*(IAUX-1)+1),
     &                              GSCOO(NDIM*(IAUX-1)+2)
 6022     CONTINUE
          WRITE (IFM,60022)
C
C     6.3. DIMENSION 3
C
        ELSE
          WRITE (IFM,60003) 'NOEUDS         '
          DO 6013 , IAUX = 1 , NNO
            WRITE (IFM,60013) IAUX, REFCOO(NDIM*(IAUX-1)+1),
     &                              REFCOO(NDIM*(IAUX-1)+2),
     &                              REFCOO(NDIM*(IAUX-1)+3)
 6013     CONTINUE
          WRITE (IFM,60023)
          WRITE (IFM,60003) 'POINTS DE GAUSS'
          DO 6023 , IAUX = 1 , NBPG
            WRITE (IFM,60013) IAUX, GSCOO(NDIM*(IAUX-1)+1),
     &                              GSCOO(NDIM*(IAUX-1)+2),
     &                              GSCOO(NDIM*(IAUX-1)+3)
 6023     CONTINUE
          WRITE (IFM,60023)
        ENDIF
C
        WRITE (IFM,60004)
        DO 6024 , IAUX = 1 , NBPG
          WRITE (IFM,60011) IAUX, WG(IAUX)
 6024   CONTINUE
        WRITE (IFM,60021)
C
      ENDIF
C
      ENDIF
C
60001 FORMAT(
     &/,28('*'),
     &/,'*      COORDONNEES DES     *',
     &/,'*      ',A15        ,'     *',
     &/,28('*'),
     &/,'*  NUMERO  *       X       *',
     &/,28('*'))
60002 FORMAT(
     &/,44('*'),
     &/,'*       COORDONNEES DES ',A15        ,'    *',
     &/,44('*'),
     &/,'*  NUMERO  *       X       *       Y       *',
     &/,44('*'))
60003 FORMAT(
     &/,60('*'),
     &/,'*            COORDONNEES DES ',A15         ,
     &'               *',
     &/,60('*'),
     &/,'*  NUMERO  *       X       *       Y       *',
     &'       Z       *',
     &/,60('*'))
60004 FORMAT(
     &/,28('*'),
     &/,'*      POINTS DE GAUSS     *',
     &/,'*  NUMERO  *     POIDS     *',
     &/,28('*'))
60011 FORMAT('* ',I5,'    *',G11.5,'    *')
60012 FORMAT('* ',I5,2('    *',G11.5),'    *')
60013 FORMAT('* ',I5,3('    *',G11.5),'    *')
60021 FORMAT(28('*'))
60022 FORMAT(44('*'))
60023 FORMAT(60('*'))
61000 FORMAT(A,' : ',A)
61001 FORMAT(A,' : ',I4)
C
      CALL JEDEMA()
      END
