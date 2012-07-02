      SUBROUTINE DELAT(MODGEN,NBSST,NBMO)
      IMPLICIT NONE

C---------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C---------------------------------------------------------------------
C AUTEUR : G. ROUSSEAU
C
      INCLUDE 'jeveux.h'
      INTEGER       IBID,NBID,ISST
      CHARACTER*8   K8BID
      CHARACTER*8   MODGEN,MACEL
      COMPLEX*16    CBID
C -----------------------------------------------------------------
C---------------------------------------------------------------------

C-----------------------------------------------------------------------
      INTEGER IBAMO ,ICOMPT ,IDELAT ,IJ ,IMACL ,JPARA ,NBMO 
      INTEGER NBMODG ,NBSST ,NBTYPE 
      REAL*8 BID ,EBID 
C-----------------------------------------------------------------------
      CALL JEMARQ()

C NB DE MODES TOTAL

       NBMO=0
       DO 1 ISST=1,NBSST
        CALL JEVEUO(JEXNUM(MODGEN//'      .MODG.SSME',ISST),
     +            'L',IMACL)
        MACEL=ZK8(IMACL)
        CALL JEVEUO(MACEL//'.MAEL_REFE','L',IBAMO)
        CALL RSORAC(ZK24(IBAMO),'LONUTI',IBID,BID,K8BID,CBID,EBID,
     +              'ABSOLU',
     +             NBMODG,1,NBID)
        NBMO=NBMO+NBMODG
1      CONTINUE

C TABLEAU INDIQUANT LES MODES PROPRES

       CALL WKVECT('&&DELAT.INDIC','V V I',NBMO,IDELAT)
       ICOMPT=0
       DO 2 ISST=1,NBSST
        CALL JEVEUO(JEXNUM(MODGEN//'      .MODG.SSME',ISST),
     +            'L',IMACL)
        MACEL=ZK8(IMACL)

        CALL JEVEUO(MACEL//'.MAEL_REFE','L',IBAMO)

C       CALL JEVEUO(ZK24(IBAMO)(1:19)//'.TYPE','L',ITYPE)
        CALL JELIRA(ZK24(IBAMO)(1:19)//'.ORDR'
     +           ,'LONUTI',NBTYPE,K8BID)
         DO 3, IJ=1,NBTYPE
            ICOMPT=ICOMPT+1
            CALL RSADPA(ZK24(IBAMO)(1:19),'L',1,'TYPE_DEFO',IJ,
     &                   0,JPARA,K8BID)
            IF(ZK16(JPARA)(1:8).NE.'PROPRE  ') GOTO 3
            ZI(IDELAT+ICOMPT-1)=1
3       CONTINUE
2      CONTINUE
      CALL JEDEMA()
       END
