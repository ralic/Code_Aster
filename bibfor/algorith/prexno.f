      SUBROUTINE PREXNO ( CHAMP, IOC, NOMAX, CMPMAX, VALMAX,
     +                                NOMIN, CMPMIN, VALMIN,
     +                                NOAMAX, CMAMAX, VAAMAX,
     +                                NOAMIN, CMAMIN, VAAMIN )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      INTEGER             IOC
      REAL*8              VALMIN, VALMAX, VAAMIN, VAAMAX
      CHARACTER*8         NOMAX, NOMIN, CMPMAX, CMPMIN
      CHARACTER*8         NOAMAX, NOAMIN, CMAMAX, CMAMIN
      CHARACTER*(*)       CHAMP
C ----------------------------------------------------------------------
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
C     COMMANDE : POST_RELEVE_T
C                DETERMINE LES EXTREMA POUR UN CHAM_NO
C
C ----------------------------------------------------------------------
C
      INTEGER      JCNSK, JCNSD, JCNSV, JCNSL, JCNSC, NBNO, NCMP, NBN
      INTEGER      IBID, NBNOEU, INDIK8, IDNOEU, NBC, NBCMP, JCMP
      INTEGER      I100, I110, ICP, INO, INOMAX,INOMIN,INAMAX,INAMIN
      REAL*8       X, R8VIDE
      CHARACTER*8  K8B, NOCMP, MA
      CHARACTER*16 MOTCLE(4), TYPMCL(4)
      CHARACTER*19 CHAMS1
      CHARACTER*24 MESNOE
      INTEGER      IARG
C ---------------------------------------------------------------------
C
      MOTCLE(1) = 'GROUP_NO'
      MOTCLE(2) = 'NOEUD'
      MOTCLE(3) = 'GROUP_MA'
      MOTCLE(4) = 'MAILLE'
      TYPMCL(1) = 'GROUP_NO'
      TYPMCL(2) = 'NOEUD'
      TYPMCL(3) = 'GROUP_MA'
      TYPMCL(4) = 'MAILLE'
      MESNOE = '&&PREXNO.MES_NOEUDS'
C
      CHAMS1 = '&&PREXNO.CHAMS1'
      CALL CNOCNS ( CHAMP,'V', CHAMS1 )
C
      CALL JEVEUO ( CHAMS1//'.CNSK', 'L', JCNSK )
      CALL JEVEUO ( CHAMS1//'.CNSD', 'L', JCNSD )
      CALL JEVEUO ( CHAMS1//'.CNSC', 'L', JCNSC )
      CALL JEVEUO ( CHAMS1//'.CNSV', 'L', JCNSV )
      CALL JEVEUO ( CHAMS1//'.CNSL', 'L', JCNSL )
      MA    = ZK8(JCNSK-1+1)
      NBNO  =  ZI(JCNSD-1+1)
      NCMP  =  ZI(JCNSD-1+2)
C
      CALL RELIEM(' ',MA,'NU_NOEUD','ACTION',IOC,4,MOTCLE,TYPMCL,
     +                                                   MESNOE,NBN)
      IF (NBN.GT.0) THEN
        NBNOEU = NBN
        CALL JEVEUO ( MESNOE, 'L', IDNOEU )
      ELSE
        NBNOEU = NBNO
      ENDIF
C
      CALL GETVTX ( 'ACTION', 'NOM_CMP', IOC,IARG,0,K8B, NBC )
      IF (NBC.NE.0) THEN
         NBCMP = -NBC
         CALL WKVECT('&&PREXNO.NOM_CMP','V V K8',NBCMP,JCMP)
         CALL GETVTX('ACTION','NOM_CMP',IOC,IARG,NBCMP,ZK8(JCMP),IBID)
      ELSE
         NBCMP = NCMP
      ENDIF
C
      INOMAX = 0
      VALMAX = -R8VIDE()
      INOMIN = 0
      VALMIN =  R8VIDE()
C
      INAMAX = 0
      VAAMAX = -R8VIDE()
      INAMIN = 0
      VAAMIN =  R8VIDE()
C
      DO 100 I100 = 1, NBCMP
         IF (NBC.NE.0) THEN
            NOCMP = ZK8(JCMP+I100-1)
            ICP = INDIK8( ZK8(JCNSC), NOCMP, 1, NCMP )
            IF (ICP.EQ.0) GOTO 100
         ELSE
            ICP = I100
            NOCMP = ZK8(JCNSC+I100-1)
         ENDIF
C
         DO 110 I110 = 1 , NBNOEU
            IF (NBN.GT.0) THEN
               INO = ZI(IDNOEU+I110-1)
            ELSE
               INO = I110
            ENDIF
C
            IF ( ZL(JCNSL-1+(INO-1)*NCMP+ICP) ) THEN
C
               X = ZR(JCNSV-1+(INO-1)*NCMP+ICP)
C
               IF ( X .GT. VALMAX ) THEN
                  INOMAX = INO
                  VALMAX = X
                  CMPMAX = NOCMP
               ENDIF
C
               IF ( ABS(X) .GT. VAAMAX ) THEN
                  INAMAX = INO
                  VAAMAX = ABS(X)
                  CMAMAX = NOCMP
               ENDIF
C
               IF ( X .LT. VALMIN ) THEN
                  INOMIN = INO
                  VALMIN = X
                  CMPMIN = NOCMP
               ENDIF
C
               IF ( ABS(X) .LT. VAAMIN ) THEN
                  INAMIN = INO
                  VAAMIN = ABS(X)
                  CMAMIN = NOCMP
               ENDIF
C
            ENDIF
C
 110     CONTINUE
C
 100  CONTINUE
C
      CALL JENUNO(JEXNUM(MA//'.NOMNOE',INOMAX),NOMAX)
      CALL JENUNO(JEXNUM(MA//'.NOMNOE',INOMIN),NOMIN)
      CALL JENUNO(JEXNUM(MA//'.NOMNOE',INAMAX),NOAMAX)
      CALL JENUNO(JEXNUM(MA//'.NOMNOE',INAMIN),NOAMIN)
C
      CALL DETRSD ( 'CHAM_NO_S', CHAMS1 )
      CALL JEDETC ( 'V','&&PREXNO',1 )
C
      END
