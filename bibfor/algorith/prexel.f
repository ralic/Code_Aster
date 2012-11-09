      SUBROUTINE PREXEL ( CHAMP, IOC,
     &                    MAMAX, NOMAX, ISPMAX, CMPMAX, VALMAX,
     &                    MAMIN, NOMIN, ISPMIN, CMPMIN, VALMIN,
     &                    MAAMAX, NOAMAX, ISAMAX, CMAMAX, VAAMAX,
     &                    MAAMIN, NOAMIN, ISAMIN, CMAMIN, VAAMIN )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      INTEGER             IOC, ISPMAX, ISPMIN, ISAMAX, ISAMIN
      REAL*8              VALMIN, VALMAX, VAAMIN, VAAMAX
      CHARACTER*8         MAMAX, NOMAX, CMPMAX, MAMIN, NOMIN, CMPMIN
      CHARACTER*8         MAAMAX, NOAMAX, CMAMAX, MAAMIN, NOAMIN, CMAMIN
      CHARACTER*(*)       CHAMP
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C TOLE CRP_21
C
C     COMMANDE : POST_RELEVE_T
C                DETERMINE LES EXTREMA POUR UN CHAM_ELNO
C
C ----------------------------------------------------------------------
C
      INTEGER      JCESK, JCESD, JCESV, JCESL, JCESC, NBMA, NCMP, NBM
      INTEGER      IBID, NBMAIL, INDIK8, IDMAIL, NBC, NBCMP, JCMP
      INTEGER      I100, I110, ICP, IMAI, NBPT, NBSP, IPT, ISP, IAD
      INTEGER      IMAMAX, IPTMAX, IMAMIN, IPTMIN, JCONE
      INTEGER      IMAAAX, IPAMAX, IMAAIN, IPAMIN, IER1,IER2
      REAL*8       X, R8VIDE
      CHARACTER*8  K8B, NOCMP, MA
      CHARACTER*16 MOTCLE(2), TYPMCL(2)
      CHARACTER*19 CHAMS1
      CHARACTER*24 MESMAI
      INTEGER      IARG
C ---------------------------------------------------------------------
C
      MOTCLE(1) = 'GROUP_MA'
      TYPMCL(1) = 'GROUP_MA'
      MOTCLE(2) = 'MAILLE'
      TYPMCL(2) = 'MAILLE'
      MESMAI = '&&PREXEL.MES_MAILLES'
C
      CHAMS1 = '&&PREXEL.CHAMS1'
      CALL CELCES ( CHAMP, 'V', CHAMS1 )
C
      CALL JEVEUO ( CHAMS1//'.CESK', 'L', JCESK )
      CALL JEVEUO ( CHAMS1//'.CESD', 'L', JCESD )
      CALL JEVEUO ( CHAMS1//'.CESV', 'L', JCESV )
      CALL JEVEUO ( CHAMS1//'.CESL', 'L', JCESL )
      CALL JEVEUO ( CHAMS1//'.CESC', 'L', JCESC )
      MA    = ZK8(JCESK-1+1)
      NBMA  =  ZI(JCESD-1+1)
      NCMP  =  ZI(JCESD-1+2)
C
      CALL RELIEM(' ',MA,'NU_MAILLE','ACTION',IOC,2,MOTCLE,TYPMCL,
     +                                                   MESMAI,NBM)
      IF (NBM.GT.0) THEN
        NBMAIL = NBM
        CALL JEVEUO ( MESMAI, 'L', IDMAIL )
      ELSE
        NBMAIL = NBMA
      ENDIF
C
      CALL GETVTX ( 'ACTION', 'NOEUD', IOC,IARG,0,K8B, IER1 )
      CALL GETVTX ( 'ACTION', 'GROUP_NO', IOC,IARG,0,K8B, IER2 )
      IF(IER1.NE.0.OR.IER2.NE.0) CALL U2MESS('F','POSTRELE_66')

      CALL GETVTX ( 'ACTION', 'NOM_CMP', IOC,IARG,0,K8B, NBC )
      IF (NBC.NE.0) THEN
         NBCMP = -NBC
         CALL WKVECT('&&PREXEL.NOM_CMP','V V K8',NBCMP,JCMP)
         CALL GETVTX('ACTION','NOM_CMP',IOC,IARG,NBCMP,ZK8(JCMP),IBID)
      ELSE
         NBCMP = NCMP
      ENDIF
C
      IMAMAX = 0
      IPTMAX = 0
      ISPMAX = 0
      VALMAX = -R8VIDE()
      IMAMIN = 0
      IPTMIN = 0
      ISPMIN = 0
      VALMIN =  R8VIDE()
C
      IMAAAX = 0
      IPAMAX = 0
      ISAMAX = 0
      VAAMAX = -R8VIDE()
      IMAAIN = 0
      IPAMIN = 0
      ISAMIN = 0
      VAAMIN =  R8VIDE()
C
      DO 100 I100 = 1 , NBCMP
         IF (NBC.NE.0) THEN
            NOCMP = ZK8(JCMP+I100-1)
            ICP = INDIK8( ZK8(JCESC), NOCMP, 1, NCMP )
            IF (ICP.EQ.0) GOTO 100
         ELSE
            ICP = I100
            NOCMP = ZK8(JCESC+I100-1)
         ENDIF
C
         DO 110 I110 = 1 , NBMAIL
            IF (NBM.NE.0) THEN
               IMAI = ZI(IDMAIL+I110-1)
            ELSE
               IMAI = I110
            ENDIF
            NBPT = ZI(JCESD-1+5+4*(IMAI-1)+1)
            NBSP = ZI(JCESD-1+5+4*(IMAI-1)+2)
            CALL JEVEUO(JEXNUM(MA//'.CONNEX',IMAI), 'L', JCONE )
            DO 120,IPT = 1,NBPT
               DO 130,ISP = 1,NBSP
                  CALL CESEXI('C',JCESD,JCESL,IMAI,IPT,ISP,ICP,IAD)
                  IF (IAD.GT.0) THEN
                     X = ZR(JCESV-1+IAD)
                     IF ( X .GT. VALMAX ) THEN
                        IMAMAX = IMAI
                        IPTMAX = ZI(JCONE+IPT-1)
                        ISPMAX = ISP
                        CMPMAX = NOCMP
                        VALMAX = X
                     ENDIF
                     IF ( ABS(X) .GT. VAAMAX ) THEN
                        IMAAAX = IMAI
                        IPAMAX = ZI(JCONE+IPT-1)
                        ISAMAX = ISP
                        CMAMAX = NOCMP
                        VAAMAX = ABS(X)
                     ENDIF
                     IF ( X .LT. VALMIN ) THEN
                        IMAMIN = IMAI
                        IPTMIN = ZI(JCONE+IPT-1)
                        ISPMIN = ISP
                        CMPMIN = NOCMP
                        VALMIN = X
                     ENDIF
                     IF ( ABS(X) .LT. VAAMIN ) THEN
                        IMAAIN = IMAI
                        IPAMIN = ZI(JCONE+IPT-1)
                        ISAMIN = ISP
                        CMAMIN = NOCMP
                        VAAMIN = ABS(X)
                     ENDIF
                  ENDIF
 130           CONTINUE
 120        CONTINUE
 110     CONTINUE
 100  CONTINUE
C
      CALL JENUNO(JEXNUM(MA//'.NOMMAI',IMAMAX),MAMAX)
      CALL JENUNO(JEXNUM(MA//'.NOMMAI',IMAMIN),MAMIN)
      CALL JENUNO(JEXNUM(MA//'.NOMNOE',IPTMAX),NOMAX)
      CALL JENUNO(JEXNUM(MA//'.NOMNOE',IPTMIN),NOMIN)
C
      CALL JENUNO(JEXNUM(MA//'.NOMMAI',IMAAAX),MAAMAX)
      CALL JENUNO(JEXNUM(MA//'.NOMMAI',IMAAIN),MAAMIN)
      CALL JENUNO(JEXNUM(MA//'.NOMNOE',IPAMAX),NOAMAX)
      CALL JENUNO(JEXNUM(MA//'.NOMNOE',IPAMIN),NOAMIN)
C
C --- MENAGE
      CALL DETRSD ( 'CHAM_ELEM_S', CHAMS1 )
      CALL JEDETR ( MESMAI )
      CALL JEDETR ('&&PREXEL.NOM_CMP' )
C
      END
