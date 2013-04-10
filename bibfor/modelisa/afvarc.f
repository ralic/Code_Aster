      SUBROUTINE AFVARC(CHMAT,NOMAIL,NOMODE)
      IMPLICIT   NONE
      CHARACTER*8 CHMAT,NOMAIL,NOMODE
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 09/04/2013   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE PELLET J.PELLET
C ----------------------------------------------------------------------
C     TRAITEMENT DE AFFE_MATERIAU / AFFE_VARC
C ----------------------------------------------------------------------

      INCLUDE 'jeveux.h'
      COMPLEX*16         CBID

      INTEGER N1,N2,NBOCCV,NBMA,K,NCMP
      INTEGER IFAC,NBFAC,NMXFAC,NMXCMP,IBID,NBVARC,NBTOU,JMA
      INTEGER IOCC,JNCMP1,JNCMP2,JVALV1,JVALV2,KVARC,NBCVRC
      INTEGER JCVNOM,JCVVAR,JCVCMP,JCVGD,ITROU,NBM1,NBGM1,IER
      INTEGER IARG,NREF,NBDETR,NBGDUT,NBGDMX,ICO,JADETR
      INTEGER JVALE,JDESC

      CHARACTER*8 K8B,TYPMCL(2),NOMGD,KBID
      CHARACTER*8 NOMGD2,CHAMGD,EVOL,NOCMP1,NOCMP2,FINST,EVOUCH
      CHARACTER*16 MOTCLE(2),NOMCHA,PROLGA,PROLDR,K16A,K16B,K16C
      CHARACTER*24 MESMAI,CVNOM,CVVAR,CVGD,CVCMP,VALK(3)
      PARAMETER (NMXFAC=20,NMXCMP=20)
      CHARACTER*16 MOTFAC(NMXFAC),LIMFAC(NMXFAC),MOFAC
      CHARACTER*19 CART1,CART2,CARVID
      CHARACTER*8  NOVARC,NOVAR1,NOVAR2,LIVARC(NMXFAC),KNUMER
      CHARACTER*8  NOCVRC
      CHARACTER*32 JEXNOM
      REAL*8 VRCREF(NMXCMP),R8NNEM,RCMP(10),R8VIDE,VREF
      LOGICAL ERRGD,LDETR,LAUTR
C ----------------------------------------------------------------------

      CALL JEMARQ()

      MESMAI = '&&AFVARC.MES_MAILLES'
      MOTCLE(1) = 'GROUP_MA'
      MOTCLE(2) = 'MAILLE'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
      CALL GETFAC('AFFE_VARC',NBOCCV)


C     1. CALCUL DE NBVARC, NBCVRC, LIVARC ET LIMFAC
C        ALLOCATION DES 5 OBJETS : .CVRCNOM .CVRCVARC ...
C     -------------------------------------------------
      CALL GETVTX(' ','LIST_NOM_VARC',1,IARG,NMXFAC,LIVARC,N1)
      CALL ASSERT(N1.GT.0)
      NBFAC=N1
      DO 1,IFAC=1,NBFAC
        MOTFAC(IFAC)='VARC_'//LIVARC(IFAC)
1     CONTINUE

      NBVARC = 0
      NBCVRC=0
      DO 20,IFAC = 1,NBFAC
        MOFAC=MOTFAC(IFAC)
        CALL ASSERT(MOFAC(1:5).EQ.'VARC_')
        CALL GETVTX(MOFAC,'NOM_VARC',1,IARG,1,NOVARC,N1)
        CALL ASSERT(N1.EQ.1)
        ITROU=0
        DO 21,IOCC = 1,NBOCCV
          CALL GETVTX('AFFE_VARC','NOM_VARC',IOCC,IARG,1,NOVAR1,N1)
          CALL ASSERT(N1.EQ.1)
          IF (NOVAR1.EQ.NOVARC) ITROU=1
21      CONTINUE
        IF (ITROU.EQ.0) GO TO 20
        NBVARC = NBVARC + 1
        LIVARC(NBVARC) = NOVARC
        LIMFAC(NBVARC) = MOFAC
        CALL GETVTX(MOFAC,'CMP_GD',1,IARG,0,K8B,NCMP)
        CALL ASSERT(NCMP.LT.0)
        NCMP=-NCMP
        NBCVRC=NBCVRC+NCMP
   20 CONTINUE
      IF (NBVARC.EQ.0) GO TO 9999


      CVNOM = CHMAT//'.CVRCNOM'
      CVVAR = CHMAT//'.CVRCVARC'
      CVGD  = CHMAT//'.CVRCGD'
      CVCMP = CHMAT//'.CVRCCMP'
      CALL WKVECT(CVNOM,'G V K8',NBCVRC,JCVNOM)
      CALL WKVECT(CVVAR,'G V K8',NBCVRC,JCVVAR)
      CALL WKVECT(CVGD ,'G V K8',NBCVRC,JCVGD)
      CALL WKVECT(CVCMP,'G V K8',NBCVRC,JCVCMP)



C     2. ALLOCATION ET REMPLISSAGE DES CARTES
C        REMPLISSAGE DE .CVRCNOM .CVRCVARC ...
C     --------------------------------------------
      NBCVRC=0
      DO 90,KVARC = 1,NBVARC
        MOFAC = LIMFAC(KVARC)

        CART1 = CHMAT//'.'//LIVARC(KVARC)//'.1'
        CART2 = CHMAT//'.'//LIVARC(KVARC)//'.2'
        CALL ALCART('G',CART1,NOMAIL,'NEUT_R')
        CALL ALCART('G',CART2,NOMAIL,'NEUT_K16')
        CALL JEVEUO(CART1//'.NCMP','E',JNCMP1)
        CALL JEVEUO(CART1//'.VALV','E',JVALV1)
        CALL JEVEUO(CART2//'.NCMP','E',JNCMP2)
        CALL JEVEUO(CART2//'.VALV','E',JVALV2)
        NOCMP1 = 'X'
        DO 30,K = 1,NMXCMP
          CALL CODENT(K,'G',NOCMP1(2:8))
          ZK8(JNCMP1-1+K) = NOCMP1
   30   CONTINUE
        NOCMP2 = 'Z'
        DO 40,K = 1,7
          CALL CODENT(K,'G',NOCMP2(2:8))
          ZK8(JNCMP2-1+K) = NOCMP2
   40   CONTINUE


C       2.1 REPLISSAGE DE .CVRCNOM, .CVRCVARC, ...
C       ------------------------------------------------------------
        CALL GETVTX(MOFAC,'NOM_VARC',1,IARG,1,NOVARC,N1)
        CALL GETVTX(MOFAC,'GRANDEUR',1,IARG,1,NOMGD,N1)
        CALL GETVTX(MOFAC,'CMP_GD',1,IARG,NMXCMP,
     &              ZK8(JCVCMP+NBCVRC),NCMP)
        CALL ASSERT(NCMP.GE.1)
        CALL GETVTX(MOFAC,'CMP_VARC',1,IARG,NMXCMP,
     &              ZK8(JCVNOM+NBCVRC),N1)
        CALL ASSERT(N1.EQ.NCMP)
        DO 49,K = 1,NCMP
            ZK8(JCVVAR+NBCVRC-1+K) = NOVARC
            ZK8(JCVGD +NBCVRC-1+K) = NOMGD
   49   CONTINUE

        DO 80,IOCC = 1,NBOCCV
          CALL GETVTX('AFFE_VARC','NOM_VARC',IOCC,IARG,1,NOVAR2,N1)
          CALL ASSERT(N1.EQ.1)
          IF (NOVAR2.NE.NOVARC) GO TO 80

C         2.2 CALCUL DE  VRCREF(:) :
C         ---------------------------
          CALL GETVR8('AFFE_VARC','VALE_REF',IOCC,IARG,NMXCMP,VRCREF,N1)
C         -- ON NE PEUT DONNER QU'UNE SEULE VALEUR (TEMP OU SECH) :
          NREF=N1
          CALL ASSERT(N1.EQ.0 .OR. N1.EQ.1)
          IF (N1.EQ.1) THEN
            VREF=VRCREF(1)
          ELSE
            VREF=R8VIDE()
          ENDIF
C         -- IL FAUT RECOPIER VREF POUR TEMP QUI A PLUSIEURS CMPS :
          DO 60,K = 1,NCMP
            VRCREF(K) = VREF
   60     CONTINUE


C         2.3 CALCUL DE EVOL,CHAMGD,NOMCHA ET VERIFICATIONS :
C         ------------------------------------------------------------
          EVOL = ' '
          CHAMGD = ' '
          NOMCHA = ' '
          ERRGD = .FALSE.

          CALL GETVID('AFFE_VARC','CHAM_GD',IOCC,IARG,1,CHAMGD,N1)
          CALL GETVID('AFFE_VARC','EVOL',IOCC,IARG,1,EVOL,N2)
          CALL ASSERT(N1+N2.LE.1)
          IF (N1.EQ.1) THEN
            EVOUCH='CHAMP'
          ELSEIF (N2.EQ.1) THEN
            EVOUCH='EVOL'
          ELSE
            EVOUCH='VIDE'
            IF (NOVARC.NE.'TEMP') CALL U2MESK('F','CALCULEL4_11',
     &                                        1 ,NOVARC)
C           -- POUR LA THM, ON PEUT UTILISER VALE_REF SANS DONNER
C              CHAM_GD NI EVOL :
            CALL ASSERT(NREF.EQ.1)
          ENDIF


          IF (EVOUCH.EQ.'CHAMP') THEN
            CALL DISMOI('F','NOM_GD',CHAMGD,'CHAMP',IBID,NOMGD2,
     &                  IER)
            IF (NOMGD2.NE.NOMGD) ERRGD = .TRUE.

          ELSEIF (EVOUCH.EQ.'EVOL') THEN
            CALL GETVTX('AFFE_VARC','NOM_CHAM',IOCC,IARG,1,NOMCHA,N1)
C           -- NOM_CHAMP (VALEUR PAR DEFAUT) :
            IF (N1.EQ.0) THEN
              IF (NOVARC.EQ.'SECH') THEN
                NOMCHA='TEMP'
              ELSEIF (NOVARC.EQ.'HYDR')  THEN
                NOMCHA='HYDR_ELNO'
              ELSEIF (NOVARC.EQ.'HYDR')   THEN
                NOMCHA='EPSA'
              ELSEIF (NOVARC.EQ.'EPSA_ELNO') THEN
                NOMCHA='NEUT'
              ELSEIF (NOVARC.EQ.'M_ACIER') THEN
                NOMCHA='META_ELNO'
              ELSEIF (NOVARC.EQ.'M_ZIRC') THEN
                NOMCHA='META_ELNO'
              ELSEIF (NOVARC(1:4).EQ.'NEUT') THEN
                NOMCHA='NEUT'
              ELSE
                NOMCHA=NOVARC
              ENDIF
            ENDIF
            CALL GETVTX('AFFE_VARC','PROL_GAUCHE',IOCC,IARG,1,PROLGA,N1)
            CALL GETVTX('AFFE_VARC','PROL_DROITE',IOCC,IARG,1,PROLDR,N1)
            CALL GETVID('AFFE_VARC','FONC_INST',IOCC,IARG,1,FINST,N1)
            IF (N1.EQ.0) FINST=' '
C           A FAIRE ??? VERIFIER QUE EVOL+NOMCHA => LA BONNE GRANDEUR
          END IF

          IF (ERRGD) THEN
             VALK(1) = MOFAC
             VALK(2) = NOMGD
             VALK(3) = NOMGD2
             CALL U2MESK('A','MODELISA5_50', 3 ,VALK)
          ENDIF

C         2.4 ECRITURE DANS LES CARTES :
C         ------------------------------------------------------------
          ZK16(JVALV2-1+1) = LIVARC(KVARC)
          IF (EVOUCH.EQ.'CHAMP') THEN
            ZK16(JVALV2-1+2) = 'CHAMP'
            ZK16(JVALV2-1+3) = CHAMGD
            ZK16(JVALV2-1+4) = ' '
            ZK16(JVALV2-1+5) = ' '
            ZK16(JVALV2-1+6) = ' '
            ZK16(JVALV2-1+7) = ' '
          ELSE IF (EVOUCH.EQ.'EVOL') THEN
            ZK16(JVALV2-1+2) = 'EVOL'
            ZK16(JVALV2-1+3) = EVOL
            ZK16(JVALV2-1+4) = NOMCHA
            ZK16(JVALV2-1+5) = PROLGA
            ZK16(JVALV2-1+6) = PROLDR
            ZK16(JVALV2-1+7) = FINST
          ELSE IF (EVOUCH.EQ.'VIDE') THEN
C           -- ON AFFECTE UNE CARTE CONTENANT DES R8NNEM :
            CALL GCNCON('_',KNUMER)
            CARVID = KNUMER
            CALL ASSERT(NCMP.LE.10)
            DO 31,K = 1,NCMP
              RCMP(K) = R8NNEM()
   31       CONTINUE
            CALL MECACT('G',CARVID,'MAILLA',NOMAIL,NOMGD,NCMP,
     &                  ZK8(JCVCMP+NBCVRC),IBID,RCMP,CBID,KBID)

            ZK16(JVALV2-1+2) = 'CHAMP'
            ZK16(JVALV2-1+3) = CARVID(1:16)
            ZK16(JVALV2-1+4) = ' '
            ZK16(JVALV2-1+5) = ' '
            ZK16(JVALV2-1+6) = ' '
            ZK16(JVALV2-1+7) = ' '
          END IF
          DO 70,K = 1,NCMP
            ZR(JVALV1-1+K) = VRCREF(K)
   70     CONTINUE

C         TOUT='OUI' PAR DEFAUT :
          CALL GETVTX('AFFE_VARC','TOUT',IOCC,IARG,1,K8B,NBTOU)
          CALL GETVTX('AFFE_VARC','GROUP_MA',IOCC,IARG,0,K8B,NBGM1)
          CALL GETVTX('AFFE_VARC','MAILLE',IOCC,IARG,0,K8B,NBM1)
          IF (NBGM1+NBM1.EQ.0) NBTOU=1

          IF (NBTOU.NE.0) THEN
            CALL NOCART(CART1,1,' ','NOM',0,' ',0,' ',NCMP)
            CALL NOCART(CART2,1,' ','NOM',0,' ',0,' ',7)
          ELSE
            CALL RELIEM(NOMODE,NOMAIL,'NU_MAILLE','AFFE_VARC',IOCC,
     &                  2,MOTCLE,TYPMCL,MESMAI,NBMA)
            IF (NBMA.EQ.0) GOTO 80
            CALL JEVEUO(MESMAI,'L',JMA)
            CALL NOCART(CART1,3,K8B,'NUM',NBMA,' ',ZI(JMA),' ',NCMP)
            CALL NOCART(CART2,3,K8B,'NUM',NBMA,' ',ZI(JMA),' ',7)
            CALL JEDETR(MESMAI)
          END IF


   80   CONTINUE

        NBCVRC=NBCVRC+NCMP
   90 CONTINUE


C     3. POUR NE PAS PENALISER LES CALCULS N'AYANT QUE LA COMPOSANTE
C        TEMP (POUR NOM_VARC='TEMP'), ON VA TENTER DE REDUIRE NBCVRC :
C     -----------------------------------------------------------------
      CALL WKVECT('&&AFVARC.ADETR','V V I',NBCVRC,JADETR)
      NBDETR=0
      DO 91, K=1,NBCVRC
        NOVARC=ZK8(JCVVAR-1+K)
        NOCVRC=ZK8(JCVNOM-1+K)
        IF (NOVARC.EQ.'TEMP') THEN
          IF (NOCVRC.NE.'TEMP') THEN
             NBDETR=NBDETR+1
             ZI(JADETR-1+K)=1
          ENDIF
        ENDIF
  91  CONTINUE
      IF (NBDETR.EQ.0) GOTO 9999

C     3.1 PEUT-ON REELLEMENT SUPPRIMER CES CVRC ?
C     -------------------------------------------
      LDETR=.TRUE.
      CART2 = CHMAT//'.TEMP    .2'
      CALL JEVEUO(CART2//'.DESC','L',JDESC)
      CALL JEVEUO(CART2//'.VALE','L',JVALE)
      CALL JELIRA(JEXNOM('&CATA.GD.NOMCMP','NEUT_K16'),
     &            'LONMAX',NCMP,KBID)
      NBGDMX=ZI(JDESC-1+2)
      NBGDUT=ZI(JDESC-1+3)
      CALL JELIRA(CHMAT//'.TEMP    .2.VALE','LONMAX',N1,KBID)
      CALL ASSERT(N1.EQ.NBGDMX*NCMP)
C     3.2 ON PARCOURT LES SD STOCKEES DANS LA CARTE ET ON REGARDE S'IL
C         EXISTE D'AUTRES CMPS QUE TEMP ET LAGR :  LAUTR=.TRUE.
C     --------------------------------------------------------------
      DO 93, K=1,NBGDUT
        K16A=ZK16(JVALE-1+NCMP*(K-1)+1)
        CALL ASSERT(K16A.EQ.'TEMP')
        K16A=ZK16(JVALE-1+NCMP*(K-1)+2)
        K16B=ZK16(JVALE-1+NCMP*(K-1)+3)
        K16C=ZK16(JVALE-1+NCMP*(K-1)+4)
        CALL AFVA01(K16A,K16B,K16C,LAUTR)
        IF (LAUTR) THEN
          LDETR=.FALSE.
          GOTO 94
        ENDIF

 93   CONTINUE
 94   CONTINUE

C     3.3 ON SUPPRIME CE QUI NE SERT A RIEN :
C     ----------------------------------------
      IF (LDETR) THEN
        ICO=0
        DO 92, K=1,NBCVRC
          IF (ZI(JADETR-1+K).EQ.0) THEN
            ICO=ICO+1
            ZK8(JCVNOM-1+ICO)=ZK8(JCVNOM-1+K)
            ZK8(JCVVAR-1+ICO)=ZK8(JCVVAR-1+K)
            ZK8(JCVGD-1+ICO)=ZK8(JCVGD-1+K)
            ZK8(JCVCMP-1+ICO)=ZK8(JCVCMP-1+K)
          ENDIF
  92    CONTINUE
        CALL ASSERT(ICO.EQ.NBCVRC-NBDETR)
        CALL JUVECA(CVNOM,ICO)
        CALL JUVECA(CVVAR,ICO)
        CALL JUVECA(CVGD,ICO)
        CALL JUVECA(CVCMP,ICO)
      ENDIF


9999  CONTINUE

      CALL JEDEMA()
      END
