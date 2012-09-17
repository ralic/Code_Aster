      SUBROUTINE SSDMRG(MAG)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SOUSTRUC  DATE 18/09/2012   AUTEUR LADIER A.LADIER 
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
      IMPLICIT NONE
C     ARGUMENTS:
C     ----------
      INCLUDE 'jeveux.h'
      CHARACTER*8 MAG
C ----------------------------------------------------------------------
C     BUT:
C        - TRAITER LE MOTS CLEF "RECO_GLOBAL"
C          DE LA COMMANDE DEFI_MAILLAGE.
C
C     IN:
C        MAG : NOM DU MAILLAGE QUE L'ON DEFINIT.
C     VAR:
C        --MODIFICATION DE L'OBJET .NOEUD_CONF CREE DANS SSDMRC
C
      CHARACTER*8  KBID,CRIT
      REAL*8 PREC,DI,DJ
      INTEGER      IARG
C ----------------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER I ,IACOO2 ,IADIM2 ,IADIME ,IALIIS ,IALIK8 ,IANCNF 
      INTEGER IAPARR ,IASUPI ,IASUPJ ,ICONF ,II ,INOI ,INOJ 
      INTEGER IOCC ,ISMA ,J ,JJ ,JSMA ,N1 ,NBNOI 
      INTEGER NBNOJ ,NBSMA ,NBSMAR ,NNNOE ,NOCC 
C-----------------------------------------------------------------------
      CALL JEMARQ()
      CALL GETFAC('RECO_GLOBAL',NOCC)
      IF (NOCC.EQ.0) GO TO 9999
C
C     -- ON RECUPERE CERTAINES DIMENSIONS:
C     ------------------------------------
      CALL JEVEUO(MAG//'.DIME','L',IADIME)
      NBSMA=ZI(IADIME-1+4)
      NNNOE=ZI(IADIME-1+1)
C
      CALL JEVEUO(MAG//'.NOEUD_CONF','E',IANCNF)
C
      CALL JEVEUO(MAG//'.COORDO_2','L',IACOO2)
      CALL JEVEUO(MAG//'.DIME_2','L',IADIM2)
      CALL JEVEUO(MAG//'.PARA_R','L',IAPARR)
      CALL WKVECT('&&SSDMRG.LIK8','V V K8',NBSMA,IALIK8)
      CALL WKVECT('&&SSDMRG.LIIS','V V I',NBSMA,IALIIS)
C
C
C     -- BOUCLE SUR LES OCCURENCES DU MOT-CLEF:
C     -----------------------------------------
      DO 2, IOCC=1,NOCC
C
C     -- ON RECUPERE LA LISTE DES MAILLES A TRAITER :
C     -----------------------------------------------
        CALL GETVTX('RECO_GLOBAL','TOUT',IOCC,IARG,1,KBID,N1)
        IF (N1.EQ.1) THEN
          NBSMAR= NBSMA
          DO 3, I=1,NBSMAR
            ZI(IALIIS-1+I)=I
 3        CONTINUE
        ELSE
          CALL GETVEM(MAG,'MAILLE','RECO_GLOBAL','SUPER_MAILLE',
     &                IOCC,IARG,NBSMA,ZK8(IALIK8),N1)
          IF (N1.LT.0) CALL U2MESS('F','SOUSTRUC_63')
          NBSMAR= N1
          DO 4, I=1,NBSMAR
            CALL JENONU(JEXNOM(MAG//'.SUPMAIL',ZK8(IALIK8-1+I)),ISMA)
            ZI(IALIIS-1+I)=ISMA
 4        CONTINUE
        END IF
C
        CALL GETVR8('RECO_GLOBAL','PRECISION',IOCC,IARG,1,PREC,N1)
        CALL GETVTX('RECO_GLOBAL','CRITERE',IOCC,IARG,1,CRIT,N1)
C
          DO 5, I=1,NBSMAR
            ISMA=ZI(IALIIS-1+I)
            CALL JEVEUO(JEXNUM(MAG//'.SUPMAIL',ISMA),'L',IASUPI)
            NBNOI=ZI(IADIM2-1+4*(ISMA-1)+1)+ZI(IADIM2-1+4*(ISMA-1)+2)
            DI=ZR(IAPARR-1+14*(ISMA-1)+13)
            DO 6, J=I+1,NBSMAR
              JSMA=ZI(IALIIS-1+J)
              CALL JEVEUO(JEXNUM(MAG//'.SUPMAIL',JSMA),'L',IASUPJ)
              NBNOJ=ZI(IADIM2-1+4*(JSMA-1)+1)+ZI(IADIM2-1+4*(JSMA-1)+2)
              DJ=ZR(IAPARR-1+14*(JSMA-1)+13)
              DJ=MIN(DI,DJ)
              DO 7, II=1,NBNOI
                INOI=ZI(IASUPI-1+II)
C               -- SI C'EST UN NOEUD DE LAGRANGE, ON SAUTE :
                IF (INOI.GT.NNNOE) GO TO 7
                DO 8, JJ=1,NBNOJ
                  INOJ=ZI(IASUPJ-1+JJ)
                  IF (INOJ.GT.NNNOE) GO TO 8
                  CALL SSDMU1(DJ,CRIT,PREC,ZR(IACOO2+3*(INOI-1)),
     &                        ZR(IACOO2+3*(INOJ-1)),ICONF)
                  IF (ICONF.EQ.0) THEN
                    IF (INOI.LT.INOJ) THEN
                      ZI(IANCNF-1+INOJ)=INOI
                    ELSE
                      ZI(IANCNF-1+INOI)=INOJ
                    END IF
                  END IF
 8              CONTINUE
 7            CONTINUE
 6        CONTINUE
 5        CONTINUE
C
 2    CONTINUE
C
C
 9999 CONTINUE
C --- MENAGE
      CALL JEDETR('&&SSDMRG.LIK8')
      CALL JEDETR('&&SSDMRG.LIIS')
C
      CALL JEDEMA()
      END
