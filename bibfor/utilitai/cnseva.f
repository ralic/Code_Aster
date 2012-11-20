      SUBROUTINE CNSEVA(CNSF,NPARA,LPARA,CNSR)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 19/11/2012   AUTEUR PELLET J.PELLET 
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
C RESPONSABLE PELLET J.PELLET
C A_UTIL
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM
      INTEGER NPARA
      CHARACTER*(*) CNSF,LPARA(NPARA),CNSR
C ---------------------------------------------------------------------
C BUT: EVALUER LE CHAM_NO_S DE FONCTIONS CNSF EN UTILISANT
C      LES PARAMETRES TROUVES DANS LES CHAM_NO_S LPARA
C ---------------------------------------------------------------------
C ARGUMENTS:
C CNSF  IN/JXIN  K19 : SD CHAM_NO_S A EVALUER
C NPARA IN       I   : NOMBRE DE CHAM_NO_S PARAMETRES (LPARA)
C LPARA IN/JXIN  V(K19) : LISTE DES CHAM_NO_S PARAMETRES
C CNSR  IN/JXOUT K19  : SD CHAM_NO RESULTAT DE L'EVALUATION

C REMARQUES :
C  EN CHAQUE POINT DE DISCRETISATION DE CNSF, ON FERA "CALL FOINTE"
C  POUR EVALUER LES FONCTIONS AFFECTEES A CE POINT.
C  ON PASSERA EN PARAMETRES DES FONCTIONS, LES VALEURS DES CHAMPS
C  DE LPARA AVEC COMME NOM DE PARAMETRE LE NOM DE LA CMP

C  ON NE TRAITE QUE LES FONCTIONS REELLES F : R * R(* R,...) -> R
C-----------------------------------------------------------------------

C     ------------------------------------------------------------------
      INTEGER JFD,JFC,JFV,JFL,JFK
      INTEGER JPD,JPC,JPV,JPL,JPK
      INTEGER JRD,JRC,JRV,JRL,JRK
      INTEGER NBNO,IB,K,INO,NCMP,NBPU,IER,NBPUMX
      INTEGER K2,NCMP2,IPARA,JAD1,IBID,INDIK8
      PARAMETER (NBPUMX=50)
      CHARACTER*8 MA,NOMGDF,NOMGDR,FO,NOMPU(NBPUMX)
      CHARACTER*8 MA2,NOMGD2
      CHARACTER*3 TSCA
      CHARACTER*19 F,P,R
      CHARACTER*24 VALK
      REAL*8 X,VALPU(NBPUMX)
C     ------------------------------------------------------------------

      CALL JEMARQ()

C     1- RECUPERATIONS D'INFOS DANS LE CHAMP DE FONCTIONS :
C     ------------------------------------------------------------
      F = CNSF
      CALL JEVEUO(F//'.CNSK','L',JFK)
      CALL JEVEUO(F//'.CNSD','L',JFD)
      CALL JEVEUO(F//'.CNSC','L',JFC)
      CALL JEVEUO(F//'.CNSV','L',JFV)
      CALL JEVEUO(F//'.CNSL','L',JFL)

      MA = ZK8(JFK-1+1)
      NOMGDF = ZK8(JFK-1+2)
      NBNO = ZI(JFD-1+1)
      NCMP = ZI(JFD-1+2)

      CALL DISMOI('F','TYPE_SCA',NOMGDF,'GRANDEUR',IB,TSCA,IB)
      IF (TSCA.NE.'K8') CALL U2MESS('F','UTILITAI_16')


C     2- ALLOCATION DU CHAM_NO_S RESULTAT ET RECUPERATION
C        DES ADRESSES DE SES OBJETS   :
C     ------------------------------------------------------------
      R = CNSR
      NOMGDR = NOMGDF(1:4)//'_R'
      CALL CNSCRE(MA,NOMGDR,NCMP,ZK8(JFC),'V',R)
      CALL JEVEUO(R//'.CNSK','L',JRK)
      CALL JEVEUO(R//'.CNSD','L',JRD)
      CALL JEVEUO(R//'.CNSC','L',JRC)
      CALL JEVEUO(R//'.CNSV','E',JRV)
      CALL JEVEUO(R//'.CNSL','E',JRL)


C     3- ON MET EN MEMOIRE LES OBJETS UTILES DES CHAMPS PARAMETRES :
C     --------------------------------------------------------------
      CALL WKVECT('&&CNSEVA.JAD1','V V I',4*NPARA,JAD1)
      DO 10,IPARA = 1,NPARA
        P = LPARA(IPARA)
        CALL JEVEUO(P//'.CNSK','L',JPK)
        CALL JEVEUO(P//'.CNSD','L',JPD)
        CALL JEVEUO(P//'.CNSC','L',JPC)
        CALL JEVEUO(P//'.CNSV','L',JPV)
        CALL JEVEUO(P//'.CNSL','L',JPL)
        MA2 = ZK8(JPK-1+1)
        NOMGD2 = ZK8(JPK-1+2)

        CALL DISMOI('F','TYPE_SCA',NOMGD2,'GRANDEUR',IB,TSCA,IB)
        IF (TSCA.NE.'R') CALL U2MESS('F','UTILITAI_17')
        IF (MA2.NE.MA) CALL U2MESS('F','UTILITAI_18')
        ZI(JAD1-1+4* (IPARA-1)+1) = JPC
        ZI(JAD1-1+4* (IPARA-1)+2) = JPD
        ZI(JAD1-1+4* (IPARA-1)+3) = JPL
        ZI(JAD1-1+4* (IPARA-1)+4) = JPV
   10 CONTINUE



C     4- EVALUATION DES FONCTIONS :
C     ---------------------------------------
C     ON BOUCLE D'ABORD SUR LES CMPS POUR AVOIR PLUS DE CHANCES
C     DE FAIRE PLUSIEURS FOINTE SUCCESSIFS AVEC LA MEME FONCTION.

      DO 50,K = 1,NCMP
        DO 40,INO = 1,NBNO
          IF (ZL(JFL-1+ (INO-1)*NCMP+K)) THEN
            ZL(JRL-1+ (INO-1)*NCMP+K) = .TRUE.
            FO = ZK8(JFV-1+ (INO-1)*NCMP+K)
            IF (FO.EQ.' ') GO TO 40

C           4.1 FABRICATION DE LA LISTE DES PARAMETRES POUR FOINTE:
C           -------------------------------------------------------
            NBPU = 0
            DO 30,IPARA = 1,NPARA
              JPC = ZI(JAD1-1+4* (IPARA-1)+1)
              JPD = ZI(JAD1-1+4* (IPARA-1)+2)
              JPL = ZI(JAD1-1+4* (IPARA-1)+3)
              JPV = ZI(JAD1-1+4* (IPARA-1)+4)
              NCMP2 = ZI(JPD-1+2)
              DO 20,K2 = 1,NCMP2
                IF (ZL(JPL-1+ (INO-1)*NCMP2+K2)) THEN
                  NBPU = NBPU + 1
                  IF(NBPU.GT.NBPUMX) CALL U2MESS('F','CALCULEL2_66')

C                 -- ON VERIFIE QU'UN MEME PARAMETRE N'EST PAS AJOUTE
C                    PLUSIEURS FOIS:
                  IBID=INDIK8(NOMPU,ZK8(JPC-1+K2),1,NBPU-1)
                  IF(IBID.GT.0)
     &                CALL U2MESK('F','CALCULEL2_78',1,ZK8(JPC-1+K2))

                  NOMPU(NBPU) = ZK8(JPC-1+K2)
                  VALPU(NBPU) = ZR(JPV-1+ (INO-1)*NCMP2+K2)
                END IF
   20         CONTINUE
   30       CONTINUE


C           4.2 APPEL A FOINTE :
C           --------------------
            CALL FOINTE('E',FO,NBPU,NOMPU,VALPU,X,IER)
            IF (IER.NE.0) THEN
               CALL U2MESK('F+', 'FONCT0_9', 1, FO)
               CALL JENUNO(JEXNUM(MA//'.NOMNOE',INO),VALK)
               CALL U2MESK('F','FONCT0_53',1, VALK)
            ENDIF

C           4.3 STOCKAGE DU RESULTAT :
C           --------------------------
            ZR(JRV-1+ (INO-1)*NCMP+K) = X

          END IF
   40   CONTINUE
   50 CONTINUE


C     5- MENAGE :
C     ---------------------------------------
      CALL JEDETR('&&CNSEVA.JAD1')

      CALL JEDEMA()
      END
