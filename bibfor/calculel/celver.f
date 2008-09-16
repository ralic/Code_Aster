      SUBROUTINE CELVER(CELZ,TYPVER,ARRET,IRET)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 16/09/2008   AUTEUR PELLET J.PELLET 
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
C RESPONSABLE                            VABHHTS J.PELLET
      IMPLICIT NONE
      CHARACTER*(*) CELZ,TYPVER,ARRET
      INTEGER IRET
C ------------------------------------------------------------------
C BUT : VERIFIER QUE LE CHAM_ELEM  CEL  A UNE CERTAINE PROPRIETE
C       SINON ERREUR '<F>' (OU BIEN CODE_RETOUR:1)
C ------------------------------------------------------------------

C CELZ    IN/JXIN  K19 : SD CHAM_ELEM A VERIFIER

C TYPVER  IN       K*  : TYPE DE VERIFICATION A EFFECTUER
C     /'NBSPT_1'    : LES ELEMENTS DU CHAM_ELEM N'ONT QU'1 SOUS-POINT
C     /'NBVARI_CST' : POUR UN CHAM_ELEM(VARI_R), ON VERIFIE QUE
C                     TOUS LES ELEMENTS ONT LE MEME NOMBRE DE CMPS
C     /'PAS_NAN'    : IL N'Y A PAS DE VALEURS "NAN" DANS LE CHAMP
C                     (VOIR CESCEL.F POUR LA DEFINITION DE "NAN")

C ARRET   IN   K* :  /'STOP' : ON ARRET LE CODE EN ERREUR FATALE
C                    /'COOL' : ON LAISSE PASSER MAIS ON REND IRET=1

C IRET   OUT   I :  /  0 : LA CONDITION EST VERIFIEE
C                   /  1 : LA CONDITION N'EST PAS VERIFIEE
C-----------------------------------------------------------------------

C---- COMMUNS NORMALISES  JEVEUX
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     ------------------------------------------------------------------
      CHARACTER*8 KNAN,KBID,TSCA,NOMGD
      CHARACTER*19 CEL
      INTEGER JCELD,KK,MXSPT,IGR,NGREL,NEL,IEL,IPREM,NCDYN,NCDYN1
      INTEGER IMOLO,INAN,IISNAN,NB1,K,ISNNEM,IBID,JCELV
      LOGICAL LNAN

C     ------------------------------------------------------------------
      CALL JEMARQ()
      CEL = CELZ
      IRET = 0

      CALL JEEXIN(CEL//'.CELD',KK)
      IF (KK.EQ.0) CALL U2MESK('F','CALCULEL_47',1,CEL)

      CALL JEVEUO(CEL//'.CELD','L',JCELD)


      IF (TYPVER.EQ.'NBVARI_CST') THEN
C     --------------------------------
        NGREL = ZI(JCELD-1+2)
        IPREM = 0
        DO 20,IGR = 1,NGREL
          IMOLO = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+2)
          IF (IMOLO.EQ.0) GO TO 20
          NEL = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+1)
          DO 10,IEL = 1,NEL
            NCDYN = ZI(JCELD-1+ZI(JCELD-1+4+IGR)+4+4* (IEL-1)+2)
            IPREM = IPREM + 1
            IF (IPREM.EQ.1) THEN
              NCDYN1 = NCDYN
            ELSE
              IF (NCDYN.NE.NCDYN1) THEN
                IF (ARRET.NE.'COOL') THEN
                  CALL U2MESK('F','CALCULEL_48',1,CEL)
                ELSE
                  IRET = 1
                END IF
              END IF
            END IF
   10     CONTINUE
   20   CONTINUE


      ELSE IF (TYPVER.EQ.'NBSPT_1') THEN
C     --------------------------------
        MXSPT = ZI(JCELD-1+3)
        IF (MXSPT.GT.1) THEN
          IF (ARRET.NE.'COOL') THEN
            CALL U2MESK('F','CALCULEL_49',1,CEL)
          ELSE
            IRET = 1
          END IF
        END IF


      ELSE IF (TYPVER.EQ.'PAS_NAN') THEN
C     --------------------------------
        CALL DISMOI('F','NOM_GD',CEL,'CHAMP',IBID,NOMGD,IBID)
        CALL DISMOI('F','TYPE_SCA',NOMGD,'GRANDEUR',IBID,TSCA,IBID)
        CALL JEVEUO(CEL//'.CELV','L',JCELV)
        CALL JELIRA(CEL//'.CELV','LONMAX',NB1,KBID)
        LNAN=.FALSE.
        INAN = ISNNEM()
        KNAN = '????????'

        IF (TSCA.EQ.'R') THEN
          DO 80,K = 1,NB1
            IF (IISNAN(ZR(JCELV-1+K)).EQ.1) LNAN=.TRUE.
   80     CONTINUE
        ELSEIF (TSCA.EQ.'C') THEN
          DO 81,K = 1,NB1
            IF (IISNAN(DBLE(ZC(JCELV-1+K))).EQ.1) LNAN=.TRUE.
   81     CONTINUE
        ELSEIF (TSCA.EQ.'I') THEN
          DO 82,K = 1,NB1
            IF (ZI(JCELV-1+K).EQ.INAN) LNAN=.TRUE.
   82     CONTINUE
        ELSEIF (TSCA.EQ.'K8') THEN
          DO 83,K = 1,NB1
            IF (ZK8(JCELV-1+K).EQ.KNAN) LNAN=.TRUE.
   83     CONTINUE
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF

        IF (LNAN) THEN
          IF (ARRET.NE.'COOL') THEN
            CALL U2MESK('F','CALCULEL4_1',1,CEL)
          ELSE
            IRET = 1
          END IF
        END IF


      ELSE
        CALL ASSERT(.FALSE.)
      END IF

      CALL JEDEMA()
      END
