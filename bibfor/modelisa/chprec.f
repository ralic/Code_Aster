      SUBROUTINE CHPREC(CHOU)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 24/07/2012   AUTEUR PELLET J.PELLET 
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
C     TRAITEMENT DE COMMANDE:   CREA_CHAMP / OPTION: 'EXTR'

C     ------------------------------------------------------------------
C
      IMPLICIT   NONE

C
C 0.1. ==> ARGUMENTS
C
      INCLUDE 'jeveux.h'
      CHARACTER*(*) CHOU
C
C 0.2. ==> COMMUNS
C
C
C 0.3. ==> VARIABLES LOCALES
C
      CHARACTER*6 NOMPRO
      PARAMETER ( NOMPRO = 'CHPREC' )
C
      INTEGER IBID,ICORET,IRET,JORDR,N1,N2,N3,N4,N5,NBORDR,NC,NP,IE
      INTEGER IFM,NIV
      REAL*8 INST,EPSI
      CHARACTER*1 BASE
      CHARACTER*8 RESUCO,INTERP,CRIT,PROLDR,PROLGA,TYPMAX
      CHARACTER*8 NOMGD
      CHARACTER*16 K16BID,NOMCMD,NOMCH,ACCES,TYSD,TYCHLU,TYCH
      CHARACTER*19 CHEXTR,NOCH19,KNUM
      CHARACTER*24 VALK(3)
      CHARACTER*8 K8BID,MA,FIS
      LOGICAL GRILLE
      INTEGER      IARG
C     ------------------------------------------------------------------

      CALL JEMARQ()
      CALL INFNIV(IFM,NIV)

C
      BASE = 'G'
      CALL GETRES(K8BID,K16BID,NOMCMD)
      NOCH19 = CHOU

      CALL GETVTX(' ','NOEUD_CMP',0,IARG,0,K8BID,N1)
      IF (N1.NE.0 .AND. N1.NE.-2) CALL U2MESS('F','MODELISA4_16')
      NOMCH=' '
      CALL GETVTX(' ','NOM_CHAM',0,IARG,1,NOMCH,N2)
      TYCHLU=' '
      CALL GETVTX(' ','TYPE_CHAM',0,IARG,1,TYCHLU,N2)

C     1. CAS DE LA RECUPERATION DU CHAMP DE GEOMETRIE D'UN MAILLAGE
C     ==============================================================
      IF (NOMCH.EQ.'GEOMETRIE') THEN
        CALL GETVID(' ','MAILLAGE',0,IARG,1,MA,N1)
        IF (N1.EQ.0) CALL U2MESS('F','MODELISA4_17')
C
C     ON VERIFIE QUE LE MOT-CLE TYPE_CHAMP EST COHERENT AVEC LE
C     TYPE DU CHAMP EXTRAIT.
C
        CALL DISMOI('F','TYPE_CHAMP',MA//'.COORDO','CHAMP',
     &              IBID,TYCH,IE)
        CALL DISMOI('F','NOM_GD',MA//'.COORDO','CHAMP',
     &              IBID,NOMGD,IE)

        IF ((TYCHLU(1:4).NE.TYCH).OR.(TYCHLU(6:12).NE.NOMGD)) THEN
            VALK(1) = TYCHLU
            VALK(2) = TYCH(1:4)
            VALK(3) = NOMGD
            CALL U2MESK('F','MODELISA4_18', 3 ,VALK)
        END IF
        CALL COPISD('CHAMP_GD','G',MA//'.COORDO',NOCH19)
        GO TO 20
      END IF

C     2. CAS DE LA RECUPERATION D'UN CHAMP DANS UNE SD FISS_XFEM
C     ==============================================================
      CALL GETVID(' ','FISSURE',0,IARG,1,FIS,N1)
             IF (N1.EQ.1) THEN

C              VERIFIE SI UNE GRILLE AUXILIAIRE EST DEFINIE POUR LA FISS
               CALL JEEXIN(FIS//'.GRI.MODELE',IBID)
               IF (IBID.EQ.0) THEN
                  GRILLE=.FALSE.
               ELSE
                  GRILLE=.TRUE.
               ENDIF

               IF (NOMCH.EQ.'LTNO') THEN
                 CHEXTR = FIS//'.LTNO'
               ELSE IF (NOMCH.EQ.'LNNO') THEN
                 CHEXTR = FIS//'.LNNO'
               ELSE IF (NOMCH.EQ.'GRLNNO') THEN
                 CHEXTR = FIS//'.GRLNNO'
               ELSE IF (NOMCH.EQ.'GRLTNO') THEN
                 CHEXTR = FIS//'.GRLTNO'
               ELSE IF (NOMCH.EQ.'STNO') THEN
                 CHEXTR = FIS//'.STNO'
               ELSE IF (NOMCH.EQ.'STNOR') THEN
                 CHEXTR = FIS//'.STNOR'
               ELSE IF (NOMCH.EQ.'BASLOC') THEN
                 CHEXTR = FIS//'.BASLOC'
               ELSE
                 IF (GRILLE) THEN
                     IF (NOMCH.EQ.'GRI.LTNO') THEN
                        CHEXTR = FIS//'.GRI.LTNO'
                     ELSE IF (NOMCH.EQ.'GRI.LNNO') THEN
                        CHEXTR = FIS//'.GRI.LNNO'
                     ELSE IF (NOMCH.EQ.'GRI.GRLNNO') THEN
                        CHEXTR = FIS//'.GRI.GRLNNO'
                     ELSE IF (NOMCH.EQ.'GRI.GRLTNO') THEN
                        CHEXTR = FIS//'.GRI.GRLTNO'
                     END IF
                 ELSE
                     CALL U2MESS('F','XFEM2_98')
                 ENDIF
               ENDIF
C
C     ON VERIFIE QUE LE MOT-CLE TYPE_CHAMP EST COHERENT AVEC LE
C     TYPE DU CHAMP EXTRAIT.
C
               CALL DISMOI('F','TYPE_CHAMP',CHEXTR,'CHAMP',IBID,TYCH,IE)
               CALL DISMOI('F','NOM_GD',CHEXTR,'CHAMP',IBID,NOMGD,IE)

               IF ((TYCHLU(1:4).NE.TYCH).OR.(TYCHLU(6:12).NE.NOMGD))THEN
                    VALK(1) = TYCHLU
                    VALK(2) = TYCH(1:4)
                    VALK(3) = NOMGD
                    CALL U2MESK('F','MODELISA4_18', 3 ,VALK)
               END IF
               CALL COPISD('CHAMP_GD','G',CHEXTR,NOCH19)
               GOTO 20
             END IF

C     3. CAS DE LA RECUPERATION D'UN CHAMP D'UNE SD RESULTAT
C     ==============================================================
      CALL GETVID(' ','RESULTAT',0,IARG,1,RESUCO,N1)
      INTERP=' '
      CALL GETVTX(' ','INTERPOL',0,IARG,1,INTERP,N3)
      TYPMAX=' '
      CALL GETVTX(' ','TYPE_MAXI',0,IARG,1,TYPMAX,N5)
      CALL GETTCO(RESUCO,TYSD)
C
C     --- ON PEUT FAIRE UNE INTERPOLATION ---
C         ===============================
      IF (TYSD.EQ.'EVOL_THER' .OR. TYSD.EQ.'EVOL_ELAS' .OR.
     &    TYSD.EQ.'EVOL_NOLI' .OR. TYSD.EQ.'DYNA_TRANS' .OR.
     &    TYSD.EQ.'EVOL_VARC') THEN

        IF (INTERP(1:3).EQ.'LIN') THEN
          CALL GETVR8(' ','INST',0,IARG,1,INST,N4)
          CALL ASSERT(N4.EQ.1)
          PROLDR = 'EXCLUS'
          PROLGA = 'EXCLUS'
          ACCES = 'INST'
          CALL RSINCH(RESUCO,NOMCH,ACCES,INST,NOCH19,PROLDR,PROLGA,2,
     &                BASE,ICORET)
        ELSE
          IF (N5.NE.0) THEN
            CALL CHMIMA(RESUCO,NOMCH,TYPMAX,NOCH19)
          ELSE
            KNUM = '&&'//NOMPRO//'.NUME_ORDRE'
            CALL GETVR8(' ','PRECISION',1,IARG,1,EPSI,NP)
            CALL GETVTX(' ','CRITERE',1,IARG,1,CRIT,NC)
            CALL RSUTNU(RESUCO,' ',0,KNUM,NBORDR,EPSI,CRIT,IRET)
            IF ((IRET.NE.0) .OR. (NBORDR.GT.1)) GO TO 10
            IF (NBORDR.EQ.0) THEN
              CALL U2MESS('F','UTILITAI_23')
            END IF
            CALL JEVEUO(KNUM,'L',JORDR)
            CALL RSEXCH('F',RESUCO,NOMCH,ZI(JORDR),CHEXTR,IRET)

C
C           ON VERIFIE QUE LE MOT-CLE TYPE_CHAMP EST COHERENT AVEC LE
C           TYPE DU CHAMP EXTRAIT.
C
            CALL DISMOI('F','TYPE_CHAMP',CHEXTR,'CHAMP',IBID,TYCH,IE)
            CALL DISMOI('F','NOM_GD',CHEXTR,'CHAMP',IBID,NOMGD,IE)

            IF ((TYCHLU(1:4).NE.TYCH).OR.(TYCHLU(6:12).NE.NOMGD)) THEN
              VALK(1) = TYCHLU
              VALK(2) = TYCH(1:4)
              VALK(3) = NOMGD
              CALL U2MESK('F','MODELISA4_18', 3 ,VALK)
            END IF
            CALL COPISD('CHAMP_GD','G',CHEXTR,NOCH19)
            CALL JEDETR(KNUM)
          END IF
        END IF

C     --- ON NE FAIT QU'UNE EXTRACTION ---
C         ===========================
      ELSE
        IF (INTERP(1:3).EQ.'LIN') THEN
          VALK(1) = TYSD
          CALL U2MESG('F', 'MODELISA8_55',1,VALK,0,0,0,0.D0)
        ELSE
          KNUM = '&&'//NOMPRO//'.NUME_ORDRE'
          CALL GETVR8(' ','PRECISION',1,IARG,1,EPSI,NP)
          CALL GETVTX(' ','CRITERE',1,IARG,1,CRIT,NC)
          CALL RSUTNU(RESUCO,' ',0,KNUM,NBORDR,EPSI,CRIT,IRET)
          IF ((IRET.NE.0) .OR. (NBORDR.GT.1)) GO TO 10
          IF (NBORDR.EQ.0) THEN
            CALL U2MESS('F','UTILITAI_23')
          END IF
          CALL JEVEUO(KNUM,'L',JORDR)
          CALL RSEXCH('F',RESUCO,NOMCH,ZI(JORDR),CHEXTR,IRET)
              CALL DISMOI('F','TYPE_CHAMP',CHEXTR,'CHAMP',IBID,TYCH,IE)
              CALL DISMOI('F','NOM_GD',CHEXTR,'CHAMP',IBID,NOMGD,IE)

              IF ((TYCHLU(1:4).NE.TYCH).OR.(TYCHLU(6:12).NE.NOMGD)) THEN
            VALK(1) = TYCHLU
            VALK(2) = TYCH(1:4)
            VALK(3) = NOMGD
            CALL U2MESK('F','MODELISA4_18', 3 ,VALK)
            END IF

            CALL COPISD('CHAMP_GD','G',CHEXTR,NOCH19)
          CALL JEDETR(KNUM)
        END IF
      END IF

C============= FIN DE LA BOUCLE SUR LE NOMBRE DE PASSAGES ==============

      GO TO 20
   10 CONTINUE
      CALL U2MESS('F','MODELISA4_19')

   20 CONTINUE
      CALL TITRE
C
      CALL JEDETC ( 'V', '&&'//NOMPRO, 1)

      CALL JEDEMA()
      END
