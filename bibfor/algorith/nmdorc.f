      SUBROUTINE NMDORC ( MODELZ, COMPOZ )
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
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
      IMPLICIT NONE
      CHARACTER*(*)       MODELZ, COMPOZ
C ----------------------------------------------------------------------
C MODIF ALGORITH  DATE 26/11/2002   AUTEUR ADBHHVV V.CANO 
C     SAISIE ET VERIFICATION DE LA RELATION DE COMPORTEMENT UTILISEE
C
C IN  MODELZ  : NOM DU MODELE
C OUT COMPOZ  : CARTE DECRIVANT LE TYPE DE COMPORTEMENT
C ----------------------------------------------------------------------
C --- DEBUT DECLARATIONS NORMALISEES JEVEUX ----------------------------

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

C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------
      INTEGER      NCMPMA, DIMAKI, N2, N3, IBID, NBOCC, I, ICMP,
     +             ICOMEL, II, JMA, JNCMP, JNOMA, JVALV, K, N1, NBAP,
     +             NBET, NBMA, NBMO1, NBVARI, NC1, NC2,
     +             NCOMEL, NS1
      REAL*8       RBID
      COMPLEX*16   CBID

C  DIMAKI = DIMENSION MAX DE LA LISTE DES RELATIONS KIT

      PARAMETER (DIMAKI=9)
      PARAMETER (NCMPMA=7+DIMAKI)
      LOGICAL      EXIST, GETEXM
      CHARACTER*8  NOMA,NOMGRD,NOMCMP(NCMPMA),K8B,TYPMCL(2)
      CHARACTER*16 COMP,DEFO,MOCLEF(2),K16BID,NOMCMD,MOCLES(2)
      CHARACTER*16 VALCMP(NCMPMA),TXCP
      CHARACTER*19 COMPOR
      CHARACTER*24 LIGRMO, MODELE, MESMAI

C  POUR COMPORTEMENT KIT_

      INTEGER      NBVEL(DIMAKI)
      CHARACTER*16 COMEL(DIMAKI)

      DATA NOMGRD/'COMPOR  '/
      DATA NOMCMP/'RELCOM  ','NBVARI  ','DEFORM  ','INCELA  ',
     +            'C_PLAN  ','XXXX1','XXXX2','KIT1    ',
     +            'KIT2    ','KIT3    ','KIT4    ','KIT5    ',
     +            'KIT6    ','KIT7    ','KIT8    ','KIT9    '/
C     ------------------------------------------------------------------

      CALL JEMARQ()
C
      CALL GETRES ( K8B, K16BID, NOMCMD )
C
C                           1234567890123
      IF ( NOMCMD(1:13).NE.'THER_LINEAIRE') THEN
C
      COMPOR = '&&NMDORC.COMPOR'
      MODELE = MODELZ
C
      IF (NOMCMD(1:13).EQ.'THER_NON_LINE') THEN
         NBMO1 = 1
         MOCLEF(1) = 'COMP_THER_NL'
      ELSEIF (NOMCMD(1:9).EQ.'CALC_META') THEN
         NBMO1 = 1
         MOCLEF(1) = 'COMP_INCR'
      ELSE
         NBMO1 = 2
         MOCLEF(1) = 'COMP_INCR'
         MOCLEF(2) = 'COMP_ELAS'
      ENDIF
C
      MOCLES(1) = 'GROUP_MA'
      MOCLES(2) = 'MAILLE'
      TYPMCL(1) = 'GROUP_MA'
      TYPMCL(2) = 'MAILLE'
      MESMAI = '&&NMDORC.MES_MAILLES'
C
      LIGRMO = MODELE(1:8)//'.MODELE'
      CALL JEVEUO(LIGRMO(1:19)//'.NOMA','L',JNOMA)
      NOMA = ZK8(JNOMA)
C
C     POUR LES COMMANDES CALC_G_THETA_T ET CALC_G_LOCAL_T, SI AUCUN DES
C     DEUX COMPORTEMENTS COMP_ELAS ET COMP_INCR N'EST SPECIFIE PAR
C     L'UTILISATEUR, ON CREE UNE CARTE PAR DEFAUT AVEC LES
C     CARACTERISTIQUES SUIVANTES :
C          COMP_ELAS ( RELATION    : ELAS
C                      DEFORMATION : PETIT
C                      TOUT        : OUI
C                    )
C
      IF ((NOMCMD .EQ. 'CALC_G_THETA_T') .OR.
     &    (NOMCMD .EQ. 'CALC_G_LOCAL_T')) THEN
        CALL GETFAC('COMP_INCR', N2)
        CALL GETFAC('COMP_ELAS', N3)
        IF ((N2 .EQ. 0) .AND. (N3 .EQ. 0)) THEN
          VALCMP(1)='ELAS'
          VALCMP(2)='1'
          VALCMP(3)='PETIT'
          VALCMP(4)='COMP_ELAS'
          VALCMP(5)='??'
          VALCMP(6)='0'
          VALCMP(7)='0'
          CALL MECACT( 'V', COMPOR,'MAILLA', NOMA, NOMGRD, NCMPMA,
     &                 NOMCMP, IBID, RBID, CBID, VALCMP )
          GOTO 9999
        ENDIF
      ENDIF
C
      NBAP = 0
      NBET = 0
      DO 10 I = 1 , NBMO1
         CALL GETFAC ( MOCLEF(I), NBOCC )
         NBAP = NBAP + NBOCC
         DO 12 K = 1 , NBOCC
            CALL RELIEM(MODELE, NOMA, 'NU_MAILLE', MOCLEF(I), K, 2,
     +                                   MOCLES, TYPMCL, MESMAI, NBMA )
            IF ( NBMA .NE. 0 )  CALL JEDETR ( MESMAI )
            NBET = NBET + NBMA
 12      CONTINUE
 10   CONTINUE
C
      CALL ALCART ( 'V', COMPOR, NOMA, NOMGRD, NBAP+1, NBET )
C
      CALL JEVEUO ( COMPOR//'.NCMP', 'E', JNCMP )
      CALL JEVEUO ( COMPOR//'.VALV', 'E', JVALV )
C
      DO 60 ICMP = 1,NCMPMA
         ZK8(JNCMP+ICMP-1) = NOMCMP(ICMP)
   60 CONTINUE

C     ------------------------------------------------------------------

C                       REMPLISSAGE DE LA CARTE :

C     ------------------------------------------------------------------

      DO 180 I = 1,NBMO1

        CALL GETFAC(MOCLEF(I),NBOCC)

        DO 170 K = 1,NBOCC

          NBVARI = 0
          TXCP   = 'ANALYTIQUE      '
          DEFO   = ' '
          DO 110 ICOMEL = 1,DIMAKI
            NBVEL(ICOMEL) = 0
  110     CONTINUE

          CALL GETVTX(MOCLEF(I),'RELATION',K,1,1,COMP,N1)
          IF ( COMP(1:4) .EQ. 'KIT_' ) THEN

C  POUR COMPORTEMENT KIT_

            CALL GETVTX(MOCLEF(I),'RELATION_KIT',K,1,DIMAKI,COMEL(1),N1)
            IF (N1.EQ.0) THEN
              CALL UTMESS('F','NMDORC','LISTE RELATION_KIT VIDE')
            ELSE IF (N1.GT.DIMAKI) THEN
              CALL UTMESS('F','NMDORC','LISTE RELATION_KIT TROP LONGUE')
            ELSE
              NCOMEL = N1
              DO 120 II = NCOMEL + 1,DIMAKI
                COMEL(II) = '        '
  120         CONTINUE
            END IF
            DO 130 ICOMEL = 1,NCOMEL
              EXIST = GETEXM( MOCLEF(I), COMEL(ICOMEL) )
              IF (EXIST) THEN
                CALL GETVIS(MOCLEF(I),COMEL(ICOMEL),K,1,1,NBVEL(ICOMEL),
     +                      N1)
                NBVARI = NBVARI + NBVEL(ICOMEL)
              END IF
  130       CONTINUE
          ELSEIF ( COMP(1:4) .EQ. 'META' ) THEN
            EXIST = GETEXM( MOCLEF(I), COMP )
            IF (EXIST) THEN
              CALL GETVIS ( MOCLEF(I), COMP, K,1,1, NBVARI, N1 )
              CALL GETVTX ( MOCLEF(I),'RELATION_KIT', K,1,DIMAKI,
     +                                                  COMEL(1), N1 )
              IF (N1.EQ.0) THEN
                CALL UTMESS('F','NMDORC','LISTE RELATION_KIT VIDE')
              ELSE IF (N1.GT.1) THEN
                CALL UTMESS('F','NMDORC','LISTE RELATION_KIT TROP')
              ELSE
                NCOMEL = N1
                DO 122 II = NCOMEL + 1,DIMAKI
                  COMEL(II) = '        '
  122           CONTINUE
              END IF
              EXIST = GETEXM( MOCLEF(I), COMEL(1) )
              IF (EXIST) THEN
                CALL GETVIS(MOCLEF(I),COMEL(1),K,1,1,NBVEL(1),N1)
                NBVARI = NBVARI*NBVEL(1)+NBVARI+1
              END IF
            END IF
          ELSE
            EXIST = GETEXM( MOCLEF(I), COMP )
            IF (EXIST) THEN
              CALL GETVIS(MOCLEF(I),COMP,K,1,1,NBVARI,N1)

              EXIST = GETEXM(MOCLEF(I),'ALGO_C_PLAN')
              IF (EXIST) THEN
                CALL GETVTX(MOCLEF(I),'ALGO_C_PLAN',K,1,1,TXCP,N1)
                IF (TXCP.EQ.'DEBORST')  NBVARI=NBVARI+4
              END IF
            END IF
          END IF

C RELATION SIMO_MIEHE POUR VMIS_ISOT_XXX ET META_XXX_IL
C ET META_XXX_INL

          EXIST = GETEXM(MOCLEF(I),'DEFORMATION')
          IF (EXIST) THEN
           CALL GETVTX(MOCLEF(I),'DEFORMATION',K,1,1,DEFO,N1)
           IF (DEFO.EQ.'SIMO_MIEHE')  THEN
            IF (COMP(1:9).EQ.  'META_P_IL' .OR.
     &         COMP(1:10).EQ. 'META_P_INL'.OR.
     &         COMP(1:9) .EQ. 'META_V_IL' .OR.
     &         COMP(1:10).EQ. 'META_V_INL') THEN
             NBVARI=NBVARI+1
            ENDIF
            IF (COMP(1:9) .EQ. 'VMIS_ISOT') THEN
             CALL GETVIS(MOCLEF(I),COMP,K,1,1,NBVARI,N1)
             NBVARI=NBVARI+1
            ENDIF            
           ENDIF
          ENDIF

C           
          EXIST = GETEXM(MOCLEF(I),'DEFORMATION')
          IF (EXIST) THEN
            CALL GETVTX(MOCLEF(I),'DEFORMATION',K,1,1,DEFO,N1)
          END IF

          ZK16(JVALV-1+1) = COMP
          WRITE (ZK16(JVALV-1+2),'(I16)') NBVARI
          ZK16(JVALV-1+3) = DEFO
          ZK16(JVALV-1+4) = MOCLEF(I)
          ZK16(JVALV-1+5) = TXCP
          ZK16(JVALV-1+6) = ' '
          ZK16(JVALV-1+7) = ' '

C  POUR COMPORTEMENT KIT_

          DO 140 ICOMEL = 1,DIMAKI
            IF ((COMP(1:4).EQ.'KIT_') .OR.(COMP(1:4) .EQ. 'META')) THEN
              ZK16(JVALV-1+ICOMEL+7) = COMEL(ICOMEL)
            ELSE
              ZK16(JVALV-1+ICOMEL+7) = '        '
            END IF
  140     CONTINUE
C
          CALL RELIEM(MODELE, NOMA, 'NU_MAILLE',MOCLEF(I),K,2,MOCLES,
     +                                         TYPMCL, MESMAI, NBMA )
          IF ( NBMA .NE. 0 ) THEN
             CALL JEVEUO ( MESMAI, 'L', JMA )
             CALL NOCART ( COMPOR, 3, K8B, 'NUM', NBMA, K8B,
     +                                           ZI(JMA), ' ', NCMPMA )
             CALL JEDETR ( MESMAI )
          ELSE
C ------- PAR DEFAUT C'EST TOUT='OUI'
C            CALL GETVTX ( MOCLEF(I), 'TOUT'  , K,1,1, OUI   , NT )
             CALL NOCART ( COMPOR,1,K8B,K8B,0,K8B,IBID,K8B,NCMPMA)
          ENDIF

  170   CONTINUE

  180 CONTINUE
 9999 CONTINUE

      CALL JEDETR(COMPOR//'.NCMP')
      CALL JEDETR(COMPOR//'.VALV')

      COMPOZ = COMPOR
C
      ENDIF
C
C FIN ------------------------------------------------------------------
      CALL JEDEMA()
      END
