      SUBROUTINE LECTGD(FLIGN,LLIGN)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CATAELEM  DATE 26/11/97   AUTEUR D6BHHBQ B.QUINNEZ 
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
C TOLE CRP_20
      IMPLICIT REAL*8 (A-H,O-Z)
C
C     INCLUDE($CIMPERR)
C
      COMMON /CIMP/IMP,IULMES,IULIST,IULVIG
C
C     EXCLUDE($CIMPERR)
C
C
C     VARIABLES LOCALES
C
      CHARACTER*8 GD
      CHARACTER*72 CVAL
      CHARACTER*8 BLANC
C     INCLUDE($FUNJEV)
C
C     FONCTIONS JEVEUX
C
      CHARACTER*32 JEXNUM,JEXNOM,JEXATR
C
C     COMMUNS   JEVEUX
C
      CHARACTER*6 PGC
      COMMON /NOMAJE/PGC
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C
C     EXCLUDE($FUNJEV)
      LOGICAL PASS1,PASS2
C
      INTEGER FLIGN,LLIGN
      CHARACTER*80 LIGNE
C
C     ETATS
C
      INTEGER INIT,FIN
C
C     LABELS
C
      INTEGER TRANS,SORTIE,ERRGRA,ERRCON,PASSE
C
C
C     INCLUDE($CMOTGD)
      CHARACTER*24 CLEGD
      COMMON /CMTGDN/ NCLEGD
      COMMON /CMTGDC/ CLEGD(3)
C     EXCLUDE($CMOTGD)
C     INCLUDE($CDEBUG)
      CHARACTER*8 CLEDBG
      CHARACTER*24 OBJDMP
      INTEGER PASDMP,TYOBDM
      COMMON /CMODBG/CLEDBG
      COMMON /CDEBUG/ICCDBG
      COMMON /CBDMPC/OBJDMP(30)
      COMMON /CBDMPN/NDMP,PASDMP(30),TYOBDM(30)
C
C       NDMP : NOMBRE D OBJETS A DUMPER
C       PASDMP(IDMP)  : PASSE OU ON DUMPE L OBJET IDMP
C       OBJDMP(IDMP)  : NOM DE L OBJET IDMP
C       TYOBDM(IDMP)  : GENRE DE L OBJET IDMP :  0 OBJET SIMPLE
C                                                1 COLLECTION NUMEROTEE
C                                                2 COLLECTION NOMME
C
C     EXCLUDE($CDEBUG)
C
C
C     INCLUDE($EXTF)
C     EXCLUDE($EXTF)
C
C     VARIABLES FORTRANS POUR LES OBJETS
C
C     INCLUDE($OCATGD)
C
C     TABLEAUX FORTRAN CORRESPONDANTS AUX OBJETS DU CATALOGUE DES
C     GRANDEURS
C
C-----------------------------------------------------------------------
C  OBJET          ! TYPE JEVEUX         ! TABLEAU FTN !FONCTION D ACCES!
C-----------------------------------------------------------------------
C                 !                     !             !                !
C &CATA.GD.NOMGD  ! V(K8) REPERTOIRE    ! NOMGD       !                !
C                 !                     !             !                !
C-----------------!---------------------!-------------!----------------!
C                 !                     !             !                !
C &CATA.GD.LNOCMP ! V(IS)               ! LNOCMP      ! IDLNCP(IGD)    !
C                 !                     !             !                !
C-----------------!---------------------!-------------!----------------!
C                 !                     !             !
C &CATA.GD.NOMCMP ! C(V(K8)) POINTEE PAR! NOMCMP      ! IDCMPI(IGD,J)  !
C                 !         NOMGD(NOMS) !             !                !
C-----------------!---------------------!-------------!----------------!
C                 !                     !             !                !
C &CATA.GD.TYPEGD ! V(K8)               ! TYPEGD      ! IDTGD(I)       !
C                 !                     !             !                !
C-----------------!---------------------!-------------!----------------!
C                 !                     !             ! IDDGDI(IGD,J)  !
C &CATA.GD.DESCRIGD   C(V(IS)) POINTEE PAR! DGD       ! IDDGDK(GD,J)   !
C-----------------!---------------------!-------------!----------------!
      INTEGER DGD,LNOCMP
      INTEGER NOMGD,NOMCMP,TYPEGD
C
      COMMON /COCAGD/DGD(2),LNOCMP(2),NOMGD(2),NOMCMP(2),TYPEGD(2)
C
C     EXCLUDE($OCATGD)
C
C     MOTS DU LANGAGE UTILES DANS CE PGM
C
      CHARACTER*4 TYPM(2)
C
C  FONCTIONS FORMULES ( ADRESSAGE )
C
C     INCLUDE($ADCATGD)
C
C     ADRESSES DANS LES OBJETS DU CATALOGUE  DES GRANDEURS
C
      IDTGD(I) = IDVECT('&CATA.GD.TYPEGD',TYPEGD,I)
      IDCMPI(I,J) = IDCVNU('&CATA.GD.NOMCMP',NOMCMP,I,J)
      IDDGDI(I,J) = IDCVNU('&CATA.GD.DESCRIGD',DGD,I,J)
      IDDGDK(GD,J) = IDCVNO('&CATA.GD.DESCRIGD','&CATA.GD.NOMGD',
     &                       DGD,GD,J)
      IDLNCP(IGD) = IDVECT('&CATA.GD.LNOCMP',LNOCMP,IGD)
C
C     EXCLUDE($ADCATGD)
C
      ASSIGN 50 TO PASSE
      ASSIGN 100 TO TRANS
      ASSIGN 200 TO SORTIE
      ASSIGN 900 TO ERRGRA
      ASSIGN 901 TO ERRCON
C
      TYPM(1) = 'MS'
      TYPM(2) = 'MR'
      NCLEGD = 3
      CLEGD(1) = 'GRANDEURS_1ERE'
      CLEGD(2) = 'GRANDEURS_2EME_MEMBRE'
      CLEGD(3) = 'GRANDEURS_ELEMENTAIRES'
C
      CALL LXPOSI(FLIGN,1,0)
      CALL NGDDEF(NGD)
C
      IF (NGD.LE.0) THEN
        WRITE (IULMES,*) ' NOMBRE DE GRANDEURS NULLES '
        GO TO ERRGRA

      END IF
C
C
C
      IF (ICCDBG.GT.0) THEN
        WRITE (IULMES,*) ' % NOMBRE DE GD DANS LE CATALOGUE ',NGD
      END IF
C
      IPASSE = 0
   50 CONTINUE
      IPASSE = IPASSE + 1
      IF (IPASSE.EQ.1) THEN
        PASS1 = .TRUE.
        PASS2 = .FALSE.

      ELSE IF (IPASSE.EQ.2) THEN
        PASS1 = .FALSE.
        PASS2 = .TRUE.
      END IF
C
      IF (PASS1) THEN
C
        CALL JECREO('&CATA.GD.LNOCMP',' V V I ')
        CALL JEECRA('&CATA.GD.LNOCMP','LONMAX',NGD+1,' ')
        LONG = 1
        ZI(IDLNCP(1)) = LONG
C
        CALL JECREO('&CATA.GD.NOMGD',' G N K8 ')
        CALL JEECRA('&CATA.GD.NOMGD','NOMMAX',NGD,' ')
C
        CALL JECREC('&CATA.GD.NOMCMP','G V K8 ','NO',
     +              'CONTIG','VARIABLE',NGD)
C
        CALL JECREO('&CATA.GD.TYPEGD',' G V K8 ')
        CALL JEECRA('&CATA.GD.TYPEGD','LONMAX',NGD,' ')
C
C
        CALL JECREC('&CATA.GD.DESCRIGD','G V I ','NU',
     +              'CONTIG','CONSTANT',NGD)
        CALL JEECRA('&CATA.GD.DESCRIGD','LONT',5*NGD,' ')
C
      ELSE IF (PASS2) THEN
C
C   CONVERSION DE FORMAT POUR LES POINTEURS : CUMULE -> LONGUEUR
        CALL CHFPNT('LNOCMP',ZI(IDLNCP(1)),NGD)
        CALL JEVEUO('&CATA.GD.LNOCMP','L',IBID)
        ITOTAL=0
        DO 777, KK=1,NGD
          ITOTAL=ITOTAL+ZI(IBID-1+KK)
777     CONTINUE
        CALL JEECRA('&CATA.GD.NOMCMP','LONT',ITOTAL,' ')
        DO 778, KK=1,NGD
          CALL JEECRA(JEXNUM('&CATA.GD.NOMCMP',KK),'LONMAX',
     &                ZI(IBID-1+KK),' ')
778     CONTINUE

C
C
      END IF
C
      CALL LXPOSI(FLIGN,1,0)
C
      NGD = 0
      IGD = 0
      INIT = 0
C
      CALL TRANGD(INIT,FIN,IVAL,CVAL)
C
  100 CONTINUE
      INIT = FIN
C
      IF ((INIT.EQ.4) .OR. (INIT.EQ.9) .OR. (INIT.EQ.12)) THEN
        IF (IVAL.EQ.0) THEN
          INIT = 0
        END IF

      ELSE IF (INIT.EQ.8) THEN
        IF (ICMP.EQ.NCMP) THEN
          IF (IGD.GE.NGD) THEN
            INIT = 0

          ELSE
            INIT = 4
          END IF

        END IF

      ELSE IF ((INIT.EQ.11) .OR. (INIT.EQ.15) .OR. (INIT.EQ.19)) THEN
        IF (IGD.EQ.NGD) THEN
          INIT = 0
        END IF

      END IF
C
      IF (NDMP.NE.0) CALL DMPOBJ(IPASSE,FIN,' FIN ')
      CALL TRANGD(INIT,FIN,IVAL,CVAL)
      IF (NDMP.NE.0) CALL DMPOBJ(IPASSE,FIN,' DEBUT ')
      IF (ICCDBG.GE.2) THEN
        WRITE (IMP,*) ' % ETAT ',FIN
      END IF

      GO TO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,
     +       23,24) FIN
C
    1 CONTINUE
      GO TO TRANS
C
    2 CONTINUE
      GO TO TRANS
C
    3 CONTINUE
      GO TO TRANS
C
    4 CONTINUE
      NGD = NGD + IVAL
      GO TO TRANS
C
    5 CONTINUE
      IGD = IGD + 1
      GD = CVAL(1:IVAL)
      IF (PASS1) THEN
        CALL JECROC(JEXNOM('&CATA.GD.NOMGD',GD))
        CALL JECROC(JEXNOM('&CATA.GD.NOMCMP',GD))

      ELSE IF (PASS2) THEN
        ZI(IDDGDI(IGD,1)) = 1
      END IF

      GO TO TRANS
C
    6 CONTINUE
      IF (PASS2) THEN
        ZK8(IDTGD(IGD)) = CVAL(1:IVAL)
      END IF

      GO TO TRANS
C
    7 CONTINUE
      NCMP = IVAL
      ICMP = 0
      IF (PASS1) THEN
        LONG = LONG + NCMP
        ZI(IDLNCP(IGD+1)) = LONG

      ELSE IF (PASS2) THEN
C
C ACTUELLEMENT INUTILE ET IMPOSSIBLE REDEFINIR LONMAX SUR UN OBJET
C QUAND LE POINTEUR DE LONGUEUR A ETE DEFINI
C

C
        ZI(IDDGDI(IGD,3)) = (NCMP-1)/30 + 1
C
      END IF

      GO TO TRANS
C
    8 CONTINUE
      ICMP = ICMP + 1
      IF (PASS2) THEN
        ZK8(IDCMPI(IGD,ICMP)) = CVAL(1:IVAL)
      END IF

      GO TO TRANS
C
    9 CONTINUE
      NGD = NGD + IVAL
      GO TO TRANS
C
   10 CONTINUE
      IGD = IGD + 1
      GD = CVAL(1:IVAL)
      IF (PASS1) THEN
        CALL JECROC(JEXNOM('&CATA.GD.NOMGD',GD))
        CALL JECROC(JEXNOM('&CATA.GD.NOMCMP',GD))

      ELSE IF (PASS2) THEN
        ZI(IDDGDI(IGD,1)) = 2
      END IF

      GO TO TRANS
C
   11 CONTINUE
      IF (PASS1) THEN
        CALL JENONU(JEXNOM('&CATA.GD.NOMGD',CVAL(1:IVAL)),IGD1)
        IF (IGD1.GT.0) THEN
          NCMP1 = ZI(IDLNCP(IGD1+1)) - ZI(IDLNCP(IGD1))
          LONG = LONG + NCMP1
          ZI(IDLNCP(IGD+1)) = LONG

        ELSE
          GO TO ERRCON

        END IF

      ELSE IF (PASS2) THEN
        CALL JENONU(JEXNOM('&CATA.GD.NOMGD',CVAL(1:IVAL)),IGD1)
        IF (IGD1.GT.0) THEN
          ZI(IDDGDI(IGD,2)) = IGD1
          ZI(IDDGDI(IGD,3)) = ZI(IDDGDI(IGD1,3))
          ZK8(IDTGD(IGD)) = ZK8(IDTGD(IGD1))
          CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',IGD1),'LONMAX',NCMP1,
     +                BLANC)
C
C ACTUELLEMENT INUTILE ET IMPOSSIBLE REDEFINIR LONMAX SUR UN OBJET
C QUAND LE POINTEUR DE LONGUEUR A ETE DEFINI
C

          DO 110 I = 1,NCMP1
          ZK8(IDCMPI(IGD,I)) = ZK8(IDCMPI(IGD1,I))
  110     CONTINUE

        ELSE
          GO TO ERRCON

        END IF

      END IF

      GO TO TRANS
C
   12 CONTINUE
      NGD = NGD + IVAL
      GO TO TRANS
C
   13 CONTINUE
      IGD = IGD + 1
      GD = CVAL(1:IVAL)
      IF (PASS1) THEN
        CALL JECROC(JEXNOM('&CATA.GD.NOMGD',GD))
        CALL JECROC(JEXNOM('&CATA.GD.NOMCMP',GD))
        ZI(IDLNCP(IGD+1)) = LONG
      END IF

      GO TO TRANS
C
   14 CONTINUE
      IF (PASS2) THEN
        ZI(IDDGDK(GD,1)) = 3
      END IF

      GO TO TRANS
C
   15 CONTINUE
      IF (PASS2) THEN
        CALL JENONU(JEXNOM('&CATA.GD.NOMGD',CVAL(1:IVAL)),IGD1)
        IF (IGD1.GT.0) THEN
          ZI(IDDGDI(IGD,4)) = IGD1
          ZK8(IDTGD(IGD)) = ZK8(IDTGD(IGD1))

        ELSE
          GO TO ERRCON

        END IF

      END IF

      GO TO TRANS
C
   16 CONTINUE
      GO TO TRANS
C
   17 CONTINUE
      IF (PASS2) THEN
        CALL JENONU(JEXNOM('&CATA.GD.NOMGD',CVAL(1:IVAL)),IGD1)
        IF (IGD1.GT.0) THEN
          ZI(IDDGDI(IGD,4)) = IGD1

        ELSE
          GO TO ERRCON

        END IF

      END IF

      GO TO TRANS
C
   18 CONTINUE
      IF (PASS2) THEN
        CALL JENONU(JEXNOM('&CATA.GD.NOMGD',CVAL(1:IVAL)),IGD2)
        IF (IGD2.GT.0) THEN
          ZI(IDDGDI(IGD,5)) = IGD2

        ELSE
          GO TO ERRCON

        END IF

      END IF

      GO TO TRANS
C
   19 CONTINUE
      IF (PASS2) THEN
        IF (CVAL(1:IVAL).EQ.TYPM(1)) THEN
          ZI(IDDGDI(IGD,1)) = 4

        ELSE IF (CVAL(1:IVAL).EQ.TYPM(2)) THEN
          ZI(IDDGDI(IGD,1)) = 5
        END IF

        IF (ZK8(IDTGD(IGD2)).EQ.ZK8(IDTGD(IGD1))) THEN
          ZK8(IDTGD(IGD)) = ZK8(IDTGD(IGD1))

        ELSE
          GO TO ERRCON

        END IF

      END IF

      GO TO TRANS
C
   20 CONTINUE
      IF (IVAL.EQ.0) THEN
        GO TO SORTIE

      ELSE
        GO TO ERRGRA

      END IF
C
   21 CONTINUE
      IF (IGD.EQ.NGD) THEN
        GO TO SORTIE

      ELSE
        GO TO ERRGRA

      END IF
C
   22 CONTINUE
      IF ((IVAL.EQ.0) .AND. (IGD.EQ.NGD)) THEN
        GO TO SORTIE

      ELSE
        GO TO ERRGRA

      END IF
C
   23 CONTINUE
      IF ((ICMP.EQ.NCMP) .AND. (IGD.EQ.NGD)) THEN
        GO TO SORTIE

      ELSE
        GO TO ERRGRA

      END IF
C
   24 CONTINUE
      GO TO SORTIE
C
  200 CONTINUE
      IF (ICCDBG.GT.0) THEN
        WRITE (IMP,*) ' % OK PASSE ',IPASSE
      END IF

      IF (PASS1) THEN
        IF (ICCDBG.GT.0) THEN
          WRITE (IMP,*) '  % NGD = ',NGD,' LONG (NOMCMP)  = ',LONG
        END IF

        GO TO PASSE

      ELSE
CCCC   WRITE(IULMES,*) ' % FIN CATALOGUE DES GRANDEURS '
C
        CALL LXINFO(LIGNE,LLIGN,JCOL)
C
        GOTO 9999

      END IF

  900 CONTINUE
      CALL UTMESS('F','LECTGD','PB. LECTURE CATALOGUES.')
      WRITE (IULMES,*) ' %  ERREUR GRANDEURS GRAMMAIRE '
      GOTO 9999

  901 CONTINUE
      CALL UTMESS('F','LECTGD','PB. LECTURE CATALOGUES.')
      WRITE (IULMES,*) ' %  ERREUR GRANDEURS CONTEXT '
C
 9999 CONTINUE

      CALL JEDETR('&CATA.GD.LNOCMP')
      END
