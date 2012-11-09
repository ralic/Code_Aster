      SUBROUTINE RBPH02(MAILLA,NUMDDL,CHAMNO,NOMGD,NEQ,NBNOEU,OBJVE1,
     &                    NCMP, OBJVE2, OBJVE3, OBJVE4 )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
      INTEGER             NBNOEU, NEQ
      CHARACTER*8         MAILLA
      CHARACTER*14        NUMDDL
      CHARACTER*(*)       CHAMNO
      CHARACTER*24        OBJVE1 , OBJVE2 , OBJVE3 , OBJVE4
C     ------------------------------------------------------------------
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C     ------------------------------------------------------------------
C     OPERATEUR REST_BASE_PHYS
C               TRAITEMENT DES MOTS CLES "NOEUD" ET "GROUP_NO"
C  IN MAILLA : NOM D'UN MAILLAGE
C  IN CHAMNO : NOM D'UN CHAM_NO  (OU ' ')
C  IN NUMDDL : NOM D'UN NUME_DDL (OU ' ')
C     REMARQUE : IL FAUT NUMDLL OU CHAMNO != ' '
C ----------------------------------------------------------------------
C
C     ------------------------------------------------------------------
      INTEGER       IBID, IE, JPRNO, NEC, TABEC(10), I, IDEC, INUDDL,
     &              IAD, IEC, INO, NCMPMX, JNOEU, NCMP, ICMP, NUNOE,
     &              JNEQ, JCMP, J, NBCMP
      LOGICAL       EXISDG
      CHARACTER*8   K8B, MOTCLS(4), TYPMCL(4), NOMGD, NOMNOE, NOMCMP
      CHARACTER*19  PRNO
C     ------------------------------------------------------------------
C
      MOTCLS(1) = 'GROUP_NO'
      MOTCLS(2) = 'NOEUD'
      MOTCLS(3) = 'GROUP_MA'
      MOTCLS(4) = 'MAILLE'
      TYPMCL(1) = 'GROUP_NO'
      TYPMCL(2) = 'NOEUD'
      TYPMCL(3) = 'GROUP_MA'
      TYPMCL(4) = 'MAILLE'
C
      CALL RELIEM(' ', MAILLA, 'NU_NOEUD', ' ', 1, 4,
     +                                  MOTCLS, TYPMCL, OBJVE1, NBNOEU )
      CALL JEVEUO ( OBJVE1, 'L', JNOEU )
C
      IF (NUMDDL.NE.' ') THEN
        CALL DISMOI('F','PROF_CHNO',NUMDDL,'NUME_DDL',IBID,PRNO,IE)
      ELSE
        CALL ASSERT(CHAMNO.NE.' ')
        CALL DISMOI('F','PROF_CHNO',CHAMNO,'CHAM_NO',IBID,PRNO,IE)
      ENDIF

      CALL DISMOI ('F', 'NB_EC'    ,NOMGD,'GRANDEUR' , NEC , K8B  ,IE )
      CALL DISMOI ('F','NB_CMP_MAX',NOMGD,'GRANDEUR',NCMPMX, K8B  ,IE )
      CALL ASSERT ( NEC .LE. 10 )
      CALL JEVEUO ( JEXNOM('&CATA.GD.NOMCMP',NOMGD), 'L', IAD )
      CALL JEVEUO ( JEXNUM(PRNO//'.PRNO',1), 'L', JPRNO )
C
      CALL WKVECT ( OBJVE2, 'V V K8', NCMPMX, JCMP )
C
      NEQ  = 0
      NCMP = 0
      DO 10 I = 1 , NBNOEU
         INO = ZI(JNOEU+I-1)
         DO 12 IEC = 1 , NEC
            TABEC(IEC)= ZI(JPRNO-1+(INO-1)*(NEC+2)+2+IEC )
 12      CONTINUE
         NBCMP = 0
         DO 14 ICMP = 1 , NCMPMX
            IF ( EXISDG(TABEC,ICMP) ) THEN
               NBCMP = NBCMP + 1
               DO 16 J = 1 , NCMP
                  IF ( ZK8(JCMP+J-1) .EQ. ZK8(IAD-1+ICMP) ) GOTO 14
 16            CONTINUE
               NCMP = NCMP + 1
               ZK8(JCMP-1+NCMP) = ZK8(IAD-1+ICMP)
            ENDIF
 14      CONTINUE
         NEQ = NEQ + NBCMP
 10   CONTINUE
C
      CALL WKVECT ( OBJVE3, 'V V I', NBNOEU*NCMP, JNEQ )
      CALL WKVECT ( OBJVE4, 'V V I', NEQ, INUDDL )
C
      IDEC = 0
      DO 20 I = 1 , NBNOEU
         INO = ZI(JNOEU+I-1)
         CALL JENUNO ( JEXNUM(MAILLA//'.NOMNOE',INO), NOMNOE )
         DO 22 IEC = 1 , NEC
            TABEC(IEC)= ZI(JPRNO-1+(INO-1)*(NEC+2)+2+IEC )
 22      CONTINUE
         DO 24 ICMP = 1 , NCMPMX
            IF ( EXISDG(TABEC,ICMP) ) THEN
               IDEC = IDEC + 1
               NOMCMP = ZK8(IAD-1+ICMP)
               IF (NUMDDL.NE.' ') THEN
                 CALL POSDDL('NUME_DDL',NUMDDL,NOMNOE,NOMCMP,
     &                        NUNOE, ZI(INUDDL+IDEC-1) )
               ELSE
                 CALL POSDDL('CHAM_NO',CHAMNO,NOMNOE,NOMCMP,
     &                                   NUNOE, ZI(INUDDL+IDEC-1) )
               ENDIF
               DO 26 J = 1 , NCMP
                  IF ( ZK8(JCMP+J-1) .EQ. ZK8(IAD-1+ICMP) ) THEN
                     ZI(JNEQ-1+(I-1)*NCMP+J) = 1
                     GOTO 24
                  ENDIF
 26            CONTINUE
            ENDIF
 24      CONTINUE
 20   CONTINUE
C
      END
