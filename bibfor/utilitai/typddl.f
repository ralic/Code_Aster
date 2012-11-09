      SUBROUTINE TYPDDL(CHOIXZ,NUMEZ,NEQ,TABDDL,NBACTI,NBBLOQ,NBLAGR,
     &                  NBLIAI)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
      INTEGER           NEQ,TABDDL(*),NBACTI,NBBLOQ,NBLAGR,NBLIAI
      CHARACTER*4       CHOIX
      CHARACTER*14            NUME
      CHARACTER*(*)     CHOIXZ, NUMEZ
C----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
C----------------------------------------------------------------------
C
C     DETERMINATION DU TYPE DES DDL
C     SI CHOIX = 'ACTI' : TABDDL(I) = 1 ,  DDL I ACTIF
C                                   = 0 ,  DDL I BLOQUE OU LAGRANGE
C     SI CHOIX = 'BLOQ' : TABDDL(I) = 1 ,  DDL I BLOQUE
C                                   = 0 ,  DDL I ACTIF OU LAGRANGE
C     SI CHOIX = 'LAGR' : TABDDL(I) = 1 ,  DDL I LAGRANGE
C                                   = 0 ,  DDL I ACTIF OU BLOQUE
C     SI CHOIX = 'BLLA' : TABDDL(I) = 0 ,  DDL I ACTIF
C                                   = 1 ,  DDL I BLOQUE OU LAGRANGE
C     SI CHOIX = 'ACLA' : TABDDL(I) = 0 ,  DDL I BLOQUE
C                                   = 1 ,  DDL I ACTIF OU LAGRANGE
C     SI CHOIX = 'ACBL' : TABDDL(I) = 0 ,  DDL I LAGRANGE
C                                   = 1 ,  DDL I ACTIF OU BLOQUE
C
C----------------------------------------------------------------------
C IN  CHOIX  : K : CHOIX DE LA SORTIE DU TABLEAU TABDDL
C IN  NUME   : K : NOM DE LA NUMEROTATION
C IN  NEQ    : I : NOMBRE D' EQUATIONS
C OUT TABDDL : I : TABLEAU DES TYPES
C OUT NBACTI : I : NOMBRE DE DDL ACTIF
C OUT NBBLOQ : I : NOMBRE DE DDL BLOQUE
C OUT NBLAGR : I : NOMBRE DE DDL LAGRANGE
C OUT NBLIAI : I : NOMBRE DE DDL LAGRANGE UTILISES POUR DES LIAISONS
C----------------------------------------------------------------------
C
C
      INTEGER      APRNO, ADEEQ, IDDL, IDEB, ND, N, NEC, IERD, GD,IER
      CHARACTER*8  MODGEN, BASEMO
      CHARACTER*16 TYPREP
      CHARACTER*24 NPRNO, NDEEQ, KBID, NORIG
      CHARACTER*8  K8B
      LOGICAL      EXISDG
C
C
C-----------------------------------------------------------------------
      INTEGER I ,IBID ,ICMP ,ICO ,J ,JORIG ,JPRNO
      INTEGER JREFE ,N1DDL ,N2DDL ,NBDEFO ,NBEC ,NBPRNO ,NBSST
      INTEGER NUSST
C-----------------------------------------------------------------------
      CALL JEMARQ()

      NBLIAI = 0
      CHOIX  = CHOIXZ
      NUME   = NUMEZ
      NPRNO = NUME//'.NUME.PRNO'
      NORIG = NUME//'.NUME.ORIG'
      NDEEQ = NUME//'.NUME.DEEQ'
C
      CALL JENONU(JEXNOM(NPRNO(1:19)//'.LILI','&MAILLA'),NBPRNO)
      CALL JEVEUO(NDEEQ,'L',ADEEQ)
      CALL DISMOI('F','NUM_GD_SI',NUME,'NUME_DDL',GD,KBID,IERD)
      NEC = NBEC(GD)
C
      IF (NBPRNO.NE.0) THEN
C
C     --- CONSTRUCTION D'UN VECTEUR D'ENTIERS TEL QUE  ---
C     --- = 1 DDL PHYSIQUE LIBRE OU BLOQUE PAR LIAISON ---
C     --- = 0 LAGRANGE                                 ---
C     --- = -1 DDL PHYSIQUE BLOQUE                     ---
C
      CALL JENONU(JEXNOM(NPRNO(1:19)//'.LILI','&MAILLA'),IBID)
      CALL JEVEUO(JEXNUM(NPRNO,IBID),'L',APRNO)
      DO 5  I = 1, NEQ
         TABDDL(I) = 1
 5    CONTINUE
      DO 10 I = 1, NEQ
         N = ZI(ADEEQ + 2*I-1)
         IF ( N .EQ. 0 ) THEN
            NBLIAI = NBLIAI + 1
            TABDDL(I) = 0
         ELSE IF ( N .LT. 0 ) THEN
            TABDDL(I) = 0
            ND   = ZI(ADEEQ + 2*I-2)
            IDEB = ZI(APRNO + (NEC+2)*(ND-1) + 1-1)
            ICO = 0
            DO 15 ICMP = 1, -N - 1
               IF (EXISDG(ZI(APRNO+(NEC+2)*(ND-1)+ 3-1),ICMP)) THEN
                  ICO = ICO + 1
               ENDIF
 15         CONTINUE
            IDDL = IDEB + ICO
            TABDDL(IDDL) = -1
         ENDIF
 10   CONTINUE
C
      ELSE
C
C CAS DE LA NUMEROTATION GENERALISEE
C
      DO 11 I=1,NEQ
         N=ZI(ADEEQ+2*I-1)
         IF(N.GT.0) THEN
            TABDDL(I)=I
         ELSE
            TABDDL(I)=0
         ENDIF
11    CONTINUE
C
      CALL JEVEUO(NUME//'.NUME.REFN','L',JREFE)
      CALL GETTCO(ZK24(JREFE),TYPREP)
      IF (TYPREP.EQ.'MODELE_GENE     ') THEN
        MODGEN = ZK24(JREFE)
        CALL JENONU(JEXNOM(NORIG(1:19)//'.LILI','&SOUSSTR'),IBID)
        CALL JELIRA(JEXNUM(NORIG,IBID),'LONMAX',NBSST,K8B)
C On compte que si il y a plus d'une sous-structure
        IF (NBSST.GT.2) THEN
          CALL JEVEUO(JEXNUM(NORIG,IBID),'L',JORIG)
          CALL JEVEUO(JEXNUM(NPRNO,IBID),'L',JPRNO)
          DO 23 I=1,NBSST
            NUSST = ZI(JORIG-1+I)
            KBID = '        '
            CALL MGUTDM(MODGEN,KBID,NUSST,'NOM_BASE_MODALE',IBID,
     &                 BASEMO)
            CALL DISMOI('F','NB_MODES_STA',BASEMO,'RESULTAT',
     &                      NBDEFO,KBID,IER)
            N1DDL = ZI(JPRNO+2*(I-1))+ZI(JPRNO+2*(I-1)+1)-NBDEFO
            N2DDL = ZI(JPRNO+2*(I-1))+ZI(JPRNO+2*(I-1)+1)-1
            DO 24 J=N1DDL,N2DDL
              TABDDL(J)=-J
24          CONTINUE
23        CONTINUE
        ENDIF
      ENDIF
C
      ENDIF
C
C
      NBACTI = 0
      NBBLOQ = 0
      NBLAGR = 0
      IF ( CHOIX.EQ.'ACTI') THEN
         DO 12 I = 1, NEQ
            N = TABDDL(I)
            IF ( N .GT. 0 ) THEN
               NBACTI = NBACTI + 1
               TABDDL(I) = 1
            ELSEIF ( N .EQ. 0 ) THEN
               NBLAGR = NBLAGR + 1
               TABDDL(I) = 0
            ELSE
               NBBLOQ = NBBLOQ + 1
               TABDDL(I) = 0
            ENDIF
 12      CONTINUE
      ELSEIF ( CHOIX.EQ.'BLOQ') THEN
         DO 14 I = 1, NEQ
            N = TABDDL(I)
            IF ( N .GT. 0 ) THEN
               NBACTI = NBACTI + 1
               TABDDL(I) = 0
            ELSEIF ( N .EQ. 0 ) THEN
               NBLAGR = NBLAGR + 1
               TABDDL(I) = 0
            ELSE
               NBBLOQ = NBBLOQ + 1
               TABDDL(I) = 1
            ENDIF
 14      CONTINUE
      ELSE IF ( CHOIX .EQ. 'LAGR' ) THEN
         DO 16 I = 1, NEQ
            N = TABDDL(I)
            IF ( N .GT. 0 ) THEN
               NBACTI = NBACTI + 1
               TABDDL(I) = 0
            ELSEIF ( N .EQ. 0 ) THEN
               NBLAGR = NBLAGR + 1
               TABDDL(I) = 1
            ELSE
               NBBLOQ = NBBLOQ + 1
               TABDDL(I) = 0
            ENDIF
 16      CONTINUE
      ELSE IF ( CHOIX .EQ. 'ACBL' ) THEN
         DO 18 I = 1, NEQ
            N = TABDDL(I)
            IF ( N .GT. 0 ) THEN
               NBACTI = NBACTI + 1
               TABDDL(I) = 1
            ELSEIF ( N .EQ. 0 ) THEN
               NBLAGR = NBLAGR + 1
               TABDDL(I) = 0
            ELSE
               NBBLOQ = NBBLOQ + 1
               TABDDL(I) = 1
            ENDIF
 18      CONTINUE
      ELSE IF ( CHOIX .EQ. 'ACLA' ) THEN
         DO 20 I = 1, NEQ
            N = TABDDL(I)
            IF ( N .GT. 0 ) THEN
               NBACTI = NBACTI + 1
               TABDDL(I) = 1
            ELSEIF ( N .EQ. 0 ) THEN
               NBLAGR = NBLAGR + 1
               TABDDL(I) = 1
            ELSE
               NBBLOQ = NBBLOQ + 1
               TABDDL(I) = 0
            ENDIF
 20      CONTINUE
      ELSE IF ( CHOIX .EQ. 'BLLA' ) THEN
         DO 22 I = 1, NEQ
            N = TABDDL(I)
            IF ( N .GT. 0 ) THEN
               NBACTI = NBACTI + 1
               TABDDL(I) = 0
            ELSEIF ( N .EQ. 0 ) THEN
               NBLAGR = NBLAGR + 1
               TABDDL(I) = 1
            ELSE
               NBBLOQ = NBBLOQ + 1
               TABDDL(I) = 1
            ENDIF
 22      CONTINUE
      ELSE
         CALL U2MESK('F','UTILITAI5_3',1,CHOIX)
      ENDIF
C
      CALL JEDEMA()
C
      END
