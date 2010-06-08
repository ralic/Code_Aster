      SUBROUTINE UIMPBA(CLAS,IUNMES)
      IMPLICIT NONE
      CHARACTER*(*)       CLAS
      INTEGER IUNMES
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 10/11/2008   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
C BUT:
C   IMPRIMER LA TAILLE DES CONCEPTS STOCKES SUR UNE BASE
C
C  IN    CLAS  : NOM DE LA BASE : 'G', 'V', ..(' ' -> TOUTES LES BASES)
C ----------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
      CHARACTER*32       JEXNOM, JEXNUM,JEXATR
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      CHARACTER*8 K8
      CHARACTER*24 KBID,OBJ
      CHARACTER*16 TYPCON
      REAL*8    RLONG,MEGA,TAITOT
      INTEGER  I,NBOBJ,NBVAL,IEXI,NBCON,JTAILO,JTAILC,NBSV
      INTEGER  JLOBJ,JNBOBJ,JNBSVO,JNBSVC ,NSTOT


      MEGA=1024*1024


C     -- 1 : NBOBJ + .LISTE_OBJ :LISTE DES OBJETS :
C     ----------------------------------------------
      NBOBJ=0
      CALL JELSTC(CLAS,' ',0,NBOBJ,KBID,NBVAL)
      CALL ASSERT(NBVAL.LE.0)
      IF (NBVAL.EQ.0) GOTO 9999
      NBOBJ=-NBVAL
      CALL WKVECT('&&UIMPBA.LISTE_OBJ','V V K24',NBOBJ+1,JLOBJ)
      CALL JELSTC(CLAS,' ',0,NBOBJ,ZK24(JLOBJ),NBVAL)
C     NBVAL = NBOBJ (+1 EVENTUELLEMENT A CAUSE DE '&&UIMPBA.LISTE_OBJ')
      CALL ASSERT(NBVAL.EQ.NBOBJ+1 .OR. NBVAL.EQ.NBOBJ)
      NBOBJ=NBVAL

C     -- 2 : .TAILLE = TAILLE DES OBJETS :
C     --------------------------------------
      CALL WKVECT('&&UIMPBA.TAILLE','V V R',NBOBJ,JTAILO)
      CALL WKVECT('&&UIMPBA.NBSVO','V V I',NBOBJ,JNBSVO)
      DO 1, I=1,NBOBJ
        OBJ=ZK24(JLOBJ-1+I)
        CALL JELGDQ (OBJ,RLONG,NBSV)
        CALL ASSERT(RLONG.GT.0.D0)
        ZR(JTAILO-1+I)=RLONG
        ZI(JNBSVO-1+I)=NBSV
  1   CONTINUE


C     -- 3 : .LCONK8 = LISTE DES CONCEPTS (K8) DE .LISTE_OBJ
C     -----------------------------------------------------------
      CALL JECREO('&&UIMPBA.LCONK8','V N K8')
      CALL JEECRA('&&UIMPBA.LCONK8','NOMMAX',NBOBJ,KBID)
      DO 2, I=1,NBOBJ
        OBJ=ZK24(JLOBJ-1+I)
        K8=OBJ(1:8)
        CALL JENONU(JEXNOM('&&UIMPBA.LCONK8',K8),IEXI)
        IF (IEXI.EQ.0) THEN
          CALL JECROC(JEXNOM('&&UIMPBA.LCONK8',K8))
        ENDIF
  2   CONTINUE


C     -- 4 : .TAILCON = TAILLE DES CONCEPTS
C     -----------------------------------------------------------
      CALL JELIRA('&&UIMPBA.LCONK8','NOMUTI',NBCON,KBID)
      CALL WKVECT('&&UIMPBA.TAILCON','V V R',NBCON,JTAILC)
      CALL WKVECT('&&UIMPBA.NBSVC','V V I',NBCON,JNBSVC)
      CALL WKVECT('&&UIMPBA.NBOBJ','V V I',NBCON,JNBOBJ)
      TAITOT=0.D0
      NSTOT=0
      DO 3, I=1,NBOBJ
        OBJ=ZK24(JLOBJ-1+I)
        K8=OBJ(1:8)
        CALL JENONU(JEXNOM('&&UIMPBA.LCONK8',K8),IEXI)
        CALL ASSERT(IEXI.GT.0)
        CALL ASSERT(IEXI.LE.NBCON)
        ZR(JTAILC-1+IEXI)=ZR(JTAILC-1+IEXI)+ZR(JTAILO-1+I)
        TAITOT=TAITOT+ZR(JTAILO-1+I)
        ZI(JNBSVC-1+IEXI)=ZI(JNBSVC-1+IEXI)+ZI(JNBSVO-1+I)
        ZI(JNBOBJ-1+IEXI)=ZI(JNBOBJ-1+IEXI)+1
        NSTOT=NSTOT+ZI(JNBSVO-1+I)
  3   CONTINUE


C     -- 5 : IMPRESSION DU RESULTAT :
C     -----------------------------------------------------------
      WRITE(IUNMES,*) '-----------------------------------------------',
     &                '----------------------------'
      WRITE(IUNMES,*) 'Concepts de la base: ',CLAS
      WRITE(IUNMES,*) '   Nom       Type                 Taille (Mo)',
     &                '         Nombre      Nombre de'
      WRITE(IUNMES,*) '                                            ',
     &                '        d''objets       segments'

      WRITE(IUNMES,1000) 'TOTAL   ',' ',TAITOT/MEGA,NBOBJ,NSTOT
      WRITE(IUNMES,*) ' '

C     -- ON IMPRIME D'ABORD LES CONCEPTS UTILISATEUR :
      DO 4, I=1,NBCON
        CALL JENUNO(JEXNUM('&&UIMPBA.LCONK8',I),K8)
        CALL GETTCO(K8,TYPCON)
        IF (TYPCON.EQ.' ') GOTO 4
        WRITE(IUNMES,1000) K8,TYPCON,ZR(JTAILC-1+I)/MEGA,
     &        ZI(JNBOBJ-1+I),ZI(JNBSVC-1+I)
  4   CONTINUE
C     -- ON IMPRIME ENSUITE LES CONCEPTS CACHES  :
      DO 5, I=1,NBCON
        CALL JENUNO(JEXNUM('&&UIMPBA.LCONK8',I),K8)
        CALL GETTCO(K8,TYPCON)
        IF (TYPCON.NE.' ') GOTO 5
        WRITE(IUNMES,1000) K8,TYPCON,ZR(JTAILC-1+I)/MEGA,
     &        ZI(JNBOBJ-1+I),ZI(JNBSVC-1+I)
  5   CONTINUE
      WRITE(IUNMES,*) '-----------------------------------------------',
     &                '----------------------------'


9999  CONTINUE
      CALL JEDETR('&&UIMPBA.LISTE_OBJ')
      CALL JEDETR('&&UIMPBA.TAILLE')
      CALL JEDETR('&&UIMPBA.LCONK8')
      CALL JEDETR('&&UIMPBA.TAILCON')
      CALL JEDETR('&&UIMPBA.NBSVO')
      CALL JEDETR('&&UIMPBA.NBSVC')
      CALL JEDETR('&&UIMPBA.NBOBJ')
1000  FORMAT (4X,A8,3X,A16,3X,F12.2,3X,I12,3X,I12)
      END
