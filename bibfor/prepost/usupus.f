      SUBROUTINE USUPUS ( PUUSUR, KFORN, KVGLI, NBPT)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C-----------------------------------------------------------------------
C     CALCULE LA PUISSANCE D'USURE AU SENS D'ARCHARD
C                    PU  =  FN * VT
C
C OUT : PUUSUR : PUISSANCE USURE
C-----------------------------------------------------------------------
      INCLUDE 'jeveux.h'
      CHARACTER*8  K8B, NOEU
      CHARACTER*19 TRANGE, KFORN, KVGLI
      INTEGER      IARG
C
C-----------------------------------------------------------------------
      INTEGER I ,ICHOC ,IDEBUT ,IDIADH ,IDWK4 ,IFIN ,IFIRES 
      INTEGER IMPR ,IUNIFI ,J ,JDESC ,JFCHO ,JFN ,JINST 
      INTEGER JNCHO ,JVG ,JVGLI ,JWK1 ,JWK2 ,JWK3 ,LG 
      INTEGER LXLGUT ,N1 ,N2 ,N3 ,N4 ,NBCHOC ,NBLOC 
      INTEGER NBPAS ,NBPT ,NBVAL ,NT 
      REAL*8 PUUSUR ,TDEBUT ,TFIN ,TMAX ,TMIN 
C-----------------------------------------------------------------------
      CALL JEMARQ()
      IFIRES = IUNIFI('RESULTAT')
      NBPT = 0
      IMPR = 2
C
      CALL GETVR8(' ','PUIS_USURE',1,IARG,1,PUUSUR,N1)
      IF ( N1 .NE. 0 ) THEN
         CALL IMPUS ( IFIRES, 0, PUUSUR )
         GOTO 9999
      ENDIF
C
      CALL GETVID(' ','RESU_GENE',1,IARG,1,TRANGE,NT)
      IF ( NT .NE. 0 ) THEN
         CALL JEVEUO(TRANGE//'.DESC','L',JDESC)
         IF ( ZI(JDESC) .EQ. 2 .OR. ZI(JDESC) .EQ. 3 ) THEN
            NBCHOC = ZI(JDESC+2)
            CALL GETVIS(' ','NB_BLOC'   ,1,IARG,1,NBLOC,N1)
            IF ( N1 .EQ. 0 ) NBLOC = 1
            CALL GETVR8(' ','INST_INIT' ,1,IARG,1,TDEBUT,N2)
            CALL GETVR8(' ','INST_FIN'  ,1,IARG,1,TFIN  ,N3)
            CALL GETVTX(' ','NOEUD'     ,1,IARG,1,NOEU  ,N4)
C
            CALL JEVEUO(TRANGE//'.NCHO','L',JNCHO)
C           --- RECHERCHE DU NOEUD DE CHOC ---
            DO 10 ICHOC = 1,NBCHOC
               IF (ZK8(JNCHO+ICHOC-1).EQ.NOEU) GOTO 12
 10         CONTINUE
            LG = MAX(1,LXLGUT(NOEU))
            CALL U2MESK('F','UTILITAI_87',1,NOEU(1:LG))
            GOTO 9999
 12         CONTINUE
C
            CALL JEVEUO(TRANGE//'.INST','L',JINST)
            CALL JELIRA(TRANGE//'.INST','LONMAX',NBPT,K8B)
            TMAX = ZR(JINST+NBPT-1)
            TMIN = ZR(JINST)
            IF ( N2 .EQ. 0 ) THEN
               TDEBUT = TMIN
            ELSE
               IF ( TDEBUT .LT. TMIN ) TDEBUT = TMIN
            ENDIF
            IF ( N3 .EQ. 0 ) THEN
               TFIN = TMAX
            ELSE
               IF ( TFIN .GT. TMAX ) TFIN  = TMAX
            ENDIF
            IF ( TDEBUT .GE. TFIN ) THEN
               CALL U2MESS('F','PREPOST4_47')
            ENDIF
            DO 14 J=1,NBPT
             IF (ZR(JINST+J-1) .GE. TDEBUT) THEN
               IDEBUT = J
               GO TO 15
             ENDIF
 14         CONTINUE
 15         CONTINUE
            DO 16 J=1,NBPT
             IF (ZR(JINST+J-1) .GE. TFIN) THEN
               IFIN = J
               GO TO 17
             ENDIF
 16         CONTINUE
 17         CONTINUE
            NBPAS  = IFIN - IDEBUT + 1
            IF ( NBLOC.EQ.0 ) NBLOC = 1
            NBVAL = NBPAS / NBLOC
C
            CALL JEVEUO(TRANGE//'.FCHO','L',JFCHO)
            CALL JEVEUO(TRANGE//'.VCHO','L',JVGLI)
            CALL JEVEUO(TRANGE//'.ICHO','L',IDIADH)
C
            CALL WKVECT('&&USURPU.WK1','V V R',NBPT,JWK1)
            CALL WKVECT('&&USURPU.WK2','V V R',NBPT,JWK2)
            CALL WKVECT('&&USURPU.WK3','V V R',NBPT,JWK3)
            CALL WKVECT('&&USURPU.IWK4','V V I',NBPT,IDWK4)
C
            CALL STATPU ( NBCHOC,NBPT,ZR(JINST),ZR(JFCHO),ZR(JVGLI),
     &                    ZI(IDIADH),ZR(JWK1),ZR(JWK2),ZR(JWK3),
     &                    ZI(IDWK4),IDEBUT,NBLOC,
     &                    NBVAL,IFIRES,ICHOC,IMPR, PUUSUR )
C
            CALL WKVECT(KFORN,'V V R',NBPT,JFN)
            CALL WKVECT(KVGLI,'V V R',NBPT,JVG)
            CALL DCOPY(NBPT,ZR(JWK1),1,ZR(JFN),1)
            DO 20 I = 0 , NBPT-1
               ZR(JVG+I) = SQRT( ZR(JWK2+I)**2 + ZR(JWK3+I)**2 )
 20         CONTINUE
C
            CALL JEDETR('&&USURPU.WK1')
            CALL JEDETR('&&USURPU.WK2')
            CALL JEDETR('&&USURPU.WK3')
            CALL JEDETR('&&USURPU.IWK4')
         ELSE
            CALL U2MESS('F','PREPOST4_84')
         ENDIF
      ENDIF
C
 9999 CONTINUE
      CALL JEDEMA()
      END
