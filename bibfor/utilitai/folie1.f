      SUBROUTINE FOLIE1 ( NBFR, ABSC, ACCE, NBCREU, LCREU, ELARG, 
     +                    VALTG, VALTD, F1, F2, LISS, LARG, 
     +                    NBFLIS, ABSLIS, ACCLIS )
      IMPLICIT  NONE
      INTEGER             NBFR, NBCREU, NBFLIS
      REAL*8              ABSC(*), ACCE(*), LCREU(*), VALTG, VALTD, F1,
     +                    F2, LISS, LARG, ABSLIS(*), ACCLIS(*)
      CHARACTER*8         ELARG
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 18/09/2002   AUTEUR CIBHHLV L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C  IN : NBFR   : NOMBRE DE FREQUENCES DE LA FONCTION A TRAITER
C  IN : ABSC   : LISTE DES ABSCISSES DE LA FONCTION A TRAITER
C  IN : ACCE   : LISTE DES ORDONNEES DE LA FONCTION A TRAITER
C  IN : NBCREU : NOMBRE DE FREQUENCES DE CREUX A GARDER
C  IN : LCREU  : LISTE DES FREQUENCES DE CREUX
C  IN : ELARG  : ELARGISSEMENT EN FREQUENCE DU SPECTRE
C  IN : VALTG  : VALEUR DE L'ELARGISSEMENT A GAUCHE
C  IN : VALTD  : VALEUR DE L'ELARGISSEMENT A DROITE
C  IN : F1     : BORNE A GAUCHE POUR UN ELARGISSEMENT LOCAL
C  IN : F2     : BORNE A DROITE POUR UN ELARGISSEMENT LOCAL
C  IN : LISS   : CRITERE POUR L'ELIMINATION DES POINTS DE LISSAGE
C  IN : LARG   : SEUIL EN LARGEUR POUR SELECTIONNER LES PLATEAUX
C OUT : NBFLIS : NOMBRE DE FREQUENCES DE LA FONCTION LISSEE
C OUT : ABSLIS : LISTE DES ABSCISSES DE LA FONCTION LISSEE
C OUT : ACCLIS : LISTE DES ORDONNEES DE LA FONCTION LISSEE
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER       NDIM, I, J, K, JNT, NBINT, JMAX, JELIM, JLISS, 
     +              JENV, JPLAT, LVELM, LVELP, NBMAX, LCOUNT, JNCRE, 
     +              NBDEB, NBFRI, IFREQ, IFREQ1, IFREQ2, IMAX, LISCON
      REAL*8        TOLE, TOMOIN, TOPLUS, GAMMAX, TOPLAT, YMAX
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      TOLE = 1.0D-08
C
C     ------------------------------------------------------------------
C                    --- LISSSAGE ENVELOPPE ---
C     ------------------------------------------------------------------
C
      NDIM = NBFR + NBCREU
      CALL WKVECT ( '&&FOLIE1.ELIME'    ,'V V I',   NDIM, JELIM )
      CALL WKVECT ( '&&FOLIE1.LISS'     ,'V V I',   NDIM, JLISS )
      CALL WKVECT ( '&&FOLIE1.MAX'      ,'V V I',   NDIM, JMAX  )
      CALL WKVECT ( '&&FOLIE1.ENVELOP'  ,'V V R', 2*NDIM, JENV  )
      CALL WKVECT ( '&&FOLIE1.FREQ_PLAT','V V R', 2*NDIM, JPLAT )
      CALL WKVECT ( '&&FOLIE1.ELARM'    ,'V V R', 2*NDIM, LVELM )
      CALL WKVECT ( '&&FOLIE1.ELARP'    ,'V V R', 2*NDIM, LVELP )
C
      NBINT = 1
      IF ( NBCREU .NE. 0 ) THEN
         NBINT = NBCREU + 1
         CALL WKVECT ( '&&FOLIE1.NB_CREUX', 'V V I', NBINT, JNCRE )
C
C ------ DEFINTION DES INTERVALLES DE FREQUENCES POUR LES FREQ_CREUX
C
         DO 10 I = 1 , NBFR
            IF ( ABSC(I).LE.LCREU(1) ) ZI(JNCRE) = ZI(JNCRE) + 1
 10      CONTINUE
C
         DO 12 J = 2 , NBCREU
            DO 14 I = 1 , NBFR
               IF ( (ABSC(I).GE.LCREU(J-1)) .AND.
     +              (ABSC(I).LE.LCREU(J  )) ) THEN
                  ZI(JNCRE+J-1) = ZI(JNCRE+J-1) + 1
               ENDIF
 14         CONTINUE
 12      CONTINUE
C
         DO 16 I = 1 , NBFR
            IF ( ABSC(I).GE.LCREU(NBCREU) )
     +                       ZI(JNCRE+NBINT-1) = ZI(JNCRE+NBINT-1) + 1
 16      CONTINUE
      ENDIF  
C
C --- BOUCLE SUR LES FREQ_CREUX
C
      DO 100 JNT = 1 , NBINT      
         IF ( NBCREU .NE. 0 ) THEN
            IF ( JNT .EQ. 1 ) THEN
               NBDEB = 1
               NBFRI = ZI(JNCRE)
            ELSEIF ( (JNT.GT.1) .AND. (JNT.LT.NBINT) ) THEN
               NBDEB = NBFRI
               NBFRI = NBFRI + ZI(JNCRE+JNT-1)
            ELSEIF ( JNT.EQ.NBINT ) THEN
               NBDEB = NBFRI
               NBFRI = NBFR
            ENDIF
         ELSE 
            NBDEB = 1
            NBFRI = NBFR
         ENDIF
C
         IF ( ELARG .EQ. 'LOCAL' ) THEN
            CALL FOFREQ ( F1, ABSC, NBDEB, NBFR, 'PRO' )
            CALL FOFREQ ( F2, ABSC, NBDEB, NBFR, 'PRO' )
         ENDIF
C
C ------ CALCUL DE L'ELARGISSEMENT GAUCHE ET DROITE
C
         TOMOIN = (1.0D0-VALTG/100.0D0)
         TOPLUS = (1.0D0+VALTD/100.0D0)
         DO 110 I = NBDEB , NBFRI
            ZR(LVELM+I-1) = ABSC(I)*TOMOIN
            ZR(LVELP+I-1) = ABSC(I)*TOPLUS
C           ON RECUPERE FJ ET FK PROCHE DE F- ET F+ 
            CALL FOFREQ ( ZR(LVELM+I-1), ABSC, NBDEB, NBFRI, 'INF' )
            CALL FOFREQ ( ZR(LVELP+I-1), ABSC, NBDEB, NBFRI, 'SUP' )
 110     CONTINUE
         DO 112 I = NBFR+NBDEB, NBFR+NBFRI
            ZR(LVELM+I-1) = ABSC(I)
            ZR(LVELP+I-1) = ABSC(I)
 112     CONTINUE
C
C ------ ON ELIMINE LES FREQUENCES EN DEHORS DU DOMAINE DE DEFINITION
C
         DO 120 I = NBDEB,NBFRI
            IF (ZR(LVELM+I-1).LT.ABSC(NBDEB)) J=I+1-NBDEB
 120     CONTINUE
C
         DO 122 I = NBFRI,NBDEB,-1
            IF (ZR(LVELP+I-1).GT.ABSC(NBFRI)) K= NBFRI-I+1
 122     CONTINUE
         IFREQ1 = J
         IFREQ  = 0
         IFREQ2 = K
C
C ------ ENVELOPPE: POUR CHAQUE FREQUENCE ON PREND LE MAX DES GAMMA
C
         DO 130 I = NBDEB , NBFRI
            ZR(JENV+I-1) = ABSC(I)
C
            IF (ABSC(I).LT.ZR(LVELP+NBDEB-1)) THEN
               GAMMAX = MAX( ZR(LVELM+NBFR+IFREQ1+I-1), ACCE(IFREQ+I) )

C
            ELSEIF ( ZR(LVELM+NBFRI-1).LT.ABSC(I) ) THEN
               GAMMAX = MAX( ACCE(IFREQ+I), ZR(LVELP+NBFR-IFREQ2+I-1) )

C
            ELSEIF ( (ZR(LVELM+NBFRI-1).GE.ABSC(I)) .AND.
     +               (ZR(LVELP+NBDEB-1).LE.ABSC(I)) ) THEN
               GAMMAX = MAX( ZR(LVELM+NBFR+IFREQ1+I-1), ACCE(IFREQ+I) )

C
               GAMMAX = MAX( GAMMAX, ZR(LVELP+NBFR-IFREQ2+I-1) )
 
            ENDIF
C
            ZR(JENV+NBFR+I-1) = GAMMAX
 130     CONTINUE
         ZR(JENV+NBFR+NBDEB-1) = ACCE(NBDEB)
         ZR(JENV+NBFR+NBFRI-1) = ACCE(NBFRI)
C
C ------ LISSAGE DE L'ENVELOPPE DU SPECTRE
C
C ------ PLATEAUX SIGNIFICATIFS ----
C
         TOPLAT = (LARG/100.0D0)*(VALTG+VALTD)
         DO 140 I = NBDEB,NBFRI
            IF (ABS(ZR(JENV+NBFR+I-1)-ZR(JENV+NBFR+I)).LT.TOLE) THEN
               IF ((ZR(JENV+I-1)-ZR(JENV+I)).GT.TOPLAT) THEN
                  ZR(JPLAT+I-1) = ZR(JENV+I-1)
                  ZR(JPLAT+I  ) = ZR(JENV+I  )
               ENDIF
            ENDIF
 140     CONTINUE
C
C ------ RECUPERATION DES PICS DE L ENVELOPPE ---
C        ON RECUPERE LES INDICES DES FREQUENCES DES PICS DE LA FORME /\
C
         J = 1
         DO 150 I=NBDEB+1,NBFRI-1
            IF ( (ZR(JENV+NBFR+I-2).LT.ZR(JENV+NBFR+I-1)) .AND.
     +           (ZR(JENV+NBFR+I  ).LT.ZR(JENV+NBFR+I-1))   ) THEN
               ZI(JMAX+J-1)= I
               J = J + 1
            ENDIF
 150     CONTINUE
         NBMAX = J-1
C
C ------ RECUPERATION DU MAX DE L ENVELOPPE
C
         YMAX = ZR(JENV+NBFR+ZI(JMAX)-1)
         DO 160  J =1,NBMAX
            IF (ZR(JENV+NBFR+ZI(JMAX+J-1)-1).GT.YMAX) THEN
               YMAX = ZR(JENV+NBFR+ZI(JMAX+J-1)-1)
               IMAX = ZI(JMAX+J-1)
            ENDIF
 160     CONTINUE
C
C ------ ELIMINATION DES MAX "PATHOGENES"
C
         LCOUNT = 0
C ------ DE 1, NBMAX
         DO 170 I = 1,NBMAX-1
            IF ( (ZI(JMAX+I-1).LT.IMAX) .AND. (I.GT.1) ) THEN
               IF ( ZR(JENV+NBFR+ZI(JMAX+I-1)-1).GT.
     +                               ZR(JENV+NBFR+ZI(JMAX+I)-1)) THEN
                  LCOUNT = LCOUNT+1
               ELSE
                  ZI(JMAX+I-1-LCOUNT) = ZI(JMAX+I-1)
               ENDIF
            ELSEIF ( ZI(JMAX+I-1).GT.IMAX ) THEN
               IF ( ZR(JENV+NBFR+ZI(JMAX+I-1)-1).LT.
     +                               ZR(JENV+NBFR+ZI(JMAX+I)-1)) THEN
                  LCOUNT = LCOUNT+1
               ELSE
                  ZI(JMAX+I-1-LCOUNT) = ZI(JMAX+I-1)
               ENDIF
            ENDIF
 170     CONTINUE
         ZI(JMAX+NBMAX-1-LCOUNT) = ZI(JMAX+NBMAX-1)
C
         NBMAX = NBMAX - LCOUNT  

C ------ REDUCTION DU SPECTRE

         DO 180 I = 1,NBMAX+1
            CALL FOLIDR ( LISS, I, ZI(JMAX), NBMAX, NBDEB, NBFRI, NBFR,
     +                    ZR(JENV), ZI(JELIM) )
 180     CONTINUE
C    
 100  CONTINUE
C
C --- REUNION DES INTERVVALLES DE FREQUENCES
C
      ZI(JLISS) = 1

      DO 20 I = 2 , NBFR
         IF (ZI(JELIM+I-1).NE.I) ZI(JLISS+I-1) = I
 20   CONTINUE

      DO 22 I = 2 , NBFR
         DO 24 K =1 , NBFR
            IF ( (ZI(JELIM+I-1).EQ.I) .AND.
     +           (ZR(JENV+I-1).EQ.ZR(JPLAT+K-1)) )  ZI(JLISS+I-1) = I
 24      CONTINUE
 22   CONTINUE
C
C --- ON ELIMINE LES ZEROS DU TABLEAUX JLISS
C
      LISCON = 0
      DO 30 I = 1 , NBFR
         IF ( ZI(JLISS+I-1).EQ.0 ) THEN
            LISCON = LISCON + 1
         ELSE
            ZI(JLISS+I-1-LISCON) = ZI(JLISS+I-1)
         ENDIF
 30   CONTINUE
C
      NBFLIS = NBFR - LISCON        
C
C --- FONCTION LISSEE EN SORTIE ---
C
      J = 1
      DO 40 I = 1 , NBFR
         IF ( (J.LE.NBFLIS) .AND. (ZI(JLISS+I-1).NE.0) ) THEN
            ABSLIS(J) = ZR(JENV+ZI(JLISS+I-1)-1)
            ACCLIS(J) = ZR(JENV+NBFR+ZI(JLISS+I-1)-1)
            J = J + 1
         ENDIF
 40   CONTINUE      
C
      CALL JEDETR ( '&&FOLIE1.ELIME'     )
      CALL JEDETR ( '&&FOLIE1.LISS'      )
      CALL JEDETR ( '&&FOLIE1.MAX'       )
      CALL JEDETR ( '&&FOLIE1.ENVELOP'   )
      CALL JEDETR ( '&&FOLIE1.FREQ_PLAT' )
      CALL JEDETR ( '&&FOLIE1.ELARM'     )
      CALL JEDETR ( '&&FOLIE1.ELARP'     )
      CALL JEDETR ( '&&FOLIE1.NB_CREUX'  )
C
      CALL JEDEMA()
      END
