      SUBROUTINE PREMLC(N1,DIAG,COL,
     +                  PARENT,PAREND,ANC,NOUV,
     +                  SUPND,SUPND2,NOUVSN,ANCSN,P,Q,LBD1,
     +                  LBD2,RL,RL1,RL2,NRL,INVP,PERM,
     +                  LGIND,DDLMOY,NBSND)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 10/07/2007   AUTEUR PELLET J.PELLET 
C RESPONSABLE JFBHHUC C.ROSE
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
C TOLE CRP_21
C     VERSION O2000 AVEC CREATION D'UN NOUVEAU SN POUR
C     CHAQUE LAMBDA1 DE LAGRANGE
C     11/12/98
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER N1,DIAG(0:*),COL(*),LGIND,DDLMOY
      INTEGER PARENT(*)
      INTEGER NBSN,PAREND(*)
      INTEGER ANC(N1),NOUV(N1),SUPND(N1),SUPND2(N1),LBD1(N1),LBD2(N1)
      INTEGER INVP(N1),PERM(N1)
      INTEGER RL(4,*),RL1(*),RL2(*)
C     VARIABLES LOCALES
      INTEGER I,J,T,NADJ,LMAT,IER,IFM,NIV
      INTEGER I1,I2,IDDL,IDDL1,IDDL2,NUM,ISN
      INTEGER NOUVSN(0:N1),ANCSN(*),P(*),Q(*)
      INTEGER NRL,MAXRL,MINRL,NBSND,J1,J2,IANC,IP,IPP,IRET
      INTEGER VALI(3)
      LOGICAL LFETI
C--------------------------------------------------------------
C      5) POUR LES REL.LIN.,ON FAIT RL1(I)=LAMBD1,I ETANT LE
C        DDL DE REL.LIN.
C          DONT L'IMAGE PAR LA NOUVELLE NUMEROTATION EST
C          L'INF DES DDL. ENCADRES
C      6) ON ECRIT LA NOUVELLE NUMEROTATION DE TOUS
C          LES DDL APRES GENMMD
C         => TAB NOUV ET ANC (1:N1) <-> (1:N2)
C            LAMBDA1  EST AMALGAME AU PREMIER ND ENCADRE (*)
C (*)        CECI EST POSSIBLE CAR TOUS LES NOEUDS D'UNE RELATION
C            LINEAIRE SONT FORCES A ETRE VOISINS ( DS CALADJ)
C            LAMBDA2 LUI EST AMALGAME AU DERNIER ND ENCADRE
C            NBSND : NBRE DE SND AVEC LES LAMBDA1 = NBSN + NRL
C            PARENTD REPRESENTE LE NOUVEL ARBRE D'ELIMINATION
C       RQE GENERALE AVEC LES REL.LIN. ON UTILISE LA DONNEE SUIVANTE :
C       LES DDL ENCADRES SONT DEFINIS PAR
C      ( COL(J),J=DIAG(LAMBDA2-1)+2,DIAG(LAMBDA2)-1 )
C--------------------------------------------- CALCUL DE ADJNC1
C****************************************************************
C      CALL UTTCPU(2,'DEBUT',6,TEMPS)
C****************************************************************
C-----RECUPERATION DU NIVEAU D'IMPRESSION
C
      CALL INFNIV(IFM,NIV)
C FETI OR NOT FETI ?
      CALL JEEXIN('&FETI.MAILLE.NUMSD',IRET)
      IF (IRET.NE.0) THEN
        CALL INFMUE()
        CALL INFNIV(IFM,NIV)
        LFETI=.TRUE.
      ELSE
        LFETI=.FALSE.
      ENDIF
C-----------------------------------------------------------------

C------------------------------- RELATIONS LINEAIRES
      DO 160 I = 1,NRL
        IDDL2 = RL(2,I)
        MINRL = N1 + 1
        MAXRL = 0
        IDDL1 = COL(DIAG(IDDL2-1)+1)
        IF((DIAG(IDDL2)-DIAG(IDDL2-1)).LE.2) THEN
        VALI (1) = IDDL2
        CALL U2MESG('F','ALGELINE5_35',0,' ',1,VALI,0,0.D0)
        END IF
        RL(1,I) = IDDL1
        J1 = DIAG(IDDL2-1) + 2
        J2 = DIAG(IDDL2) - 1
        LGIND = LGIND + 2* (J2-J1+2) * DDLMOY
        DO 150 J = J1,J2
          IDDL = COL(J)
          IPP = P(IDDL)

          IF (IPP.GT.0) THEN
            IP = INVP(IPP)
            IF (IP.GT.MAXRL) MAXRL = INVP(P(IDDL))
            IF (IP.LT.MINRL) MINRL = INVP(P(IDDL))
          END IF

  150   CONTINUE
C                      RL1 ET RL2 MARQUENT LES DDL T.Q.
C                      LEURS IMAGES PAR LA RENUMEROTATION
C                      SOIENT LES PREMIERS ET DERNIERS ENCADRES
        RL(3,I) = MINRL
        RL(4,I) = MAXRL
        RL1(MINRL) = 1
        RL2(MAXRL) = 1
  160 CONTINUE
C--------------------------------- CALCUL DE NOUV,ANC,SUPND
      NOUVSN(0) = 0
      NBSN = NBSND
      DO 180 I = 1,NBSN
        NOUVSN(I) = I
        ANCSN(I) = I
  180 CONTINUE
C                       NOUVSN ET ANCSN SERVENT DE TAB NOUV ET ANC
C                     POUR LES  SUPERNDS
      NBSND = 1
      NUM = 1
      SUPND(NBSND)= NUM
      DO 260 ISN = 1,NBSN
        I1 = SUPND2(ISN)
        I2 = SUPND2(ISN+1) - 1

C                                 ON MET EN TETE DU SUPERNOEUD :
C                                LES LAMBDA1 DE RELATION LINEAIRES
C                                PUIS LES LAMBDA1 DE BLOCAGE
        DO 200 I = I1,I2
          IF (RL1(I).NE.0) THEN
C         I EST LE 1ER DDL D UN RELATION LINEAIRE ( LA J EME)
            DO 190 J = 1,NRL
              IF (RL(3,J).EQ.I) THEN
                NOUV(RL(1,J)) = NUM
                ANC(NUM) = RL(1,J)
                NUM = NUM + 1
C       CREATION D UN NOUVEAU SN ANCSN A UNE VALEUR NEGATIVE POUR
C       MARQUER LA NOUVEAUTE
        ANCSN(NBSND) = - ISN
C        PRINT *, ' ON CREE UN NV SN LAMBD1 DE RL : '
            NBSND = NBSND + 1
            SUPND(NBSND) = NUM
              END IF
  190       CONTINUE

         END IF

  200   CONTINUE
C
        DO 210 I = I1,I2
          IANC = Q(PERM(I))
          IF (LBD1(IANC).NE.0) THEN
C         I EST UN DDL BLOQUE
            NOUV(LBD1(IANC)) = NUM
            ANC(NUM) = LBD1(IANC)
            NUM = NUM + 1
C       CREATION D UN NOUVEAU SN ANCSN A UNE VALEUR NEGATIVE POUR
C       MARQUER LA NOUVEAUTE
            ANCSN(NBSND) = - ISN
C        PRINT *, ' ON CREE UN NV SN LAMBD1 DE BLOCAGE : '
            NBSND = NBSND + 1
            SUPND(NBSND) = NUM
          END IF

  210   CONTINUE
C       ADDITION DES DDL NON LAGRANGES
        DO 220 I = I1,I2
          IANC = Q(PERM(I))
          NOUV(IANC) = NUM
          ANC(NUM) = IANC
          NUM = NUM + 1
  220   CONTINUE
C                         ON MET EN QUEUS DU SUPERNOEUD :
C                            LES LAMBDA2 DE BLOCAGE,PUIS
C                           LES LAMBDA2 DE RELATION LINEAIRES
        DO 230 I = I1,I2
          IANC = Q(PERM(I))
          IF (LBD2(IANC).NE.0) THEN
            NOUV(LBD2(IANC)) = NUM
            ANC(NUM) = LBD2(IANC)
            NUM = NUM + 1
          END IF

  230   CONTINUE
        DO 250 I = I1,I2
          IF (RL2(I).NE.0) THEN
            DO 240 J = 1,NRL
              IF (RL(4,J).EQ.I) THEN
                NOUV(RL(2,J)) = NUM
                ANC(NUM) = RL(2,J)
                NUM = NUM + 1
              END IF

  240       CONTINUE
          END IF

  250   CONTINUE
C        PRINT *, ' ON CREE UN NV SN DDL ORDINAIRE : '
         ANCSN(NBSND) =  ISN
         NOUVSN(ISN) = NBSND
        NBSND = NBSND + 1
        SUPND(NBSND) = NUM
  260 CONTINUE
      NBSND = NBSND-1
      NUM = NUM -1
      IF (NUM.NE.N1) THEN
        VALI (1) = NUM
        VALI (2) = N1
        CALL U2MESG('F+','ALGELINE5_36',0,' ',2,VALI,0,0.D0)
        DO 350 I = 1,N1
            IF(LBD1(I).NE.0) THEN
            WRITE(IFM,*)'LE DDL BLOQUE: ',I,' A POUR LAMBDA1: ',LBD1(I)
            WRITE(IFM,*)'LE DDL BLOQUE: ',I,' A POUR LAMBDA2: ',LBD2(I)
               IF(LBD2(I).EQ.0) IER =1
            ELSEIF(LBD2(I).NE.0) THEN
               IER =1
            ENDIF
         IF(IER.EQ.1) THEN
            VALI (1) = I
            VALI (2) = LBD1(I)
            VALI (3) = LBD2(I)
            CALL U2MESG('F+','ALGELINE5_37',0,' ',3,VALI,0,0.D0)
         ENDIF
 350  CONTINUE
      VALI (1) = NRL
      CALL U2MESG('F+','ALGELINE5_38',0,' ',1,VALI,0,0.D0)
      DO 360 I=1,NRL
            VALI (1) = RL(1,I)
            VALI (2) = RL(2,I)
            CALL U2MESG('F+','ALGELINE5_39',0,' ',2,VALI,0,0.D0)
 360     CONTINUE
      END IF
C----------------------    CALCUL DU NOUVEAU PARENT
      DO 270 ISN = 1,NBSND
        IF( ANCSN(ISN).GT.0) THEN
        PAREND(ISN) = NOUVSN( PARENT( ANCSN( ISN ) ) )
        ELSE
C       C'EST UN NOUVEAU SN (LAMBDA1)
        PAREND(ISN) = NOUVSN( -  ANCSN( ISN ) )
        ENDIF
  270 CONTINUE
      IF (NIV.GE.2) THEN
       WRITE(IFM,*)'   --- APRES ADDITION  DES  RELATIONS LINEAIRES '
       WRITE(IFM,*)'   --- NOMBRE DE SUPERNOEUDS ',NBSND
      ENDIF
      IF (LFETI) CALL INFBAV()
C****************************************************************
C      CALL UTTCPU(2,'FIN  ',6,TEMPS)
C****************************************************************
      END
