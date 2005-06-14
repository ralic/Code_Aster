      SUBROUTINE LIIMPR(NOML,IMPR,FICHIE)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     NOML,     FICHIE
      INTEGER                IMPR
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 27/11/2000   AUTEUR VABHHTS J.PELLET 
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
C     ROUTINE D'IMPRESSION D'UNE LISTE DE ENTIERS OU DE REELS
C     ----------------------------------------------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      CHARACTER*8   FILE, K8BID, CTYP
      CHARACTER*16  NOMCMD
      CHARACTER*19  NOMLIS
      CHARACTER*24  LPAS, NBPA, VALE, BINT, TITR
      LOGICAL       LISREE
C     ------------------------------------------------------------------
      CALL JEMARQ()
      IF ( IMPR .LE. 0) GOTO 9999
      FILE = FICHIE
      IUL  = IUNIFI(FILE)
      IF ( IUL .LE. 0 ) THEN
         CALL GETRES(K8BID,K8BID,NOMCMD)
         LG = MAX(1,LXLGUT(FILE))
         CALL UTMESS('A',NOMCMD//' (ERREUR 01)',
     +                   'LE FICHIER "'//FILE(1:LG)//'" N''EST RELIE '//
     +                   'A AUCUNE UNITE LOGIQUE.')
         GOTO 9999
      ENDIF
C
C     --- NOM DE LA FONCTION A EDITER ---
      NOMLIS = NOML
      LPAS   = NOMLIS//'.LPAS'
      NBPA   = NOMLIS//'.NBPA'
      VALE   = NOMLIS//'.VALE'
      BINT   = NOMLIS//'.BINT'
      TITR   = NOMLIS//'.TITR'
      LISREE = .TRUE.
      CALL JELIRA(VALE,'TYPE',IBID,CTYP)
      IF (CTYP(1:1).EQ.'I') LISREE = .FALSE.
C
C     --- IMPRESSION DU TITRE ---
      WRITE(IUL,'(/,1X,79(''-''))')
      CALL JEEXIN(TITR,IRET)
      IF (IRET .NE. 0 ) THEN
         CALL JEVEUO(TITR,'L',LTITR)
         CALL JELIRA(TITR,'LONMAX',NBTITR,K8BID)
         DO 10 I= 1, NBTITR
            WRITE(IUL,*) ZK80(LTITR+I-1)
 10      CONTINUE
      ENDIF
C
      CALL JELIRA(VALE,'LONMAX',NBVAL,K8BID)
      CALL JELIRA(NBPA,'LONMAX',NBINT,K8BID)
      CALL JEVEUO(LPAS,'L',JPAS)
      CALL JEVEUO(NBPA,'L',JNBP)
      CALL JEVEUO(VALE,'L',JVAL)
      CALL JEVEUO(BINT,'L',JBOR)
      NL = NBVAL / 5
      ND = NBVAL - ( NL * 5 )
C

      WRITE(IUL,'(3X,A,12X,A,10X,A,10X,A,5X,A)')
     +          'INTERVALLE','DEBUT','JUSQU_A','PAR_PAS','NOMBRE'
      IF ( LISREE ) THEN
         IF (NBVAL.EQ.1) THEN
          WRITE(IUL,'(3X,7X,I3,5X,1PE12.5,5X,1PE12.5,5X,1PE12.5,5X,I6)')
     +              1,ZR(JBOR),ZR(JBOR),ZR(JPAS),ZI(JNBP)
            IF (IMPR.GT.1) THEN
               WRITE(IUL,'(3X,A)')'IMPRESSION DE LA LISTE DE REELS'
               WRITE(IUL,'((I7,'' - '',1PE12.5))')1,ZR(JVAL)
            ENDIF
         ELSE
            DO 20 I = 1,NBINT
          WRITE(IUL,'(3X,7X,I3,5X,1PE12.5,5X,1PE12.5,5X,1PE12.5,5X,I6)')
     +              I,ZR(JBOR+I-1),ZR(JBOR+I),ZR(JPAS+I-1),ZI(JNBP+I-1)
 20         CONTINUE
            IF (IMPR.GT.1) THEN
               WRITE(IUL,'(3X,A)')'IMPRESSION DE LA LISTE DE REELS'
               WRITE(IUL,'((I7,'' - '',5(1PE12.5,1X)))')
     &               (5*(L-1)+1,(ZR( JVAL + 5*(L-1)+K-1),K=1,5),L=1,NL)
               IF (ND.NE.0) THEN
                  WRITE(IUL,'(I7,'' - '',5(1PE12.5,1X))')
     &                      5*NL+1,(ZR( JVAL +5*NL+K-1),K=1,ND)
               ENDIF
            ENDIF
         ENDIF
      ELSE
         IF (NBVAL.EQ.1) THEN
            WRITE(IUL,'(3X,7X,I3,5X,I12,5X,I12,5X,I12,5X,I6)')
     +                1,ZI(JBOR),ZI(JBOR),ZI(JPAS),ZI(JNBP)
            IF (IMPR.GT.1) THEN
               WRITE(IUL,'(3X,A)')'IMPRESSION DE LA LISTE D ENTIERS'
               WRITE(IUL,'((I7,'' - '',I12))')1,ZI(JVAL)
            ENDIF
         ELSE
            DO 30 I = 1,NBINT
               WRITE(IUL,'(3X,7X,I3,5X,I12,5X,I12,5X,I12,5X,I6)')
     +              I,ZI(JBOR+I-1),ZI(JBOR+I),ZI(JPAS+I-1),ZI(JNBP+I-1)
 30         CONTINUE
            IF (IMPR.GT.1) THEN
               WRITE(IUL,'(3X,A)')'IMPRESSION DE LA LISTE D ENTIERS'
               WRITE(IUL,'((I7,'' - '',5(I12,1X)))')
     &               (5*(L-1)+1,(ZI( JVAL + 5*(L-1)+K-1),K=1,5),L=1,NL)
               IF (ND.NE.0) THEN
                  WRITE(IUL,'(I7,'' - '',5(I12,1X))')
     &                      5*NL+1,(ZI( JVAL +5*NL+K-1),K=1,ND)
               ENDIF
            ENDIF
         ENDIF
      ENDIF
C
 9999 CONTINUE
      CALL JEDEMA()
      END
