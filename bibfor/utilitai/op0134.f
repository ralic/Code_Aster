      SUBROUTINE OP0134 ( IER )
      IMPLICIT   NONE
      INTEGER             IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 22/10/2002   AUTEUR MCOURTOI M.COURTOIS 
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
C     CALCUL D'UNE FONCTION INTERPRETEE
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER      IFM, NIV, INUMEX, N1, LNOVA, NBNOVA, ITYPFO, NBVALR,
     +             NBVAL, LVAL, NBVAL2, LVALE, LFON, IVAL, LPROL, 
     +             LFITA
      REAL*8       RVAL
      LOGICAL      COMPL
      CHARACTER*2  PROLGD
      CHARACTER*4  INTERP(2)
      CHARACTER*8  K8B, STATUT, NOMRES
      CHARACTER*16 NOMCMD, TYPRES
      CHARACTER*19 NOMFON, NOMFIN, LISTR
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
      IER = 0
      CALL INFMAJ
      CALL INFNIV ( IFM, NIV )
C
      CALL GETCMD ( NOMFON, TYPRES, NOMCMD, STATUT, INUMEX )
C
      CALL GETVID ( ' ', 'FONCTION', 1,1,1, NOMFIN, N1 )
C
      IF ( STATUT .EQ. 'MODIFIE') THEN
         IF ( NOMFON .NE. NOMFIN ) THEN
            CALL UTDEBM('F',NOMCMD,'ON NE PEUT UTILISER UNE FONCTION'//
     +                      ' DEJA EXISTANTE DIFFERENTE EN ENTREE ET'//
     +                      ' EN SORTIE.')
            CALL UTIMPK('L','FONCTION EN ENTREE ',1,NOMFIN)
            CALL UTIMPK('L','FONCTION EN SORTIE ',1,NOMFON)
            CALL UTFINM()
         ENDIF
      ENDIF
C
C
      CALL GETVTX ( ' ', 'PROL_GAUCHE', 1,1,1, PROLGD(1:1), N1 )
      CALL GETVTX ( ' ', 'PROL_DROITE', 1,1,1, PROLGD(2:2), N1 )
      CALL GETVTX ( ' ', 'NOM_RESU'   , 1,1,1, NOMRES     , N1 )
      CALL GETVTX ( ' ', 'INTERPOL'   , 1,1,2, INTERP     , N1 )
      IF ( N1 .EQ. 1 ) INTERP(2) = INTERP(1)
C
      CALL JEVEUO ( NOMFIN//'.NOVA', 'L', LNOVA )
      CALL JELIRA ( NOMFIN//'.NOVA', 'LONUTI', NBNOVA, K8B )
      IF ( NBNOVA .NE. 1 ) THEN
         CALL UTMESS('F',NOMCMD,'FONCTION A UNE SEULE VARIABLE ADMIS')
      ENDIF
C
      CALL FITYPF ( NOMFIN(1:8), ITYPFO )
      IF ( ITYPFO .EQ. 35 ) THEN
         COMPL = .TRUE.
      ELSE
         COMPL = .FALSE.
      ENDIF
C
C --- ACCES PAR "VALE_R"
C
      CALL GETVR8 ( ' ', 'VALE_R', 1,1,0, RVAL, NBVALR )
      IF ( NBVALR .NE. 0 ) THEN
         NBVAL  = - NBVALR
         CALL WKVECT ( '&&OP0134.VALE', 'V V R', NBVAL, LVAL )
         CALL GETVR8 ( ' ', 'VALE_R', 1,1,NBVAL, ZR(LVAL), N1 )
C
         IF ( COMPL ) THEN
            NBVAL2 = 3 * NBVAL
            CALL WKVECT ( NOMFON//'.VALE', 'G V R', NBVAL2, LVALE )
            LFON = LVALE + NBVAL
            DO 10 IVAL = 0, NBVAL-1
               ZR(LVALE+IVAL) = ZR(LVAL+IVAL)
               CALL FOINTC( NOMFIN, NBNOVA, ZK8(LNOVA),
     +                      ZR(LVALE+IVAL), ZR(LFON+2*IVAL), IER )
 10         CONTINUE
            CALL JEDETR('&&OP0134.VALE')
         ELSE
            NBVAL2 = 2 * NBVAL
            CALL WKVECT(NOMFON//'.VALE','G V R',NBVAL2,LVALE)
            LFON = LVALE + NBVAL
            DO 12 IVAL = 0, NBVAL-1
               ZR(LVALE+IVAL) = ZR(LVAL+IVAL)
               CALL FOINTE ( 'F ', NOMFIN, NBNOVA, ZK8(LNOVA),
     +                      ZR(LVALE+IVAL), ZR(LFON+IVAL), IER )
 12         CONTINUE
            CALL JEDETR('&&OP0134.VALE')
         ENDIF
      ENDIF
C
C --- ACCES PAR "LIST_PARA"
C
      CALL GETVID ( ' ', 'LIST_PARA', 1,1,1, LISTR, N1 )
      IF ( N1 .NE. 0 ) THEN
         CALL JEVEUO ( LISTR//'.VALE', 'L', LVAL)
         CALL JELIRA ( LISTR//'.VALE', 'LONUTI', NBVAL, K8B )
C
         IF ( COMPL ) THEN
            NBVAL2 = 3 * NBVAL
            CALL WKVECT ( NOMFON//'.VALE', 'G V R', NBVAL2, LVALE )
            LFON = LVALE + NBVAL
            DO 20 IVAL = 0, NBVAL-1
               ZR(LVALE+IVAL) = ZR(LVAL+IVAL)
               CALL FOINTC( NOMFIN, NBNOVA, ZK8(LNOVA),
     +                      ZR(LVALE+IVAL), ZR(LFON+2*IVAL), IER )
 20         CONTINUE
         ELSE
            NBVAL2 = 2 * NBVAL
            CALL WKVECT ( NOMFON//'.VALE', 'G V R', NBVAL2, LVALE )
            LFON = LVALE + NBVAL
            DO 22 IVAL = 0, NBVAL-1
               ZR(LVALE+IVAL) = ZR(LVAL+IVAL)
               CALL FOINTE ( 'F ', NOMFIN, NBNOVA, ZK8(LNOVA),
     +                             ZR(LVALE+IVAL), ZR(LFON+IVAL), IER )
 22        CONTINUE
         ENDIF
      ENDIF
C
C     --- CREATION ET REMPLISSAGE DE L'OBJET NOMFON.PROL ---
      IF (STATUT .EQ. 'NOUVEAU' ) THEN
         CALL WKVECT ( NOMFON//'.PROL', 'G V K8', 6, LPROL )
      ELSE
         CALL JEVEUO ( NOMFON//'.PROL', 'E', LPROL )
      ENDIF
      IF ( COMPL ) THEN
         ZK8(LPROL)   = 'FONCT_C '
      ELSE
         ZK8(LPROL)   = 'FONCTION'
      ENDIF
      ZK8(LPROL+1) = INTERP(1)//INTERP(2)
      ZK8(LPROL+2) = ZK8(LNOVA)
      ZK8(LPROL+3) = NOMRES
      ZK8(LPROL+4) = PROLGD
C
C     CREATION ET REMPLISSAGE DES OBJETS POUR UNE INTERPOLATION
C     INTERPRETEE.
      CALL WKVECT ( NOMFON//'.FITA', 'G V K24', 1, LFITA )
      ZK24(LFITA) = NOMFIN
C
C     --- VERIFICATION QU'ON A BIEN CREER UNE FONCTION ---
C         ET REMISE DES ABSCISSES EN ORDRE CROISSANT
      CALL ORDONN(NOMFON,NOMCMD,0)
C
      CALL TITRE
      IF (NIV.GT.1) CALL FOIMPR ( NOMFON, NIV, 'MESSAGE', 0, LISTR )
C
      CALL JEDEMA()
      END
