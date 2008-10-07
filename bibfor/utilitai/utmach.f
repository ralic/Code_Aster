      SUBROUTINE UTMACH ( CHAMPZ, NCMP, NOCMP, TYPEMZ, LITROZ, NBTROU )
      IMPLICIT   NONE
      INTEGER             NBTROU, NCMP
      CHARACTER*8         NOCMP(*)
      CHARACTER*(*)       CHAMPZ, TYPEMZ, LITROZ
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 07/10/2008   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     CETTE ROUTINE PERMET DE CREER UN OBJET JEVEUX CONTENANT UNE LISTE
C     DE NOMS OU NUMEROS DE MAILLES OU DE NOEUDS CORRESPONDANT AUX
C     MOTS-CLES TRANSMIS EN ARGUMENTS.
C
C IN  : CHAMP  : NOM D'UN CHAMP
C IN  : NCMP   : NOMBRE DE COMPOSANTES DE NOCMP
C IN  : NOCMP  : LISTE DES COMPOSANTES
C IN  : TYPEM  : PRECISE LE TYPE DE LISTE QUE L'ON VEUT RECUPERER
C              : 'NU'  : NUMEROS DE MAILLES OU DE NOEUDS
C              : 'NO'  : NOMS    DE MAILLES OU DE NOEUDS
C IN/JXOUT : LITROZ : NOM DE L'OBJET JEVEUX QUI CONTIENDRA LA LISTE DES
C                     ENTITES (MAILLE OU NOEUD) TROUVEES
C OUT : NBTROU : NOMBRE D'ENTITES TROUVEES
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
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32     JEXNOM, JEXNUM
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
C
      INTEGER       IBID, IERD, JCESD, JCESK, JCESL, NBENT, JENT, I,
     &              NBPT, NBSP, IPT, ISP, ICP, IAD, IDLIST, ICMP,
     &              NCMPMX, GD, IER
      CHARACTER*2   TYPEM
      CHARACTER*4   DOCU
      CHARACTER*8   K8B, NOMGD
      CHARACTER*19  CHAMP, CHTRA1, CHTRA2
      CHARACTER*24  LITROU, NOMOBJ
      CHARACTER*24  VALK(5)
C DEB ------------------------------------------------------------------
      CALL JEMARQ()
C
      LITROU = LITROZ
      CHAMP  = CHAMPZ
      TYPEM  = TYPEMZ
C
      NBTROU = 0
      IF ( NCMP .EQ. 0 ) GOTO 9999
C
      CHTRA1 = '&&UTMACH.CHAMP_COP'
      CHTRA2 = '&&UTMACH.CHAMP_RED'
C
      CALL DISMOI ('F', 'TYPE_CHAMP', CHAMP, 'CHAMP', IBID, DOCU, IERD)
      CALL DISMOI ('F', 'NUM_GD'    , CHAMP, 'CHAMP', GD  , K8B , IERD)
      CALL JENUNO ( JEXNUM('&CATA.GD.NOMGD',GD), NOMGD )
      IF ( NOMGD(1:6) .EQ. 'VARI_R' ) GOTO 9999
C
C --- VERIFICATION QUE LES COMPOSANTES SONT DANS LE CHAMP
C
      CALL JELIRA(JEXNUM('&CATA.GD.NOMCMP',GD),'LONMAX',NCMPMX,K8B)
      CALL JEVEUO(JEXNUM('&CATA.GD.NOMCMP',GD),'L',IAD)
      IER = 0
      DO 2 ICP = 1 , NCMP
         DO 4 ICMP = 1 , NCMPMX
            IF ( NOCMP(ICP) .EQ. ZK8(IAD+ICMP-1) ) GOTO 2
 4       CONTINUE
         IER = IER + 1
         CALL U2MESK('E','UTILITAI5_48',1,NOCMP(ICP))
 2    CONTINUE
      IF (IER .NE. 0) CALL U2MESS('F','PREPOST_60')
C
C
      IF ( DOCU .EQ. 'ELGA' .OR.
     &     DOCU .EQ. 'ELNO' .OR.
     &     DOCU .EQ. 'ELEM' .OR.
     &     DOCU .EQ. 'CART' ) THEN
C          ----------------
         IF ( DOCU .EQ. 'CART' ) THEN
            CALL  CARCES ( CHAMP, 'ELEM', K8B, 'V', CHTRA1, IERD )
         ELSE
            CALL CELCES ( CHAMP, 'V', CHTRA1 )
         ENDIF
         CALL CESRED ( CHTRA1, 0, IBID, NCMP, NOCMP, 'V', CHTRA2 )
         CALL JEVEUO ( CHTRA2//'.CESD', 'L', JCESD )
         CALL JEVEUO ( CHTRA2//'.CESK', 'L', JCESK )
         CALL JEVEUO ( CHTRA2//'.CESL', 'L', JCESL )
         NOMOBJ = ZK8(JCESK-1+1)//'.NOMMAI         '
         NBENT =  ZI(JCESD-1+1)
         CALL WKVECT ( '&&UTMACH.LIST_ENT', 'V V I', NBENT, JENT )
         DO 10 I = 1 , NBENT
            NBPT = ZI(JCESD-1+5+4* (I-1)+1)
            NBSP = ZI(JCESD-1+5+4* (I-1)+2)
            DO 12 IPT = 1 , NBPT
               DO 14 ISP = 1 , NBSP
                  DO 16 ICP = 1 , NCMP
                     CALL CESEXI ( 'C',JCESD,JCESL,I,IPT,ISP,ICP,IAD )
                     IF ( IAD .GT. 0 ) THEN
                        ZI(JENT+I-1) = 1
                        GOTO 10
                     ELSE
                     ENDIF
 16               CONTINUE
 14            CONTINUE
 12         CONTINUE
 10      CONTINUE
         CALL DETRSD ( 'CHAM_ELEM_S', CHTRA1 )
         CALL DETRSD ( 'CHAM_ELEM_S', CHTRA2 )
C
C
      ELSEIF ( DOCU .EQ. 'NOEU' ) THEN
C              ----------------
         CALL CNOCNS ( CHAMP, 'V', CHTRA1 )
         CALL CNSRED ( CHTRA1, 0, IBID, NCMP, NOCMP, 'V', CHTRA2 )
         CALL JEVEUO ( CHTRA2//'.CNSD', 'L', JCESD )
         CALL JEVEUO ( CHTRA2//'.CNSK', 'L', JCESK )
         CALL JEVEUO ( CHTRA2//'.CNSL', 'L', JCESL )
         NOMOBJ = ZK8(JCESK-1+1)//'.NOMNOE         '
         NBENT =  ZI(JCESD-1+1)
         CALL WKVECT ( '&&UTMACH.LIST_ENT', 'V V I', NBENT, JENT )
         DO 20 I = 1 , NBENT
            DO 22 ICP = 1 , NCMP
               IF ( ZL(JCESL-1+(I-1)*NCMP+ICP) )  THEN
                  ZI(JENT+I-1) = 1
                  GOTO 20
               ENDIF
 22         CONTINUE
 20      CONTINUE
         CALL DETRSD ( 'CHAM_NO_S', CHTRA1 )
         CALL DETRSD ( 'CHAM_NO_S', CHTRA2 )
C
      ELSE
C
         CALL U2MESK('F','UTILITAI5_49',1,DOCU)
C
      ENDIF
C
      IF ( NBENT .EQ. 0 ) THEN
         VALK (1) = CHAMP
         VALK (2) = NOCMP(1)
         VALK (3) = NOCMP(2)
         VALK (4) = NOCMP(3)
         VALK (5) = NOCMP(4)
         IF ( DOCU .EQ. 'NOEU' ) THEN
            CALL U2MESG('F','UTILITAI8_61',5,VALK,0,0,0,0.D0)
         ELSE
            CALL U2MESG('F','UTILITAI8_62',5,VALK,0,0,0,0.D0)
         ENDIF
      ENDIF
C
      NBTROU = 0
      DO 100 I = 1 , NBENT
         IF ( ZI(JENT+I-1) .EQ. 1 )  NBTROU = NBTROU + 1
 100  CONTINUE
C
      IF ( TYPEM .EQ. 'NU' ) THEN
C          ---------------
         CALL WKVECT ( LITROU, 'V V I', NBTROU, IDLIST )
         NBTROU = 0
         DO 110 I = 1 , NBENT
            IF ( ZI(JENT+I-1) .EQ. 1 )  THEN
            NBTROU = NBTROU + 1
            ZI(IDLIST+NBTROU-1) = I
            ENDIF
 110     CONTINUE
C
      ELSEIF ( TYPEM .EQ. 'NO' ) THEN
C              ---------------
         CALL WKVECT ( LITROU, 'V V K8', NBTROU, IDLIST )
         NBTROU = 0
         DO 120 I = 1 , NBENT
            IF ( ZI(JENT+I-1) .EQ. 1 )  THEN
            NBTROU = NBTROU + 1
            CALL JENUNO ( JEXNUM(NOMOBJ,ZI(JENT+I-1)),
     &                                           ZK8(IDLIST+NBTROU-1) )
            ENDIF
 120     CONTINUE
C
      ELSE
         CALL U2MESK('F','PREPOST3_6',1,TYPEM)
      ENDIF
C
      CALL JEDETR ( '&&UTMACH.LIST_ENT' )
C
 9999 CONTINUE
C
      CALL JEDEMA( )
      END
