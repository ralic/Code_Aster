      SUBROUTINE FOECFD ( NOMCON, NOMFON, IUL, IND, FONINS )
      IMPLICIT   NONE
      INTEGER                              IUL, IND
      CHARACTER*(*)       NOMCON, NOMFON,           FONINS
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 10/05/2004   AUTEUR REZETTE C.REZETTE 
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
C     ECRITURE D'UNE FONCTION AU FORMAT FICHIER "MEDISIS"
C     ------------------------------------------------------------------
C
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
      CHARACTER*16              ZK16
      CHARACTER*24                        ZK24
      CHARACTER*32                                  ZK32
      CHARACTER*80                                            ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32     JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER       LPROL, NBPARA, NPS, IPARA, LONUTI, LVAR, LFON, 
     +              JVAL, IVAL, NBV, LPARA, LSPECT, IRET, NBFON, JNOMF
      REAL*8        PAS
      CHARACTER*1   K1BID
      CHARACTER*4   ORG
      CHARACTER*8   FILE
      CHARACTER*16  NOMCMD, CBID
      CHARACTER*19  LISTR, K19B
      CHARACTER*24  VALE  , PARA, PROL
C
      DATA FILE/' '/
C     ------------------------------------------------------------------
      CALL JEMARQ()
C
      LISTR = FONINS
      CALL GETRES(CBID,CBID,NOMCMD)
      CALL UTDEBM('I',NOMCMD,' ')
      CALL UTIMPK('L','ECRITURE  SUR LE FICHIER',1,FILE)
      CALL UTIMPI('S','ASSOCIE A L''UNITE LOGIQUE',1,IUL)
      CALL UTIMPK('S','DE LA FONCTION DE NOM',1,NOMFON)
      CALL UTFINM()
C     ------------------------------------------------------------------
      ORG         =  'VFVF'
      PROL        = NOMFON
      VALE        = NOMFON
      PARA        = NOMFON
      VALE(20:24) = '.VALE'
      PARA(20:24) = '.PARA'
      PROL(20:24) = '.PROL'

      CALL JELIRA(NOMCON,'LONUTI',NBFON,K1BID)
      CALL JEVEUO(NOMCON,'L',JNOMF)
      
C     --- INFORMATIONS DU .PROL ---
      CALL JEVEUO ( PROL, 'L', LPROL )
      WRITE(IUL,'(A)')  '             '
      WRITE(IUL,'(A)')  '  FONCTION  ' 
      IF(NBFON.EQ.1)THEN
         WRITE(IUL,'(2A)') '    NOM_FON  = ',ZK16(JNOMF)
      ELSE
         WRITE(IUL,'(2A)') '    NOMFON_X = ',ZK16(JNOMF)
         WRITE(IUL,'(2A)') '    NOMFON_Y = ',ZK16(JNOMF+1)
      ENDIF
      WRITE(IUL,'(A)')  '    ORIGINE  = ''ASTER - IMPR_COURBE'' '
      WRITE(IUL,'(2A)') '    NATURE   = ',ZK16(LPROL)
      WRITE(IUL,'(2A)') '    NOM_PARA = ',ZK16(LPROL+2)
      WRITE(IUL,'(2A)') '    NOM_RESU = ',ZK16(LPROL+3)
   
C
      IF ( ZK16(LPROL) .EQ. 'NAPPE' ) THEN
C
         WRITE(IUL,'(2A)') '    NOM_VAR  = ',ZK16(LPROL+5)
         CALL JELIRA ( PARA, 'LONUTI', NBPARA, K1BID )
         WRITE(IUL,'(A,I6)') '    NB_PARA  = ',NBPARA
         CALL JELIRA ( JEXNUM(VALE,1), 'LONUTI', NPS, K1BID )
C
         DO 10 IPARA =  2, NBPARA
            CALL JELIRA ( JEXNUM(VALE,IPARA), 'LONUTI', LONUTI, K1BID )
            IF ( LONUTI .NE. NPS ) THEN
               CALL UTMESS('E','NAPPE','ON NE PEUT ECRIRE EN FORMAT '//
     +                         '"SISPOU" QUE DES NAPPES DEFINIES '//
     +                         'SUR LES MEMES POINTS.')
               NPS = 0
               GOTO 9999
            ENDIF
 10      CONTINUE
         NPS = NPS / 2
         WRITE(IUL,'(A,I6)') '    NPS      = ',NPS
C
      ELSEIF ( ZK16(LPROL) .EQ. 'FONCTION' ) THEN
         WRITE(IUL,'(A,A)')    '    ORG      = ', ORG
         WRITE(IUL,'(A,A)')    '    FVA      = ', 'R08'
         WRITE(IUL,'(A,A)')    '    FFO      = ', 'R08'
         IF ( IND .EQ. 0 ) THEN
            CALL JELIRA ( VALE, 'LONUTI', NPS, K1BID )
            NPS = NPS / 2
         ELSE
            CALL JELIRA ( LISTR//'.VALE', 'LONUTI', NPS, K1BID )
         ENDIF
         WRITE(IUL,'(A,I6)') '    NPS      = ',NPS
C
      ENDIF
      WRITE(IUL,'(A)') ' VALEUR ='
C
C
      IF ( ZK16(LPROL) .EQ. 'FONCTION' ) THEN
         PAS = 0.D0
         IF ( IND .EQ. 0 ) THEN
            CALL JEVEUO ( VALE, 'L', LVAR )
            LFON = LVAR + NPS
         ELSE
            CALL JEVEUO ( LISTR//'.VALE', 'L', JVAL )
            NBV = 2 * NPS
            K19B = '&&FOECFD.NEW_FONC'
            CALL WKVECT ( K19B//'.VALE', 'V V R8', NBV, LVAR )
            LFON = LVAR + NPS
            DO 20 IVAL = 0, NPS-1
               ZR(LVAR+IVAL) = ZR(JVAL+IVAL)
               CALL FOINTE ('F ',NOMFON,1,ZK16(LPROL+2),ZR(LVAR+IVAL),
     +                                    ZR(LFON+IVAL),IRET)
 20         CONTINUE
         ENDIF
         CALL FOECFF ( IUL, ORG, PAS, NPS, ZR(LVAR), ZR(LFON), IRET )
         WRITE(IUL,'(A)') '   FINSF    '
C
      ELSEIF ( ZK16(LPROL) .EQ. 'NAPPE' ) THEN
         CALL JEVEUO ( PARA, 'L', LPARA )
         CALL JEVEUO ( JEXNUM(VALE,1), 'L', LSPECT )
         CALL FOECFN ( IUL, NPS, NBPARA, ZR(LPARA), ZR(LSPECT))
         WRITE(IUL,'(A)') '   FINSF   '
C
      ELSE
         CALL UTMESS('A',NOMCMD//' (ALARME 01)',
     +               'ON NE SAIT PAS IMPRIMER UNE FONCTION DE TYPE "'//
     +               ZK16(LPROL)//'"      DESOLE. ')
C
      ENDIF
C
      IF (IND.NE.0)  CALL JEDETR ( K19B//'.VALE' )
 9999 CONTINUE
      CALL JEDEMA()
      END
