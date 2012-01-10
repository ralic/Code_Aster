      SUBROUTINE LRVEMA(NOMAIL,MFICH,NOCHMD)
      IMPLICIT NONE
C
      INTEGER      MFICH
      CHARACTER*8  NOMAIL
      CHARACTER*64 NOCHMD
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 09/01/2012   AUTEUR SELLENET N.SELLENET 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C-----------------------------------------------------------------------
C   BUT : ROUTINE DE LIRE RESU / LIRE_CHAMP QUI VERIFIE LA COHERENCE
C       ENTRE LE MAILLAGE FOURNI ET LES DONNEES DU FICHIER MED
C
C IN  :
C   NOMAIL  K8   NOM DU MAILLAGE ASTER
C   MFICH   I    NUMERO DU FICHIER MED
C   NOCHMD  K64  NOM D'UN CHAMP REPOSANT SUR LE MAILLAGE MED A VERIFIER
C-----------------------------------------------------------------------
C RESPONSABLE SELLENET N.SELLENET
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON /IVARJE/ZI(1)
      COMMON /RVARJE/ZR(1)
      COMMON /CVARJE/ZC(1)
      COMMON /LVARJE/ZL(1)
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
C     -----  FIN  COMMUNS NORMALISES  JEVEUX --------------------------
C
      INTEGER IRET,NMATYP,JTYPMA,NCMP,JCMP,JUNIT
      INTEGER NBMA,JNBTYP,JMATYP,NBTYM,NBTV,CODRET
      INTEGER I,J,IDFIMD,IAUX,JNBTY2
      INTEGER VALI(2),LNOMAM,LXLGUT,JTYMAS
      INTEGER EDLECT
      PARAMETER (EDLECT=0)
      INTEGER NTYMAX
      PARAMETER (NTYMAX = 66)
      INTEGER EDCONN
      PARAMETER (EDCONN=1)
      INTEGER EDMAIL
      PARAMETER (EDMAIL=0)
      INTEGER EDNODA
      PARAMETER (EDNODA=0)
C
      INTEGER NUMMED(NTYMAX)
      REAL*8 R8B
      CHARACTER*1 K1B
      CHARACTER*8 SAUX08,NOMAST(NTYMAX),K8B
      CHARACTER*64 NOMAMD
      CHARACTER*200 NOFIMD
      CHARACTER*255 KFIC
      LOGICAL LFIRST
C
      DATA NOMAST  /'POI1    ','SEG2    ','SEG22   ','SEG3    ',
     &              'SEG33   ','SEG4    ',
     &                         'TRIA3   ','TRIA33  ','TRIA6   ',
     &              'TRIA66  ','TRIA7   ','QUAD4   ','QUAD44  ',
     &              'QUAD8   ','QUAD88  ','QUAD9   ','QUAD99  ',
     &              'TETRA4  ','TETRA10 ','PENTA6  ','PENTA15 ',
     &              'PENTA18 ','PYRAM5  ','PYRAM13 ','HEXA8   ',
     &              'HEXA20  ','HEXA27  ','TR3QU4  ','QU4TR3  ',
     &              'TR6TR3  ','TR3TR6  ','TR6QU4  ','QU4TR6  ',
     &              'TR6QU8  ','QU8TR6  ','TR6QU9  ','QU9TR6  ',
     &              'QU8TR3  ','TR3QU8  ','QU8QU4  ','QU4QU8  ',
     &              'QU8QU9  ','QU9QU8  ','QU9QU4  ','QU4QU9  ',
     &              'QU9TR3  ','TR3QU9  ','SEG32   ','SEG23   ',
     &              'QU4QU4  ','TR3TR3  ','HE8HE8  ','PE6PE6  ',
     &              'TE4TE4  ','QU8QU8  ','TR6TR6  ','SE2TR3  ',
     &              'SE2TR6  ','SE2QU4  ','SE2QU8  ','SE2QU9  ',
     &              'SE3TR3  ','SE3TR6  ','SE3QU4  ','SE3QU8  ',
     &              'SE3QU9  '/
C
      DATA NUMMED  /1,         102,       0,         103,
     &              0,         0,
     &                         203,       0,         206,
     &              0,         207,       204,       0,
     &              208,       0,         209,       0,
     &              304,       310,       306,       315,
     &              0,         305,       313,       308,
     &              320,       327,       0,         0,
     &              0,         0,         0,         0,
     &              0,         0,         0,         0,
     &              0,         0,         0,         0,
     &              0,         0,         0,         0,
     &              0,         0,         0,         0,
     &              0,         0,         0,         0,
     &              0,         0,         0,         0,
     &              0,         0,         0,         0,
     &              0,         0,         0,         0,
     &              0/
C-----------------------------------------------------------------------
C
      CALL JEMARQ()
C
C     ON VERIFIE QUE LE MAILLAGE FOURNI ET CELUI
C     CONTENU DANS LE FICHIER MED ONT
C     - MEME TYPE DE MAILLE
C     - MEME NOMBRE DE MAILLE PAR TYPE
C     =================================
C
      CALL ULISOG(MFICH, KFIC, K1B)
      IF ( KFIC(1:1).EQ.' ' ) THEN
        CALL CODENT ( MFICH, 'G', SAUX08 )
        NOFIMD = 'fort.'//SAUX08
      ELSE
        NOFIMD = KFIC(1:200)
      ENDIF
C
      NOMAMD=' '
      CALL MFOUVR( IDFIMD, NOFIMD, EDLECT, IAUX )
      IF ( IAUX.NE.0 ) THEN
        LNOMAM = LXLGUT(SAUX08)
        CALL U2MESK('F','MED_78',1,SAUX08(1:LNOMAM))
      ENDIF
C
      CALL MFNCO2( IDFIMD, NOCHMD, NCMP, CODRET )
      CALL WKVECT('&&LRVEMA.CNAME','V V K16',NCMP,JCMP)
      CALL WKVECT('&&LRVEMA.CUNIT','V V K16',NCMP,JUNIT)
C
      CALL MFNPDT ( IDFIMD, NOCHMD, NOMAMD, NBTV, ZK16(JUNIT),
     &              ZK16(JCMP), CODRET )
C
      CALL WKVECT('&&LRVERIMO_NBETYP1','V V I',NTYMAX,JNBTYP)
      CALL WKVECT('&&LRVERIMO_NBETYP2','V V I',NTYMAX,JNBTY2)
      DO 10 I=1,NTYMAX
        ZI(JNBTYP+I-1)=0
        IF(NUMMED(I).NE.0)THEN
           CALL MFNEMA ( IDFIMD, NOMAMD, EDCONN, EDMAIL, NUMMED(I),
     &                   EDNODA, NMATYP, IRET )
            ZI(JNBTYP+I-1)=NMATYP
        ENDIF
 10   CONTINUE
C
      CALL MFFERM ( IDFIMD, IRET )
      CALL DISMOI('F','NB_MA_MAILLA',NOMAIL,'MAILLAGE',NBMA,K8B,IRET)
      CALL WKVECT('&&LRVERIMO_NBMA_TYP','V V I',NBMA,JMATYP)
      DO 20 I=1,NBMA
         ZI(JMATYP+I-1)=0
 20   CONTINUE
C
      CALL JEVEUO('&CATA.TE.TYPEMA','L',JTYPMA)
      CALL JEVEUO(NOMAIL//'.TYPMAIL','L',JTYMAS)
      DO 30 I=1,NBMA
        ZI(JMATYP+I-1)=NUMMED(ZI(JTYMAS+I-1))
 30   CONTINUE
C
      DO 50 I=1,NTYMAX
        NBTYM=0
        ZI(JNBTY2+I-1)=NBTYM
        IF(NUMMED(I).NE.0)THEN
           DO 60 J=1,NBMA
              IF(ZI(JMATYP+J-1).EQ.NUMMED(I))THEN
                 NBTYM=NBTYM+1
              ENDIF
 60        CONTINUE
        ENDIF
        ZI(JNBTY2+I-1)=NBTYM
 50   CONTINUE
C
      LFIRST=.TRUE.
      DO 70 I=1,NTYMAX
        IF(NUMMED(I).NE.0)THEN
            IF(ZI(JNBTYP+I-1).NE.ZI(JNBTY2+I-1) .AND. LFIRST) THEN
                LFIRST=.FALSE.
                CALL U2MESS('A+','MED_54')
                IF(ZI(JNBTYP+I-1).LT.ZI(JNBTY2+I-1))THEN
                   VALI(1)=ZI(JNBTYP+I-1)
                   VALI(2)=ZI(JNBTY2+I-1)
                   CALL U2MESG('A','MED_59',1,NOMAST(I),2,VALI,0,R8B)
                ELSE
                   VALI(1)=ZI(JNBTYP+I-1)
                   VALI(2)=ZI(JNBTY2+I-1)
                   CALL U2MESG('A','MED_61',1,NOMAST(I),2,VALI,0,R8B)
                ENDIF

             ENDIF
         ENDIF
 70   CONTINUE
C
      CALL JEDETR('&&LRVERIMO_NBETYP1')
      CALL JEDETR('&&LRVERIMO_NBETYP2')
      CALL JEDETR('&&LRVERIMO_NBMA_TYP')
      CALL JEDETR('&&LRVEMA.CNAME')
      CALL JEDETR('&&LRVEMA.CUNIT')
C
      CALL JEDEMA()
C
      END
