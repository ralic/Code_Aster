      SUBROUTINE CALCNA (COMPL,NBVAL,NBVALF,NOMFON,LVAL,NOMFIN,
     +                   STATUT,INTERF,INTERP,LNOVA,PROGDF,PROLGD,
     +                   LVALF,NOMCMD,NOMRES,NBNOVA )
      IMPLICIT   NONE
      INTEGER             IER
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 11/03/2003   AUTEUR DURAND C.DURAND 
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
C     CREATION DU SD FONCTION A PARTIR D'UNE FORMULE (NAPPE )
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
      INTEGER      LNOVA, NBNOVA,
     +             NBVAL, LVAL, LFON, IVAL, LPROL,
     +             LFITA, NBVALF, LPARA, LVALF,LONT, I, JVALS
      REAL*8       VALE(2)
      LOGICAL      COMPL
      CHARACTER*2  PROLGD,PROGDF
      CHARACTER*4  INTERP(2),INTERF(2)
      CHARACTER*8  K8B, STATUT, NOMRES
      CHARACTER*16 NOMCMD
      CHARACTER*19 NOMFON, NOMFIN, LISTR, LISTF
      CHARACTER*32 JEXNUM
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
C
C     --- CREATION ET REMPLISSAGE DE L'OBJET NOMFON.VALE ---
C      
C        
      IF (COMPL) THEN
        LONT=3*NBVALF*NBVAL        
      ELSE
        LONT=2*NBVALF*NBVAL
      ENDIF
C     
      CALL JECREC(NOMFON//'.VALE',' G V R','NU','CONTIG',
     +           'VARIABLE',NBVAL)
      CALL JEECRA ( NOMFON//'.VALE', 'LONT', LONT, ' ' )
      DO 10 I = 1 , NBVAL
        CALL JECROC ( JEXNUM(NOMFON//'.VALE',I) )
        CALL JEECRA ( JEXNUM(NOMFON//'.VALE',I) , 'LONMAX',
     +              LONT/NBVAL, ' ' )
        CALL JEECRA ( JEXNUM(NOMFON//'.VALE',I) , 'LONUTI',
     +                LONT/NBVAL, ' ' )
        CALL JEVEUO ( JEXNUM(NOMFON//'.VALE',I), 'E', JVALS )
        LFON = JVALS + NBVALF
        VALE(2)=ZR(LVAL+I-1)
        DO 20 IVAL = 0 , NBVALF-1
           ZR(JVALS+IVAL) = ZR(LVALF+IVAL)
           VALE(1)=ZR(JVALS+IVAL)
           IF (COMPL) THEN
             CALL FOINTC( NOMFIN, NBNOVA, ZK8(LNOVA),
     +                      VALE, ZR(LFON+2*IVAL), IER )
             IF (IER.NE.0) THEN
                CALL UTMESS('F',NOMCMD,'ERREUR DANS FOINTC') 
             ENDIF
           ELSE        
             CALL FOINTE( 'F',NOMFIN, NBNOVA, ZK8(LNOVA),
     +                      VALE, ZR(LFON+IVAL), IER )
           ENDIF          
 20     CONTINUE
 10    CONTINUE
C
C     --- CREATION ET REMPLISSAGE DE L'OBJET NOMFON.PROL ---
C
       IF (STATUT .EQ. 'NOUVEAU' ) THEN
          CALL WKVECT ( NOMFON//'.PROL', 'G V K16', 6+2*NBVAL, LPROL )
       ELSE
          CALL JEVEUO ( NOMFON//'.PROL', 'E', LPROL )
       ENDIF

       ZK16(LPROL)   = 'NAPPE'
       ZK16(LPROL+1) = INTERF(1)//INTERF(2)
       ZK16(LPROL+2) = ZK8(LNOVA+1)
       ZK16(LPROL+3) = NOMRES
       ZK16(LPROL+4) = PROGDF
       ZK16(LPROL+5) = ZK8(LNOVA)
       DO 30 IVAL = 1, NBVAL
          ZK16(LPROL+5+(2*IVAL-1)) = INTERP(1)//INTERP(2)
          ZK16(LPROL+5+(2*IVAL)) = PROLGD
 30    CONTINUE
C
C     --- CREATION ET REMPLISSAGE DE L'OBJET NOMFON.PARA ---
C
       CALL WKVECT ( NOMFON//'.PARA', 'G V R', NBVAL, LPARA )
       DO 40 IVAL=0,NBVAL-1
          ZR(LPARA+IVAL)=ZR(LVAL+IVAL)
 40    CONTINUE
C
       CALL JEDEMA()
       END
