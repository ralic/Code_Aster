      SUBROUTINE FILOAD(LPILE,PILE,ICLASS,MI,MR,ML,MC,IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           LPILE,PILE(*),ICLASS(*),MI(*),IER
      REAL*8                                       MR(*)
      LOGICAL                                         ML(*)
      COMPLEX*16                                         MC(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 01/04/96   AUTEUR GIBHHCM C.MASSERET 
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
C     CHARGEMENT D'UNE FONCTION OU D'UN TABLEAU (&LOAD)
C     ------------------------------------------------------------------
C VAR LPILE   : SOMMET DE PILE
C VAR PILE    : PILE DES OPERANDES
C VAR MI,MR,ML,MC : TABLE DES CONSTANTES ENTIERES, REELLES, ...
C OUT IER     : CODE DE RETOUR
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
      CHARACTER*16            ZK16
      CHARACTER*24                    ZK24
      CHARACTER*32                            ZK32
      CHARACTER*80                                    ZK80
      COMMON  /KVARJE/ ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER      KCLASS, IVAL, IBID, IARITE
      REAL*8       RVAL, RESU
      COMPLEX*16   CVAL
      CHARACTER*8  CBID
      CHARACTER*19 KVAL
      INTEGER      LIVAL
      REAL*8       LRVAL(2)
C     ------------------------------------------------------------------
      COMMON /FICL01/ ICSTE , IOPE1, IOPE2, LOPE1, LOPE2
C     ------------------------------------------------------------------
      CALL JEMARQ()
      IER   = 0
      NEXP  = PILE(LPILE)
      LPILE = LPILE - 1
      CALL FIEXTR(NEXP,KCLASS,IVAL,RVAL,CVAL,KVAL,IBID,IARITE)
      CALL JEEXIN(KVAL//'.ADVA',IRET)
      CALL JEEXIN(KVAL//'.PROL',IRE2)
      IF (IRET .EQ. 0 .AND. IRE2 .EQ. 0) THEN
         IER = IER + 1
         ILG = MAX(1,LXLGUT(KVAL))
         CALL UTMESS('E','SUPERVISEUR (ERREUR.FILOAD.01)','LA FONCTION '
     +                 //'"'//KVAL(:ILG)//'" N''EST PAS ACCESSIBLE. '
     +                 //'TESTER L''OPTION : DEBUT(PAR_LOT:''NON'');')
      ELSE IF (IRET .NE. 0) THEN
C  LA FONCTION EST DE TYPE INTERPRETEE
         CALL JEVEUO(KVAL//'.ADVA','L',LADVA)
         CALL JELIRA(KVAL//'.ADVA','LONUTI',LONUTI,CBID)
         DO 10  ILON = LONUTI, 1, -1
            IADR = ZI(LADVA+ILON-1)
            CALL FIEXTR(IADR,KCLASS,IVAL,RVAL,CVAL,CBID,IBID,IARITE)
            IBID = ICLASS(LPILE)+10
            LIVAL=MI(LPILE)
            LRVAL(1)=MR(LPILE)
            LRVAL(2)=MR(LPILE+1)       
            CALL FIREMP(2,IBID,LIVAL,LRVAL,
     +           CBID,LPLACE)
            LPILE = LPILE - 1
 10      CONTINUE
      ELSE IF (IRET .EQ. 0 .AND. IRE2 .NE. 0) THEN
C  LA FONCTION EST DE TYPE TABULEE
         CALL JEVEUO(KVAL//'.PROL','L',LPROL)
         CALL FITABU(KVAL,1,ZK8(LPROL+2),MR(LPILE),RESU,IER)
         MR(LPILE) = RESU
      ENDIF
      CALL JEDEMA()
      END
