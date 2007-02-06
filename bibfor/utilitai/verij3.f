      SUBROUTINE VERIJ3(OBLIG,OBZ,VERIF,KARGZ,NOBJ)
      IMPLICIT NONE
      CHARACTER*1 OBLIG
      CHARACTER*(*) OBZ,VERIF,KARGZ
      INTEGER NOBJ
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 05/02/2007   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
C RESPONSABLE VABHHTS J.PELLET
C ----------------------------------------------------------------------
C  BUT : VERIFIER UN OBJET JEVEUX
C        => ERREUR <F> SI FAUX
C  IN   OBLIG : / 'O' : OBLIGATOIRE
C               / 'F' : FACULTATIF
C  IN   OBZ   : NOM D'UN OBJET JEVEUX
C  IN   VERIF  :   /'XOUS'     : XOUS(OBZ)=KARG  : /S/XD/XC/...
C                  /'GENR'     : GENR(OBZ)=KARG  : /V/N
C                  /'TYPE'     : TYPE(OBZ)=KARG  : /R/C/I/K8/K16
C                  /'LONMAX'   : LONMAX(OBZ)=INT(KARG)
C                  /'DOCU'     : DOCU(OBZ)=KARG

C  IN   KARGZ :   ARGUMENT CHAINE (SI VERIF LE NECESSITE)
C  OUIN/OUT NOBJ : ON INCREMENTE NOBJ SI L'OBJET EXISTE
C
C ----------------------------------------------------------------------
      CHARACTER*32 OB
      CHARACTER*1  XOUS, GENR, KBID,TMES
      CHARACTER*8  TYSCA ,STOCK,KARG2
      CHARACTER*12  VERIF2
      CHARACTER*24  VALK(4)
      CHARACTER*80  KARG
      CHARACTER*4 DOCU,DOCU2
      INTEGER I1,I2,IBID,I3, I4,LTYP,IEXI,K,ITROU,VALI(4)
      REAL*8 VALR(4)
      LOGICAL CONTIG

C -DEB------------------------------------------------------------------

      OB=OBZ
      KARG=KARGZ
      CALL ASSERT(OB(25:32).EQ.' ')
      VERIF2=VERIF

      CALL ASSERT(OBLIG.EQ.'O'.OR.OBLIG.EQ.'F')
      CALL JEEXIN(OB,IEXI)
      IF (IEXI.EQ.0) THEN
        IF (OBLIG.EQ.'O') CALL U2MESK('E','SDVERI_26',1,OB)
        GOTO 9999
      ELSE
        NOBJ=NOBJ+1
      ENDIF


C ---VERIFICATION DE XOUS :
C ----------------------------------
      IF (VERIF.EQ.'XOUS') THEN
        CALL JELIRA(OB,'XOUS',IBID,XOUS)
        STOCK=' '
        IF (XOUS.EQ.'X') CALL JELIRA(OB,'STOCKAGE',IBID,STOCK)
        CONTIG=STOCK.EQ.'CONTIG'

C      KARG CONTIENT AU PLUS 10 CHAMPS "XOUS" SEPARES PAR DES "/"
       ITROU=0
       I1=1
       DO 31, K=1,10
         I2=INDEX(KARG(I1:80),'/')
         IF (I2.EQ.0) THEN
           KARG2=KARG(I1:80)
         ELSE
           IF (I2.EQ.1) CALL ASSERT(.FALSE.)
           KARG2=KARG(I1:I1+I2-2)
         ENDIF

         IF (KARG2.EQ.'S'.AND.XOUS.EQ.'S') THEN
            ITROU=1
         ELSEIF (KARG2.EQ.'N'.AND.XOUS.EQ.'N') THEN
            ITROU=1
         ELSEIF (KARG2(1:2).EQ.'XC'.AND.XOUS.EQ.'X'.AND.CONTIG) THEN
            ITROU=1
         ELSEIF (KARG2(1:2).EQ.'XD'.AND.XOUS.EQ.'X'.AND.
     &           (.NOT.CONTIG)) THEN
            ITROU=1
         ENDIF
         IF (ITROU.EQ.1) GOTO 32
         I1=I1+I2
         CALL ASSERT(I1.LE.70)
31     CONTINUE
       IF (GENR.NE.KARG) THEN
           VALK(1)=OB
           VALK(2)=KARG
           VALK(3)=XOUS
           CALL U2MESK('F','SDVERI_32',3,VALK)
       ENDIF
32     CONTINUE



C ---VERIFICATION DE GENR :
C ----------------------------------
      ELSE IF (VERIF.EQ.'GENR') THEN
        CALL JELIRA(OB,'GENR',IBID,GENR)
        IF (GENR.EQ.'E') GENR='V'
        IF (GENR.NE.KARG) THEN
           VALK(1)=OB
           VALK(2)=KARG
           VALK(3)=GENR
           CALL U2MESK('F','SDVERI_31',3,VALK)
        ENDIF



C ---VERIFICATION DU LONMAX D'UN VECTEUR
C ---------------------------------------
      ELSE IF (VERIF.EQ.'LONMAX') THEN
        CALL JELIRA(OB,'LONMAX',I2,KBID)
        READ(KARG,'(I)') I3
        IF (I2.NE.I3) THEN
           VALK(1)=OB
           VALI(1)=I3
           VALI(2)=I2
           CALL U2MESG('F','SDVERI_17',1,VALK,2,VALI,0,VALR)
        ENDIF


C ---VERIFICATION DE TYPE :
C ----------------------------------
      ELSE IF (VERIF.EQ.'TYPE') THEN
       TYSCA=' '
       CALL JELIRA(OB,'TYPE',IBID,TYSCA(1:1))
       IF (TYSCA(1:1).NE.'I') THEN
         CALL JELIRA(OB,'LTYP',LTYP,KBID)
         CALL CODENT(LTYP,'G',TYSCA(2:8))
       ENDIF
C      KARG CONTIENT AU PLUS 10 CHAMPS "TYSCA" SEPARES PAR DES "/"
       ITROU=0
       I1=1
       DO 21, K=1,10
         I2=INDEX(KARG(I1:80),'/')
         IF (I2.EQ.0) THEN
           IF (KARG(I1:32).EQ.TYSCA) ITROU=1
           GOTO 22
         ELSE
           IF (I2.EQ.1) THEN
             KARG2=' '
           ELSE
             KARG2=KARG(I1:I1+I2-2)
           ENDIF
           IF (KARG2.EQ.TYSCA) THEN
             ITROU=1
             GOTO 22
           ENDIF
           I1=I1+I2
           CALL ASSERT(I1.LE.70)
         ENDIF
21     CONTINUE
22     CONTINUE
       IF(ITROU.EQ.0) THEN
         VALK(1)=OB
         VALK(2)=KARG
         VALK(3)=TYSCA
         CALL U2MESK('F','SDVERI_29',3,VALK)
       ENDIF


C ---VERIFICATION DE DOCU :
C ----------------------------------
      ELSE IF (VERIF.EQ.'DOCU') THEN
       CALL JELIRA(OB,'DOCU',IBID,DOCU)
C      KARG CONTIENT AU PLUS 10 CHAMPS "DOCU" SEPARES PAR DES "/"
       ITROU=0
       I1=1
       DO 11, K=1,10
         I2=INDEX(KARG(I1:80),'/')
         IF (I2.EQ.0) THEN
           IF (KARG(I1:32).EQ.DOCU) ITROU=1
           GOTO 12
         ELSE
           IF (I2.EQ.1) THEN
             KARG2=' '
           ELSE
             KARG2=KARG(I1:I1+I2-2)
           ENDIF
           IF (KARG2.EQ.DOCU) THEN
             ITROU=1
             GOTO 12
           ENDIF
           I1=I1+I2
           CALL ASSERT(I1.LE.70)
         ENDIF
11     CONTINUE
12     CONTINUE
       IF(ITROU.EQ.0) THEN
         VALK(1)=OB
         VALK(2)=KARG
         VALK(3)=DOCU
         CALL U2MESK('F','SDVERI_27',3,VALK)
       ENDIF



C ---AUTRE IMPREVU :
C ----------------------------------
      ELSE
        CALL U2MESK('F','SDVERI_25',1,VERIF2)
      ENDIF

9999  CONTINUE
      END
