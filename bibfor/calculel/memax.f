      SUBROUTINE MEMAX (CHAMP,NCP,LONG,VI,VR,VC,NBMAIL,NUMAIL)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)      CHAMP
      INTEGER                 NCP,LONG,VI(*),   NBMAIL,NUMAIL(*)
      REAL*8                              VR(*)
      COMPLEX*16                             VC(*)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 11/09/2002   AUTEUR VABHHTS J.PELLET 
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
C ----------------------------------------------------------------------
C     BUT :  EXTRAIRE LE "MAXI" OU LE 'MINI" SUR LA COMPOSANTE NCP D'UN
C            CHAM_ELEM ET RECUPERER LES COMPOSANTES ASSOCIEES.
C            LA SEULE CONTRAINTE EST QUE TOUS LES TYPE_ELEMENT DU LIGREL
C            CONNAISSENT LA GRANDEUR AVEC LA MEME LONGUEUR CUMULEE :
C
C            IDEM MESOMM
C
C
C IN  : CHAMP  :  NOM DU CHAMP A SOMMER
C IN  : NCP    :  NUMERO DE COMPOSANTE SUR LEQUEL ON FAIT LE TEST
C                 SI NCP POSITIF ---> CALCUL DE MAX
C                 SI NCP NEGATIF ---> CALCUL DE MIN
C IN  : LONG   :  LONGUEUR DES VECTEURS VI VR OU VC
C IN  : NBMAIL :  = 0   , CALCUL SUR TOUT LE CHAM_ELEM
C                 SINON , CALCUL SUR UNE PARTIE DU CHAM_ELEM
C IN  : NUMAIL :  NUMERO DES MAILLES
C OUT : VI     :  VECTEUR CONTENANT LA "SOMME" DU CHAMP SI LA GRANDEUR
C                 EST ENTIERE.
C OUT : VR     :  VECTEUR CONTENANT LA "SOMME" DU CHAMP SI LA GRANDEUR
C                 EST REELLE.
C OUT : VC     :  VECTEUR CONTENANT LA "SOMME" DU CHAMP SI LA GRANDEUR
C                 EST COMPLEXE (ON COMPARE ALORS LES MODULES).
C ----------------------------------------------------------------------
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
      CHARACTER*32     JEXNUM, JEXNOM, JEXATR
C     ------------------------------------------------------------------
      INTEGER      NBGREL, NBELEM, DIGDEL, NCPM, POM
      CHARACTER*8  SCALAI
      INTEGER      LONGT, NCMPEL, MODE, J, IGD, VALI
      REAL*8       RZERO, VALR, VALC
      CHARACTER*4  CVAL
      CHARACTER*8  SCAL
      CHARACTER*19 CHAMP2, LIGREL
      LOGICAL      FIRST
C
      CALL JEMARQ()
      NCPM = ABS(NCP)
      IF (NCPM.EQ.0) CALL UTMESS('F','MEMAX','ON NE SAIT PAS CALCULER'
     &                          //' UN EXTREMUM POUR CETTE COMPOSANTE')
      POM = NCP/NCPM
      CHAMP2 = CHAMP
      RZERO = 0.0D0
C
C     -- ON RETROUVE LE NOM DU LIGREL:
C     --------------------------------
      CALL JEEXIN (CHAMP2//'.CELD',IRET)
      IF(IRET.EQ.0) CALL UTMESS('F','MEMAX',
     &  'LE CHAMP DOIT ETRE UN CHAM_ELEM.')

      CALL JEVEUO (CHAMP2//'.CELK','L',IACELK)
      LIGREL = ZK24(IACELK-1+1)(1:19)
C

C     -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
      CALL CELVER(CHAMP2,'NBVARI_CST','STOP',IBID)
      CALL CELVER(CHAMP2,'NBSPT_1','STOP',IBID)

      CALL JEVEUO (CHAMP2//'.CELD','L',JCELD)
      IGD  = ZI(JCELD-1+1)
      SCAL = SCALAI(IGD)
C
C     -- ON VERIFIE LES LONGUEURS:
C     ----------------------------
      FIRST = .TRUE.
      NBGR  = NBGREL(LIGREL)
      DO 1 ,J = 1,NBGR
         MODE=ZI(JCELD-1+ZI(JCELD-1+4+J) +2)
         IF (MODE.EQ.0) GOTO 1
         NCMPEL = DIGDEL(MODE)
         ICOEF=MAX(1,ZI(JCELD-1+4))
         NCMPEL = NCMPEL * ICOEF
         IF (FIRST) THEN
            LONGT = NCMPEL
         ELSE
            IF (LONGT.NE.NCMPEL) THEN
               CALL UTMESS('F','MEMAX','LONGUEURS DES MODES LOCAUX '
     +                     //'IMCOMPATIBLES ENTRE EUX.')
            ENDIF
         ENDIF
         FIRST = .FALSE.
   1  CONTINUE
C
C     -- ON MET A ZERO LE VECTEUR "VSCAL":
C     ------------------------------------
      IF (LONGT.GT.LONG) THEN
         CALL UTMESS ('F','MEMAX','LA LONGUEUR:LONG EST TROP PETITE.')
      ENDIF
      IF (NCPM.GT.LONGT) THEN
         CALL UTMESS('F','MEMAX','IL N''Y A PAS AUTANT DE COMPOSANTES')
      ENDIF
      DO 10, I = 1,LONGT
         IF (SCAL(1:1).EQ.'I') THEN
            VI(I) = 0
         ELSE IF (SCAL(1:1).EQ.'R') THEN
            VR(I) = RZERO
         ELSE IF (SCAL(1:1).EQ.'C') THEN
            VC(I) = DCMPLX(RZERO,RZERO)
         ELSE
            CALL UTMESS('F','MEMAX','TYPE SCALAIRE INTERDIT :'//SCAL)
         ENDIF
 10   CONTINUE
C
C        -- ON CHERCHE L'EXTREMUM :
C        --------------------------
         CALL JEVEUO (CHAMP2//'.CELV','L',IAVALE)
         IF (NBMAIL.LE.0) THEN
            DO 2 ,J = 1,NBGR
               MODE=ZI(JCELD-1+ZI(JCELD-1+4+J) +2)
               IF (MODE .EQ.0 ) GOTO 2
               NEL = NBELEM(LIGREL,J)
               IDECGR=ZI(JCELD-1+ZI(JCELD-1+4+J)+8)
               DO 3 , K = 1,NEL
                  IF (SCAL(1:1).EQ.'I') THEN
                     VALI=POM*ZI(IAVALE-1+IDECGR+(K-1)*LONGT+NCPM-1)
                     IF (VALI.GT.(POM*VI(NCPM))) THEN
                        DO 100 I=1,LONGT
                           VI(I)=ZI(IAVALE-1+IDECGR+(K-1)*LONGT+I-1)
 100                    CONTINUE
                     ENDIF
                  ELSE IF (SCAL(1:1).EQ.'R') THEN
                     VALR=POM*ZR(IAVALE-1+IDECGR+(K-1)*LONGT+NCPM-1)
                     IF (VALR.GT.(POM*VR(NCPM))) THEN
                        DO 101 I=1,LONGT
                           VR(I)=ZR(IAVALE-1+IDECGR+(K-1)*LONGT+I-1)
 101                    CONTINUE
                     ENDIF
                  ELSE IF (SCAL(1:1).EQ.'C') THEN
                   VALC=POM*ABS(ZC(IAVALE-1+IDECGR+(K-1)*LONGT+NCPM-1))
                     IF (VALC.GT.(POM*ABS(VC(NCPM)))) THEN
                        DO 102 I=1,LONGT
                           VC(I)=ZC(IAVALE-1+IDECGR+(K-1)*LONGT+I-1)
 102                    CONTINUE
                     ENDIF
                  ENDIF
 3             CONTINUE
 2          CONTINUE
         ELSE
            CALL JEVEUO (LIGREL//'.LIEL','L',JLIGR)
            DO 30 IM = 1,NBMAIL
               INUM = 0
               DO 20 J = 1,NBGR
                  MODE=ZI(JCELD-1+ZI(JCELD-1+4+J) +2)
                  IF (MODE .EQ.0 ) GOTO 20
                  NEL = NBELEM(LIGREL,J)
                  IDECGR=ZI(JCELD-1+ZI(JCELD-1+4+J)+8)
                  DO 22  K = 1,NEL
                     IEL = ZI(JLIGR+INUM+K-1)
                     IF (IEL.NE.NUMAIL(IM)) GOTO 22
                     IF (SCAL(1:1).EQ.'I') THEN
                        VALI=POM*ZI(IAVALE-1+IDECGR+(K-1)*LONGT+NCPM-1)
                        IF (VALI.GT.(POM*VI(NCPM))) THEN
                           DO 105 I=1,LONGT
                              VI(I)=ZI(IAVALE-1+IDECGR+(K-1)*LONGT+I-1)
 105                       CONTINUE
                        ENDIF
                     ELSE IF (SCAL(1:1).EQ.'R') THEN
                        VALR=POM*ZR(IAVALE-1+IDECGR+(K-1)*LONGT+NCPM-1)
                        IF (VALR.GT.(POM*VR(NCPM))) THEN
                           DO 106 I=1,LONGT
                              VR(I)=ZR(IAVALE-1+IDECGR+(K-1)*LONGT+I-1)
 106                       CONTINUE
                        ENDIF
                     ELSE IF (SCAL(1:1).EQ.'C') THEN
                   VALC=POM*ABS(ZC(IAVALE-1+IDECGR+(K-1)*LONGT+NCPM-1))
                        IF (VALC.GT.(POM*ABS(VC(NCPM)))) THEN
                           DO 107 I=1,LONGT
                              VC(I)=ZC(IAVALE-1+IDECGR+(K-1)*LONGT+I-1)
 107                       CONTINUE
                        ENDIF
                     ENDIF
                     GOTO 30
 22               CONTINUE
                  INUM = INUM + NEL + 1
 20            CONTINUE
 30         CONTINUE
         ENDIF
C
C
 9999 CONTINUE
C
      CALL JEDEMA()
      END
