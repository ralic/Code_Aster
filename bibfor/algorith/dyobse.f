      SUBROUTINE DYOBSE(NBPAS,LISINS,LISOBS,NBOBSE,RESU8)
      IMPLICIT   NONE
      INTEGER NBPAS,NBOBSE
      CHARACTER*8 RESU8
      CHARACTER*(*) LISINS,LISOBS
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/05/2000   AUTEUR VABHHTS J.PELLET 
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
C     SAISIE DU MOT CLE FACTEUR "OBSERVATION"
C     DUPLICATION DE DYARCH
C IN  : NBPAS   : NOMBRE DE PAS DE CALCUL
C IN  : LISINS  : INSTANTS DE CALCUL
C IN  : LISOBS  : LISTE D'OBSERVATION DES PAS DE CALCUL
C OUT : NBOBSE  : NOMBRE DE PAS A OBSERVER + CI
C OUT : RESU8   :

C ----------------------------------------------------------------------
C     --- DEBUT DECLARATIONS NORMALISEES JEVEUX ------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
C     --- FIN DECLARATIONS NORMALISEES JEVEUX --------------------------
      INTEGER IBID,NBPARA,JOBSE,NBOCC,N1,IRET,NTOBS,IFM,I,NIV,KCHAM,
     &        KCOMP,KNUCM,KNOEU,KMAIL,KPOIN
      PARAMETER (NBPARA=8)
      CHARACTER*4 CH4
      CHARACTER*8 MODELE,MAILLA,TYPARA(NBPARA)
      CHARACTER*16 NOPARA(NBPARA)
      CHARACTER*19 NOMT19

      DATA NOPARA/'NUME_ORDRE','INST','NOM_CHAM','NOM_CMP','NOEUD',
     &     'MAILLE','POINT','VALE'/
      DATA TYPARA/'I','R','K16','K8','K8','K8','I','R'/
C DEB------------------------------------------------------------------

      CALL JEMARQ()

      CALL INFNIV(IFM,NIV)

      NBOBSE = 0

      CALL WKVECT(LISOBS,'V V I',NBPAS,JOBSE)

      CALL GETFAC('OBSERVATION',NBOCC)

      IF (NBOCC.EQ.0) GO TO 20

      CALL GETVID(' ','MODELE',0,1,1,MODELE,N1)
      CALL DISMOI('F','NOM_MAILLA',MODELE,'MODELE',IBID,MAILLA,IRET)

C     ------------------------------------------------------------------
C                    VERIFICATION DES DONNEES
C                     COMPTAGE DES FONCTIONS
C     ------------------------------------------------------------------

      CALL DYOBS1(MAILLA,NBOCC,NTOBS)

C     ------------------------------------------------------------------
C                       SAISIE DES DONNEES
C     ------------------------------------------------------------------

      CALL DYOBS2(MAILLA,NBOCC,NTOBS)

C     ------------------------------------------------------------------
C                      TRAITEMENT DES INSTANTS
C     ------------------------------------------------------------------

      CALL DYOBS3(NBOCC,NBPAS,ZI(JOBSE),LISINS,NBOBSE)

C     ------------------------------------------------------------------
C                       CREATION DE LA TABLE
C     ------------------------------------------------------------------

      CALL JEEXIN(RESU8//'           .LTNT',IRET)
      IF (IRET.EQ.0) CALL LTCRSD(RESU8,'G')

      NOMT19 = ' '
      CALL LTNOTB(RESU8,'OBSERVATION',NOMT19)
      CALL JEEXIN(NOMT19//'.TBBA',IRET)
      IF (IRET.EQ.0) CALL TBCRSD(NOMT19,'G')

      CALL TBAJPA(NOMT19,NBPARA,NOPARA,TYPARA)

C     ------------------------------------------------------------------

C1234567890123456...12345678...12345678...12345678....4567
C    NOM_CHAM        NOM_CMP     NOEUD     MAILLE    POINT
C      IF ( NIV .EQ. 2 ) THEN
      CALL JEVEUO('&&DYOBSE.NOM_CHAM','L',KCHAM)
      CALL JEVEUO('&&DYOBSE.NOM_CMP ','L',KCOMP)
      CALL JEVEUO('&&DYOBSE.NUME_CMP','L',KNUCM)
      CALL JEVEUO('&&DYOBSE.NOEUD','L',KNOEU)
      CALL JEVEUO('&&DYOBSE.MAILLE','L',KMAIL)
      CALL JEVEUO('&&DYOBSE.POINT','L',KPOIN)
      WRITE (IFM,1000)
      DO 10 I = 0,NTOBS - 1
        IF (ZI(KPOIN+I).EQ.0) THEN
          CH4 = '    '
        ELSE
          CALL CODENT(ZI(KPOIN+I),'D',CH4)
        END IF
        WRITE (IFM,1010) ZK16(KCHAM+I),ZK8(KCOMP+I),ZK8(KNOEU+I),
     &    ZK8(KMAIL+I),CH4
   10 CONTINUE
C      ENDIF
C     ------------------------------------------------------------------
 1000 FORMAT (/,'=> OBSERVATION:',/,5X,
     &       'NOM_CHAM        NOM_CMP     NOEUD     MAILLE    POINT')
 1010 FORMAT (1X,A16,3X,A8,3X,A8,3X,A8,4X,A4)

   20 CONTINUE
      CALL JEDEMA()

      END
