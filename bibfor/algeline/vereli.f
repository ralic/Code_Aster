      SUBROUTINE VERELI(NBCOMB, LMAT, IER)
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER           NBCOMB,LMAT(*),IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 21/02/96   AUTEUR VABHHTS J.PELLET 
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
C     VERIFICATION DE LA CONFORMITE DE LA S.D. ELIM_DDL
C     DES NBCOMB MATRICES LMAT
C     ------------------------------------------------------------------
C IN  I  NBCOMB       = NOMBRE DE MATRICES DONT ON VERIFIE LA COHERENCE
C                       EN CE QUI CONCERNE LES DDLS ELIMINES
C IN  I  LMAT(NBCOMB) = POINTEUR DES DESCRIPTEURS DE MATRICES
C OUT I  IER          = INDICATEUR D'ERREUR
C                     = 0 SI LES ELIM_DDL DES MATRICES ONT LA MEME
C                         STRUCTURE
C                     = 1 DANS LE CAS CONTRAIRE
C
C     -----------------------------------------------------------------
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
      CHARACTER*32  JEXNUM
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
C     -----------------------------------------------------------------
      CHARACTER*19  MATI
C     -----------------------------------------------------------------
      CALL JEMARQ()
      IER    = 0
C
      NEQ    = ZI(LMAT(1)+2)
      NBDDL1 = ZI(LMAT(1)+7)
      NBLIC1 = ZI(LMAT(1)+18)
C
      IF (NBDDL1.EQ.0) THEN
            IER = IER + 1
            CALL UTMESS('I','VERELI','LA PREMIERE MATRICE DE LA LISTE'//
     +                  ' NE COMPORTE PAS DE DDLS ELIMINES, LA '//
     +                  'ROUTINE EST DONC INOPERANTE, ELLE NE DOIT '//
     +                  'ETRE APPELEE QUE LORSQUE LES MATRICES A '//
     +                  'COMPARER ONT TOUTES DES DDLS ELIMINES.')
            GOTO 9999
      ENDIF
C
      CALL WKVECT('&&VERELI.CONI','V V I',NBCOMB,ICONI)
      CALL WKVECT('&&VERELI.LLIG','V V I',NBCOMB,ILLIG)
      CALL WKVECT('&&VERELI.ABLI','V V I',NBCOMB,IABLI)
      CALL WKVECT('&&VERELI.ALIG','V V I',NBCOMB,IALIG)
C
      DO 10 I =1, NBCOMB
         MATI = ZK24(ZI(LMAT(I)+1))
         CALL JEVEUO(MATI//'.CONI','L',ZI(ICONI+I-1))
         CALL JEVEUO(MATI//'.LLIG','L',ZI(ILLIG+I-1))
         CALL JEVEUO(MATI//'.ABLI','L',ZI(IABLI+I-1))
         CALL JEVEUO(MATI//'.ALIG','L',ZI(IALIG+I-1))
  10  CONTINUE
C
C--- VERIFICATION DE L'EGALITE DES DIFFERENTS ATTRIBUTS
C--- CONCERNANT LES DDLS ELIMINES DES MATRICES A COMPARER
C
      DO 20 I =2, NBCOMB
            NBDDLI = ZI(LMAT(I)+7)
            IF (NBDDL1.NE.NBDDLI) THEN
                  IER = IER + 1
                  CALL UTMESS('I','VERELI','LES NOMBRES DE DDLS'//
     +                        ' ELIMINES DES MATRICES A COMPARER'//
     +                        ' SONT DIFFERENTS')
                  GOTO 9999
            ENDIF
C
            NBLIC = ZI(LMAT(I)+18)
            IF (NBLIC1.NE.NBLIC) THEN
                  IER = IER + 1
                  CALL UTMESS('I','VERELI','LES NOMBRES DE BLOCS'//
     +                        ' DES .VALI DES MATRICES A COMPARER'//
     +                        ' SONT DIFFERENTS')
                  GOTO 9999
            ENDIF
C
            DO 30 J =1, NEQ
               IF (ZI(ZI(ICONI+1-1)+J-1).NE.ZI(ZI(ICONI+I-1)+J-1)) THEN
                  IER = IER + 1
                  CALL UTMESS('I','VERELI','LES MATRICES COMPORTENT'//
     +                        ' DES DDLS ELIMINES DIFFERENTS')
                  GOTO 9999
               ENDIF
  30        CONTINUE
C
            DO 40 J =1, 3*NBDDLI+1
               IF (ZI(ZI(ILLIG+1-1)+J-1).NE.ZI(ZI(ILLIG+I-1)+J-1)) THEN
                  IER = IER + 1
                  CALL UTMESS('I','VERELI','LES MATRICES ONT'//
     +                        ' DES .LLIG DIFFERENTS')
                  GOTO 9999
               ENDIF
  40        CONTINUE
C
            DO 50 J =1, NBLIC+1
               IF (ZI(ZI(IABLI+1-1)+J-1).NE.ZI(ZI(IABLI+I-1)+J-1)) THEN
                  IER = IER + 1
                  CALL UTMESS('I','VERELI','LES MATRICES ONT'//
     +                        ' DES .ABLI DIFFERENTS')
                  GOTO 9999
               ENDIF
  50        CONTINUE
C
            DO 60 J =1, NBDDLI
               IF (ZI(ZI(IALIG+1-1)+J-1).NE.ZI(ZI(IALIG+I-1)+J-1)) THEN
                  IER = IER + 1
                  CALL UTMESS('I','VERELI','LES MATRICES ONT'//
     +                        ' DES .ALIG DIFFERENTS')
                  GOTO 9999
               ENDIF
  60        CONTINUE
C
  20  CONTINUE
C
      DO 70 I =1, NBCOMB
         MATI = ZK24(ZI(LMAT(I)+1))
  70  CONTINUE
C
 9999 CONTINUE
C
      CALL JEDETR('&&VERELI.CONI')
      CALL JEDETR('&&VERELI.LLIG')
      CALL JEDETR('&&VERELI.ABLI')
      CALL JEDETR('&&VERELI.ALIG')
C
      CALL JEDEMA()
      END
