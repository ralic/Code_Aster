      SUBROUTINE MRMULT(CUMUL,LMAT,VECT,TYPVEC,XSOL,NBVECT)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*) CUMUL
      CHARACTER*1 TYPVEC
      INTEGER LMAT,NBVECT
      REAL*8 VECT(*),XSOL(*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 01/02/2000   AUTEUR VABHHTS J.PELLET 
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
C     ------------------------------------------------------------------
C     EFFECTUE LE PRODUIT D'UNE MATRICE PAR N VECTEURS
C     ------------------------------------------------------------------
C IN  CUMUL  : K4 : INDICATEUR D'INITIALISATION A ZERO DU SECOND MEMBRE
C              = 'ZERO' ON INITIALISE A ZERO LES SECONDS MEMBRES
C              = 'CUMU' ON N 'INITIALISE PAS A ZERO LES SECONDS MEMBRES
C                  CUMUL AVEC LES VALEURS D'ENTREES
C IN  LMAT  : I : DESCRIPTEUR DE LA MATRICE
C IN  VECT  :R/C: VECTEURS A MULTIPLIER PAR LA MATRICE
C IN  TYPVEC: K1: TYPE DU VECTEUR A MULTIPLIER PAR LA MATRICE
C VAR XSOL  :R/C: VECTEUR(S) SOLUTION(S)
C             MAIS SI CUMUL = 'ZERO' ALORS XSOL EST EN MODE OUT
C IN  NBVECT: I : NOMBRE DE VECTEURS A MULTIPLIER (ET DONC DE SOLUTIONS)
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
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
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------

      CALL JEMARQ()
      CALL JEVEUO(ZK24(ZI(LMAT+1)) (1:19)//'.REFA','L',IDREFE)
      CALL JEVEUO(ZK24(IDREFE+2) (1:19)//'.HCOL','L',IDHCOL)
      GO TO (10,60) ZI(LMAT+6)

      CALL UTMESS('F','MRMULT','STOCKAGE NON PREVU')
      GO TO 110

C       ----------------------------------------------------------------
   10 CONTINUE
C       STOCKAGE PROFIL PAR BLOC
C       ZI(+12) : ABLO : DERNIER EQUATION CONTENU DANS LE BLOC
C       ZI(+2 ) : NEQ     : NOMBRE D'EQUATIONS
C       ZI(+13) : NOMBRE DE BLOC
C       ----------------------------------------------------------------
      GO TO (20,30) ZI(LMAT+3)
C          -------------------------------------------------------------
      CALL UTMESS('F','MRMULT','COEFFICIENT DE TYPE NON PREVU')
      GO TO 110
C          -------------------------------------------------------------
   20 CONTINUE
C          MATRICE REELLE
      IF (TYPVEC.EQ.'R') THEN
C             MATRICE SYMETRIQUE
        IF (ZI(LMAT+4).EQ.1) THEN
          CALL MTDSC2(ZK24(ZI(LMAT+1)),'ABLO','L',JJABLO)
          CALL MTDSC2(ZK24(ZI(LMAT+1)),'ADIA','L',JJADIA)
          CALL MRMPVR(CUMUL,ZK24(ZI(LMAT+1)),ZI(JJADIA),ZI(IDHCOL),
     &                ZI(JJABLO),ZI(LMAT+02),ZI(LMAT+13),VECT,XSOL,
     &                NBVECT)
C             MATRICE NON-SYMETRIQUE
        ELSE IF (ZI(LMAT+4).EQ.0) THEN
          CALL MTDSC2(ZK24(ZI(LMAT+1)),'ABLO','L',JJABLO)
          CALL MTDSC2(ZK24(ZI(LMAT+1)),'ADIA','L',JJADIA)
          CALL MRNPVR(CUMUL,ZK24(ZI(LMAT+1)),ZI(JJADIA),ZI(IDHCOL),
     &                ZI(JJABLO),ZI(LMAT+02),ZI(LMAT+13),VECT,XSOL,
     &                NBVECT)
        END IF
      ELSE IF (TYPVEC.EQ.'C') THEN
        CALL MTDSC2(ZK24(ZI(LMAT+1)),'ABLO','L',JJABLO)
        CALL MTDSC2(ZK24(ZI(LMAT+1)),'ADIA','L',JJADIA)
        CALL MRMPVS(CUMUL,ZK24(ZI(LMAT+1)),ZI(JJADIA),ZI(IDHCOL),
     &              ZI(JJABLO),ZI(LMAT+02),ZI(LMAT+13),VECT,XSOL,NBVECT)
      ELSE
        CALL UTMESS('F','MRMULT','TYPE DE VECTEUR NON PREVU')
      END IF
      GO TO 110
C          -------------------------------------------------------------
   30 CONTINUE
C          MATRICE COMPLEXE
      GO TO (40,50) ZI(LMAT+4)
C             ----------------------------------------------------------
      CALL UTMESS('F','MRMULT','CARACT. DE MATRICE NON PREVUE')
      GO TO 110
C             ----------------------------------------------------------
   40 CONTINUE
C             MATRICE COMPLEXE SYMETRIQUE
      CALL MTDSC2(ZK24(ZI(LMAT+1)),'ABLO','L',JJABLO)
      CALL MTDSC2(ZK24(ZI(LMAT+1)),'ADIA','L',JJADIA)
      CALL MRMPVC(CUMUL,ZK24(ZI(LMAT+1)),ZI(JJADIA),ZI(IDHCOL),
     &            ZI(JJABLO),ZI(LMAT+02),ZI(LMAT+13),VECT,XSOL,NBVECT)
      GO TO 110
C             ----------------------------------------------------------
   50 CONTINUE
C             MATRICE COMPLEXE HERMITIENNE
      CALL MTDSC2(ZK24(ZI(LMAT+1)),'ABLO','L',JJABLO)
      CALL MTDSC2(ZK24(ZI(LMAT+1)),'ADIA','L',JJADIA)
      CALL MRMPVZ(CUMUL,ZK24(ZI(LMAT+1)),ZI(JJADIA),ZI(IDHCOL),
     &            ZI(JJABLO),ZI(LMAT+02),ZI(LMAT+13),VECT,XSOL,NBVECT)
      GO TO 110
C       ----------------------------------------------------------------
   60 CONTINUE
C       STOCKAGE MORSE
C       ZI(+2 ) : NEQ     : NOMBRE D'EQUATIONS
C       ----------------------------------------------------------------
      GO TO (70,80) ZI(LMAT+3)
C          -------------------------------------------------------------
      CALL UTMESS('F','MRMULT','COEFFICIENT DE TYPE NON PREVU')
      GO TO 110
C          -------------------------------------------------------------
   70 CONTINUE
C          MATRICE REELLE
      CALL MTDSC2(ZK24(ZI(LMAT+1)),'ADIA','L',JJADIA)
      CALL MRMMVR(CUMUL,ZK24(ZI(LMAT+1)),ZI(JJADIA),ZI(IDHCOL),
     &            ZI(LMAT+02),VECT,XSOL,NBVECT)
      GO TO 110
C          -------------------------------------------------------------
   80 CONTINUE
C          MATRICE COMPLEXE
      GO TO (90,100) ZI(LMAT+4)
C             ----------------------------------------------------------
      CALL UTMESS('F','MRMULT','CARACT. DE MATRICE NON PREVUE')
      GO TO 110
C             ----------------------------------------------------------
   90 CONTINUE
C             MATRICE COMPLEXE SYMETRIQUE
      CALL MTDSC2(ZK24(ZI(LMAT+1)),'ADIA','L',JJADIA)
      CALL MRMMVC(CUMUL,ZK24(ZI(LMAT+1)),ZI(JJADIA),ZI(IDHCOL),
     &            ZI(LMAT+02),VECT,XSOL,NBVECT)
      GO TO 110
C             ----------------------------------------------------------
  100 CONTINUE
C             MATRICE COMPLEXE HERMITIENNE
      CALL MTDSC2(ZK24(ZI(LMAT+1)),'ADIA','L',JJADIA)
      CALL MRMMVZ(CUMUL,ZK24(ZI(LMAT+1)),ZI(JJADIA),ZI(IDHCOL),
     &            ZI(LMAT+02),VECT,XSOL,NBVECT)
      GO TO 110
C             ----------------------------------------------------------

  110 CONTINUE
      CALL JEDEMA()
      END
