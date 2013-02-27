      SUBROUTINE VPPFAC(LMASSE,MASGEN,VECT,NEQ,NBVECT,MXVECT,
     &                                MASMOD, FACPAR )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'

      INTEGER           LMASSE,            NEQ,NBVECT,MXVECT
      REAL*8                   MASGEN(*), VECT(NEQ,*)
      REAL*8                          MASMOD(MXVECT,*),FACPAR(MXVECT,*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 26/02/2013   AUTEUR BOITEAU O.BOITEAU 
C ======================================================================
C COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     CALCUL DES PARAMETRES MODAUX :
C            FACTEUR DE PARTICIPATION ET MASSE MODALE UNITAIRE
C     ------------------------------------------------------------------
C IN  LMASSE : IS : DESCRIPTEUR NORMALISE DE LA MATRICE DE MASSE
C     ------------------------------------------------------------------
C     PRESUME L'EXECUTION PREALABLE DE VPPGEN : CALCUL DES PARAMETRES
C     MODAUX.
C     ------------------------------------------------------------------


      INTEGER       IER,LDDL,LAUX1,LAUX2,IDDL,IA,IEQ,IVECT,MXDDL,NEQ1
      PARAMETER     ( MXDDL=6 )
      CHARACTER*8   NOMDDL(MXDDL)
      CHARACTER*14  NUME
      CHARACTER*19  MASSE
      CHARACTER*24  POSDDL, VECAU1, VECAU2
      REAL*8        DDOT,RMIN,R8MIEM,RMAX,R8MAEM,RAUX,RVAL
      INTEGER*4     NBI4
C     ------------------------------------------------------------------
      DATA NOMDDL / 'DX      ', 'DY      ', 'DZ      ' ,
     &              'DRX     ', 'DRY     ', 'DRZ     ' /

C     ------------------------------------------------------------------
      DATA  POSDDL/'&&VPPFAC.POSITION.DDL'/
      DATA  VECAU1/'&&VPPFAC.VECTEUR.AUX1'/
      DATA  VECAU2/'&&VPPFAC.VECTEUR.AUX2'/
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C     ------ RECUPERATION DES POSITIONS DES PARAMETRES DE LAGRANGE -----
C     ------------------------------------------------------------------

      CALL JEMARQ()
      MASSE = ZK24(ZI(LMASSE+1))
      CALL DISMOI('F','NOM_NUME_DDL',MASSE,'MATR_ASSE',IA,NUME,IER)

      CALL WKVECT(POSDDL,'V V I',NEQ*MXDDL,LDDL)
      CALL PTEDDL( 'NUME_DDL', NUME  , MXDDL , NOMDDL , NEQ , ZI(LDDL))

C     ------------------------------------------------------------------
C     ----------------- CREATION DE VECTEURS DE TRAVAIL ----------------
C     ------------------------------------------------------------------

      CALL WKVECT( VECAU1,'V V R', NEQ , LAUX1 )
      CALL WKVECT( VECAU2,'V V R', NEQ , LAUX2 )

C     ------------------------------------------------------------------
C     ----------- CALCUL DE  FREQ * MASSE * UNITAIRE_DIRECTION ---------
C     ------------------------------------------------------------------
      NBI4=NEQ
      NEQ1=NEQ-1
      RMIN=100.D0*R8MIEM()
      RMAX=0.01D0*R8MAEM()
      DO 100 IDDL = 1 , 3
         IA = (IDDL-1)*NEQ
         DO 110 IEQ = 0, NEQ1
             ZR(LAUX1+IEQ) = ZI(LDDL+IA+IEQ)
  110    CONTINUE
         CALL MRMULT('ZERO',LMASSE,ZR(LAUX1),ZR(LAUX2),1,.FALSE.)
         DO 200 IVECT = 1, NBVECT
            RVAL = DDOT(NBI4,VECT(1,IVECT),1,ZR(LAUX2),1)
            RAUX = MASGEN(IVECT)
            IF (ABS(RAUX).LT.RMIN) THEN
               MASMOD(IVECT,IDDL) = RMAX
               FACPAR(IVECT,IDDL) = RMAX
            ELSE
               RAUX=RVAL/RAUX
               MASMOD(IVECT,IDDL) = RVAL * RAUX
               FACPAR(IVECT,IDDL) = RAUX
            ENDIF
  200    CONTINUE
  100 CONTINUE

C     ------------------------------------------------------------------
C     ----------------- DESTRUCTION DES VECTEURS DE TRAVAIL ------------
C     ------------------------------------------------------------------

      CALL JEDETR( POSDDL )
      CALL JEDETR( VECAU1 )
      CALL JEDETR( VECAU2 )

      CALL JEDEMA()
      END
