      SUBROUTINE VPPFAC(LMASSE,MASGEN,VECT,NEQ,NBVECT,MXVECT,
     &                                MASMOD, FACPAR )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      INTEGER           LMASSE,            NEQ,NBVECT,MXVECT
      REAL*8                   MASGEN(*), VECT(NEQ,*)
      REAL*8                          MASMOD(MXVECT,*),FACPAR(MXVECT,*)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C
C
      INTEGER       IER, LDDL, LAUX1, LAUX2, IDDL, IA, IEQ, IVECT, MXDDL
      PARAMETER     ( MXDDL=6 )
      CHARACTER*8   NOMDDL(MXDDL)
      CHARACTER*14  NUME
      CHARACTER*19  MASSE
      CHARACTER*24  POSDDL, VECAU1, VECAU2
      REAL*8        RVAL, R8MAEM
C     ------------------------------------------------------------------
      DATA NOMDDL / 'DX      ', 'DY      ', 'DZ      ' ,
     &              'DRX     ', 'DRY     ', 'DRZ     ' /
C
C     ------------------------------------------------------------------
      DATA  POSDDL/'&&VPPFAC.POSITION.DDL'/
      DATA  VECAU1/'&&VPPFAC.VECTEUR.AUX1'/
      DATA  VECAU2/'&&VPPFAC.VECTEUR.AUX2'/
C     ------------------------------------------------------------------
C     ------------------------------------------------------------------
C     ------ RECUPERATION DES POSITIONS DES PARAMETRES DE LAGRANGE -----
C     ------------------------------------------------------------------
C
      CALL JEMARQ()
      MASSE = ZK24(ZI(LMASSE+1))
      CALL DISMOI('F','NOM_NUME_DDL',MASSE,'MATR_ASSE',IA,NUME,IER)
C
      CALL WKVECT(POSDDL,'V V I',NEQ*MXDDL,LDDL)
      CALL PTEDDL( 'NUME_DDL', NUME  , MXDDL , NOMDDL , NEQ , ZI(LDDL))
C
C     ------------------------------------------------------------------
C     ----------------- CREATION DE VECTEURS DE TRAVAIL ----------------
C     ------------------------------------------------------------------
C
      CALL WKVECT( VECAU1,'V V R', NEQ , LAUX1 )
      CALL WKVECT( VECAU2,'V V R', NEQ , LAUX2 )
C
C     ------------------------------------------------------------------
C     ----------- CALCUL DE  FREQ * MASSE * UNITAIRE_DIRECTION ---------
C     ------------------------------------------------------------------
C
      DO 100 IDDL = 1 , 3
         IA = (IDDL-1)*NEQ
         DO 110 IEQ = 0, NEQ-1
             ZR(LAUX1+IEQ) = ZI(LDDL+IA+IEQ)
  110    CONTINUE
         DO 200 IVECT = 1, NBVECT
            CALL MRMULT('ZERO',LMASSE,ZR(LAUX1),ZR(LAUX2),1,.FALSE.)
            RVAL = 0.0D0
            DO 210 IEQ = 1, NEQ
               RVAL  = RVAL  + VECT(IEQ,IVECT) * ZR(LAUX2+IEQ-1)
  210       CONTINUE
            IF ( MASGEN(IVECT) .EQ. 0.0D0 ) THEN
               MASMOD(IVECT,IDDL) = R8MAEM()
               FACPAR(IVECT,IDDL) = R8MAEM()
            ELSE
               MASMOD(IVECT,IDDL) = RVAL * RVAL / MASGEN(IVECT)
               FACPAR(IVECT,IDDL) = RVAL / MASGEN(IVECT)
            ENDIF
  200    CONTINUE
  100 CONTINUE
C
C     ------------------------------------------------------------------
C     ----------------- DESTRUCTION DES VECTEURS DE TRAVAIL ------------
C     ------------------------------------------------------------------
C
      CALL JEDETR( POSDDL )
      CALL JEDETR( VECAU1 )
      CALL JEDETR( VECAU2 )
C
      CALL JEDEMA()
      END
