      FUNCTION PRJSOM (NBMAT, MATER, INVARE, INVARS, B, SIIE, TYPE)
C
      IMPLICIT     NONE
      LOGICAL      PRJSOM
      INTEGER      NBMAT
      REAL*8       INVARE, INVARS, MATER(NBMAT,2), B, SIIE
      CHARACTER*9  TYPE
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 17/06/2003   AUTEUR CIBHHBC R.FERNANDES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C ======================================================================
C --- BUT : TESTER S'IL DOIT Y AVOIR PROJECTION ------------------------
C --------- AU SOMMET DU DOMAINE DE REVERSIBILITE ----------------------
C ======================================================================
C IN  : NBMAT  : NOMBRE DE PARAMETRES MATERIAU -------------------------
C --- : MATER  : PARAMETRES MATERIAU -----------------------------------
C --- : INVARE : PREMIER INVARIANT DU TENSEUR DE CONTRAINTES ELASTIQUES-
C --- : INVARS : PREMIER INVARIANT DU TENSEUR DE CONTRAINTES AU SOMMET -
C --- : B      : PARAMETRE CONTROLANT LE COMPORTEMENT VOLUMIQUE --------
C ------------ : DU MATERIAU -------------------------------------------
C --- : SIIE   : NORME DU DEVIATEUR ------------------------------------
C --- : TYPE   : 'SUPERIEUR' OU 'INFERIEUR' POUR LE CALCUL DE COSPHI ---
C OUT : PRJSOM : TRUE SI LA PROJECTION AU SOMMET EST RETENUE -----------
C --- :        : FALSE SI PROJECTION AU SOMMET DU DOMAINE --------------
C ======================================================================
      REAL*8        MUN, ZERO, DEUX, TROIS
      REAL*8        MU, K, GAMCJS, COSTYP, COSPHI, TEST1, TEST2
C ======================================================================
C --- INITIALISATION DE PARAMETRES -------------------------------------
C ======================================================================
      PARAMETER       ( MUN    = -1.0D0  )
      PARAMETER       ( ZERO   =  0.0D0  )
      PARAMETER       ( DEUX   =  2.0D0  )
      PARAMETER       ( TROIS  =  3.0D0  )
C ======================================================================
C --- INITIALISATION ---------------------------------------------------
C ======================================================================
      MU     = MATER ( 4,1)
      K      = MATER ( 5,1)
      GAMCJS = MATER (12,2)
C ======================================================================
C --- CALCUL A PRIORI DE LA PROJECTION AU SOMMET -----------------------
C ======================================================================
      TEST1  = INVARE-INVARS
      IF (TYPE.EQ.'SUPERIEUR') THEN
         IF (B.LT.ZERO) THEN
            COSTYP = COSPHI(B, GAMCJS, 'MAX')
         ELSE
            COSTYP = COSPHI(B, GAMCJS, 'MIN')
         ENDIF
         TEST2  = MUN*TROIS*K*B*SIIE*COSTYP/(DEUX*MU)
         IF (TEST1.LT.TEST2) THEN
            PRJSOM = .FALSE.
         ELSE
            PRJSOM = .TRUE.
         ENDIF
      ELSE IF (TYPE.EQ.'INFERIEUR') THEN
         IF (B.LT.ZERO) THEN
            COSTYP = COSPHI(B, GAMCJS, 'MIN')
         ELSE
            COSTYP = COSPHI(B, GAMCJS, 'MAX')
         ENDIF
         TEST2  = MUN*TROIS*K*B*SIIE*COSTYP/(DEUX*MU)
         IF (TEST1.GT.TEST2) THEN
            PRJSOM = .FALSE.
         ELSE
            PRJSOM = .TRUE.
         ENDIF
      ELSE
         CALL UTMESS('F','PRJSOM','TYPE INCONNU')
      ENDIF
C ======================================================================
      END
