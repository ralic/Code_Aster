      SUBROUTINE  ENETOT(OPTION, IORD, LIGREL, CHGEOM, CHDEPL,
     +                   CHDEPM, CHSIG, CHSIGM, CHELEM)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 17/09/2002   AUTEUR CIBHHGB G.BERTRAND 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
      IMPLICIT NONE
C
C      ENETOT  -- CALCUL DU CHAMELEM CHELEM DONT LES COMPOSANTES
C                 SONT LA DENSITE D'ENERGIE TOTALE AUX POINTS 
C                 D'INTEGRATION OU AUX NOEUDS DES ELEMENTS.
C                 LES OPTIONS SONT :
C      'ETOT_ELGA'      POUR LE CALCUL AUX POINTS D'INTEGRATION
C      'ETOT_ELNO_ELGA' POUR LE CALCUL AUX NOEUDS DES ELEMENTS
C
C   ARGUMENT        E/S  TYPE         ROLE
C    OPTION         IN     K*      OPTION DE CALCUL :
C                                    'ETOT_ELGA'
C                                    'ETOT_ELNO_ELGA'
C    IORD           IN     I       NUMERO D'ORDRE DU CALCUL A
C                                  L'INSTANT COURANT T
C    LIGREL         IN     K*      LIGREL DU MODELE
C    CHGEOM         IN     K*      NOM DU CHAMP DES COORDONNEES
C                                  DES CONNECTIVITES
C    CHDEPL         IN     K*      CHAMP DES DEPLACEMENTS A L'INSTANT T
C    CHDEPM         IN     K*      CHAMP DES DEPLACEMENTS A L'INSTANT
C                                  T-DT (S'IL EXISTE)
C    CHSIG          IN     K*      CHAMP DES CONTRAINTES AUX
C                                  POINTS D'INTEGRATION A L'INSTANT T
C    CHSIGM         IN     K*      CHAMP DES CONTRAINTES AUX
C                                  POINTS D'INTEGRATION A L'INSTANT
C                                  T-DT (S'IL EXISTE)
C    CHELEM         OUT    K*      CHAMP DES DENSITES D'ENERGIE TOTALE
C
C.========================= DEBUT DES DECLARATIONS ====================
C -----  ARGUMENTS
           INTEGER       IORD
           CHARACTER*(*) OPTION, LIGREL, CHGEOM, CHDEPL, CHDEPM
           CHARACTER*(*) CHSIG, CHSIGM, CHELEM
C -----  VARIABLES LOCALES
           INTEGER      NBIN, IRET
           REAL*8       CONST(2), UN
           CHARACTER*1  TYPCST(2), TYPECH(2)
           CHARACTER*8  LPAIN(5), LPAOUT(1)
           CHARACTER*19 NOMCH(2)
           CHARACTER*24 LCHIN(5), LCHOUT(1)
C.========================= DEBUT DU CODE EXECUTABLE ==================
C
      UN = 1.0D0
C
C ---- CALCUL DES DENSITES D'ENERGIE TOTALE :
C      ------------------------------------
      LPAIN(1) = 'PGEOMER'
      LCHIN(1) =  CHGEOM
      LPAIN(2) = 'PDEPLR'
      LCHIN(2) =  CHDEPL
      LPAIN(3) = 'PCONTPR'
      LCHIN(3) =  CHSIG
      NBIN     =  3
      IF (IORD.GT.1) THEN
        LPAIN(4) = 'PDEPLM'
        LCHIN(4) =  CHDEPM
        LPAIN(5) = 'PCONTMR'
        LCHIN(5) =  CHSIGM
        NBIN     =  5
      ENDIF
      LPAOUT(1) = 'PENERDR'
      LCHOUT(1) = '&&ENETOT.CHAMELEM1' 
C
      CALL CALCUL('S',OPTION, LIGREL, NBIN, LCHIN, LPAIN, 1, LCHOUT,
     +            LPAOUT, 'V')
C
      TYPCST(1) = 'R'
      TYPCST(2) = 'R'
      TYPECH(1) = 'R'
      TYPECH(2) = 'R'
      CONST(1)  = UN
      CONST(2)  = UN
      NOMCH(1)  = '&&ENETOT.CHAMELEM1'
      NOMCH(2)  = '&&ENETOT.CHAMELEM2'
      CALL JEEXIN(CHELEM,IRET)
      IF (IRET.EQ.0) THEN
        CALL VTDEFS(CHELEM,'&&ENETOT.CHAMELEM1','G','R')
      ENDIF
      CALL JEEXIN('&&ENETOT.CHAMELEM2 .CELK',IRET)
      IF (IRET.EQ.0) THEN
        CALL VTDEFS('&&ENETOT.CHAMELEM2','&&ENETOT.CHAMELEM1','V','R')
      ENDIF
      CALL VTCMBL(2,TYPCST,CONST,TYPECH,NOMCH,'R','&&ENETOT.CHAMELEM2')
      NOMCH(1)  = CHELEM
      NOMCH(2)  = '&&ENETOT.CHAMELEM2'
      CALL VTCMBL(2,TYPCST,CONST,TYPECH,NOMCH,'R',CHELEM)
      CALL DETRSD('CHAMP_GD','&&ENETOT.CHAMELEM1')
C
C.============================ FIN DE LA ROUTINE ======================
      END
