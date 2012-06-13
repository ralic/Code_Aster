      SUBROUTINE TBTRIK ( NDIM, TABCHA, TABINT )
      IMPLICIT   NONE
      INCLUDE 'jeveux.h'
      INTEGER             NDIM, TABINT(*)
      CHARACTER*(*)       TABCHA(*)      
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
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
C     FONCTION:
C     RANGEMENT DES CHAINES DE CARACTERES DU TABLEAU "TABCHA"
C     DANS L'ORDRE CROISSANT.
C-----------------------------------------------------------------------
C IN  NDIM   : I  : DIMENSION DU TABNLEAU TABCHA.
C IN  TABCHA : K  : TABLEAU CONTENANT DES CHAINES DE CARACTERES A RANGER
C                   DANS L'ORDRE CROISSANT.
C OUT TABINT : I  : TABLEAU D'ENTIERS CONTENANT LES POSITIONS 
C                   DANS LE TABLEAU  TABCHA DANS L'ORDRE CROISSANT.  
C-----------------------------------------------------------------------
C ----------------------------------------------------------------------
      INTEGER         IMIN, J0, J1, I, J, LMASQ
C-----------------------------------------------------------------------
      CALL JEMARQ()
C
C     --- ON DEMASQUE TOUS LES ELEMENTS DU TABLAEU A TRIER ---
C
      CALL WKVECT ( '&&TBTRIK.MASQ', 'V V I', NDIM, LMASQ )
C
      J0 = 1
      DO 10 I = 1 , NDIM
C        --- RECHERCHE DU PREMIER ELEMENT NON MASQUE ---      
         DO 20 J = J0 , NDIM
            IF ( ZI(LMASQ+J-1) .EQ. 0 ) THEN
               J1 = J 
               GOTO 22
            ENDIF
  20     CONTINUE
C         
  22     CONTINUE
C
C        -- RECHERCHE DU PLUS PETIT ELEMENT NON MASQUE --
         J0   = J1
         IMIN = J1            
         DO 30 J =  J0+1 , NDIM
           IF ( ZI(LMASQ+J-1).EQ.0 .AND. TABCHA(J).LT.TABCHA(IMIN) )
     +        IMIN = J
  30     CONTINUE
C  
C        -- RANGEMENT DU IEME ELEMENT ET MISE A JOUR DU MASQUE --
         TABINT(I) = IMIN   
         ZI(LMASQ+IMIN-1) = 1      
C
  10  CONTINUE
C
      CALL JEDETR ( '&&TBTRIK.MASQ' )
C
      CALL JEDEMA()
      END
