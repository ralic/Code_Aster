      SUBROUTINE IBOPMD(NOM, DESCR, PTR, IER)
      IMPLICIT NONE
      CHARACTER*(*)     NOM(*),DESCR(*)
      INTEGER           PTR(*)
      INTEGER           IER
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF SUPERVIS  DATE 01/10/96   AUTEUR GIBHHCM C.MASSERET 
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
C     NOM(*)   IN : MOTS CLE DE LA COMMANDE
C     DESCR(*) IN : DESCRIPTEUR DU MOT CLE
C     PTR(*)   IN : POINTEURS (NB DE MOTS CLE DANS LE MCF)
C     IER      IN/OUT : +1 SI UN DOUBLON EST DETECTE
C     ------------------------------------------------------------------
C     RECHERCHE DE DOUBLE DECLARATION DE MOTS CLE SIMPLES D'UNE CMD 
C     LORS DE LA COMPILATION DES COMMANDES.
C     ------------------------------------------------------------------
C
      INTEGER II,JJ
C            
      II=2
C     --- BOUCLE SUR TOUS LES MOTS CLE DE LA COMMANDE ---
 101  CONTINUE
      IF (II .LE. PTR(1)) THEN
         IF (DESCR(II)(1:1) .NE. '*') THEN
C           --- MOT CLE SIMPLE ---
            JJ=II+1
 102        CONTINUE
            IF (JJ .LE. PTR(1)) THEN
               IF (DESCR(JJ)(1:1) .NE. '*') THEN
C                 --- PAS UN MCF TETS DE DOUBLON ---
                  IF (NOM(II) .EQ. NOM(JJ)) THEN
                     IER=IER+1
                     CALL UTDEBM('E','COMPILATION DES COMMANDES',
     +                 'DEFINITION DOUBLEE')
                     CALL UTIMPK('L','LE MOT CLE :',1,NOM(II))
                     CALL UTIMPK('L',
     +                   'EST REPETE DANS LA COMMANDE :',1,NOM)
                     CALL UTFINM()
                  ENDIF
                  JJ=JJ+1
               ELSE
C                 --- MCF ON DECALE ---
                  JJ=JJ+PTR(JJ)+1
               ENDIF
               GOTO 102
            ENDIF
            II=II+1
         ELSE
C           --- MCF ON DECALE ---
            II=II+PTR(II)+1
         ENDIF
         GOTO 101
      ENDIF  
C     
      END
