      SUBROUTINE I3VSUP ( IER , NBSGT , EPSI )
      IMPLICIT NONE
      INTEGER             IER , NBSGT
      REAL*8                            EPSI
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 19/11/96   AUTEUR CIBHHGB G.BERTRAND 
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
C     PHASE DE VERIFICATION SUPPLEMENTAIRE OP0096 :
C           CONTROLE DE LA LONGUEUR DES LISTES COORDONEES DE A ET B
C     ------------------------------------------------------------------
C VAR IER    : I : NOMBRE D' ERREUR DETECTEE
C     ------------------------------------------------------------------
C
      INTEGER      I, NA, NB
      REAL*8       A(3), B(3)
C
C======================================================================
C
      DO 10, I = 1, NBSGT, 1
C
         CALL GETVR8('DEFI_SEGMENT','ORIGINE'   ,I,1,0,A,NA)
         NA = -NA
         IF ( NA .NE. 0  .AND.  NA .NE. 3 ) THEN
            IER = IER + 1
            CALL UTDEBM ('E','INTE_MAIL_3D',' ')
            CALL UTIMPI ('L','ERREUR NUMERO : ',1,IER)
            CALL UTIMPI ('L','DEFI_SEGMENT, OCCURENCE ',1,I)
            CALL UTIMPI ('L','<ORIGINE> ADMET POUR ARGUMENT '
     +                            //'UNE LISTE DE TAILLE : ',1,3)
            CALL UTFINM
         ENDIF
C
         CALL GETVR8('DEFI_SEGMENT','EXTREMITE',I,1,0,A,NB)
         NB = -NB
         IF ( NB .NE. 0  .AND.  NB .NE. 3) THEN
            IER = IER + 1
            CALL UTDEBM ('E','INTE_MAIL_3D',' ')
            CALL UTIMPI ('L','ERREUR NUMERO : ',1,IER)
            CALL UTIMPI ('L','DEFI_SEGMENT, OCCURENCE ',1,I)
            CALL UTIMPI ('L','<EXTREMITE> ADMET POUR ARGUMENT'
     +                            //' UNE LISTE DE TAILLE : ',1,3)
            CALL UTFINM
         ENDIF
 10   CONTINUE
C
      END
