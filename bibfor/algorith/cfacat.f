       SUBROUTINE CFACAT(NDIM, INDIC, NBLIAC, AJLIAI, SPLIAI, LLF, LLF1,
     +  LLF2, INDFAC, NESMAX,DEFICO, RESOCO, LMAT, CINE, NBLIAI, XJVMAX)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2003   AUTEUR CIBHHPD D.NUNEZ 
C ======================================================================
C COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ======================================================================
       IMPLICIT      NONE
       INTEGER       NDIM, INDIC, NBLIAC, AJLIAI, SPLIAI, LLF, LLF1
       INTEGER       LLF2, INDFAC, NESMAX, LMAT, NBLIAI
       REAL*8        XJVMAX
       CHARACTER*24  DEFICO,RESOCO, CINE
C ======================================================================
C ----------------------------------------------------------------------
C --- BUT : ROUTINE MERE POUR LE CALCUL DE ACM1AT ----------------------
C ------- : PERMETTANT DE POINTER SUR LES ROUTINES DE CALCUL ADAPTE ----
C ------- : SELON QUE L'ON AJOUTE OU RETIRE DES LIAISONS ---------------
C ----------------------------------------------------------------------
C ======================================================================
       CALL JEMARQ ()
C ======================================================================
C --- TEST SUR L'ETAT DE LA MATRICE A CALCULER -------------------------
C ======================================================================
       IF (INDIC.NE.-1) THEN
          CALL CFACA1(NDIM, NBLIAC, AJLIAI, LLF, LLF1, LLF2, NESMAX,
     +                               DEFICO,RESOCO, LMAT, CINE, NBLIAI)
       ENDIF
       CALL CFACA2(NDIM, NBLIAC, SPLIAI, LLF, LLF1, LLF2, INDFAC,
     +                             NESMAX, RESOCO, LMAT, NBLIAI, XJVMAX)
C ======================================================================
       INDIC = 1
       CALL JEDEMA ()
C ======================================================================
       END
