      SUBROUTINE JM1DN3 ( NB2 , XR         , EPAIS , KSI3S2 , INTSN ,
     &                    JM1 , J1DN3 )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 10/11/98   AUTEUR D6BHHMA M.ALMIKDAD 
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
C
C
C ......................................................................
C     FONCTION :  CALCUL DU PRODUIT
C
C                 J1DN3 ( 9 , 3 * NB2 ) =
C
C                 JTILD ( 9 , 9 ) * DNDQSI3  ( 9 , 3 * NB2 ) 
C
C                 POUR LA PARTIE NON CLASSIQUE D LA RIGIDITE GEOMETRIQUE
C
C                 AUX POINTS D INTEGRATION NORMALE
C
C ......................................................................
C
C
C
      IMPLICIT NONE
C
C
      INTEGER      JN
C
      INTEGER       NB2 
C
      INTEGER INTSN
      INTEGER INTSN2
C
      INTEGER           I3 , I4 , I5
      INTEGER L1 , L2 , L3
C
      REAL * 8 VI ( 3 )
C
      REAL * 8 XR ( * )
C
      REAL * 8 EPAIS
C
      REAL * 8 KSI3S2
C
      REAL * 8 JM1 ( 3 )
      REAL * 8 J1DN3 ( 9 , 27 )
C
      REAL * 8 TMPI ( 3 )
C
C
CDEB
C
C
C---- INITIALISATION
C
      CALL R8INIR ( 9 * 27 , 0.D0 , J1DN3 , 1 )
C
C---- LES ADRESSES DES FONCTIONS DE FORME ET DE SES DERIVEES
C     SELON IND ( VOIR ROUTINE BTDFN )
C
C
C
C
C------- NOEUDS DE LAGRANGE POUR LA ROTATION
C
C              N ( 2 )              L1  POUR  I3
C            D N ( 2 ) D QSI 1      L2  POUR  I4
C            D N ( 2 ) D QSI 2      L3  POUR  I5
C
C        VOIR ROUTINE BTDFN
C
         L1 = 459
         L2 = 540
         L3 = 621
C
C------- DECALAGE DE 9 NOEUDS DE LAGRANGE
C
         INTSN2 = 9 * ( INTSN - 1 )
C
         I3 = L1 + INTSN2
         I4 = L2 + INTSN2
         I5 = L3 + INTSN2
C
C
C        MODIFICATION VERIFIEE  DANS INIT080  NB2 AU LIEU DE NB1 
C
      DO 100 JN = 1 , NB2 
C
C------- REMPLISSAGE DE VI ( 3 )
C
         VI ( 1 ) = EPAIS * KSI3S2 * XR ( I4 + JN )
         VI ( 2 ) = EPAIS * KSI3S2 * XR ( I5 + JN )
         VI ( 3 ) = EPAIS * 0.5D0 *  XR ( I3 + JN )
C
C------- PRODUIT  JM1 ( 3 , 3 ) * VI ( 3 )
C
         CALL PROMAT ( JM1  , 3 , 3 , 3 ,
     &                 VI   , 3 , 3 , 1 , 
     &                 TMPI )
C
C------- REMPLISSAGE DE J1DN3 ( 9 , NB2 * 3 ) 
C
C        JTILD-1 ( 9 , 9 ) * DNDQSI ( 9 , 3 * NB2 ) 
C
C---------- BLOC U
C
         J1DN3 ( 1 ,(JN-1)*3 + 1 )       = TMPI ( 1 )
         J1DN3 ( 2 ,(JN-1)*3 + 1 )       = TMPI ( 2 )
         J1DN3 ( 3 ,(JN-1)*3 + 1 )       = TMPI ( 3 )
C
C---------- BLOC V
C
            J1DN3 ( 4 ,(JN-1)*3 + 2 )    = TMPI ( 1 )
            J1DN3 ( 5 ,(JN-1)*3 + 2 )    = TMPI ( 2 )
            J1DN3 ( 6 ,(JN-1)*3 + 2 )    = TMPI ( 3 )
C
C---------- BLOC W
C
               J1DN3 ( 7 ,(JN-1)*3 + 3 ) = TMPI ( 1 )
               J1DN3 ( 8 ,(JN-1)*3 + 3 ) = TMPI ( 2 )
               J1DN3 ( 9 ,(JN-1)*3 + 3 ) = TMPI ( 3 )
C
 100  CONTINUE
C
C
CFIN
C
      END
