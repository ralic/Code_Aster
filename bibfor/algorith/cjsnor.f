        SUBROUTINE CJSNOR( MATER, SIG ,X ,NOR,DEVNUL,TRAC)
C
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/03/2002   AUTEUR VABHHTS J.PELLET 
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
C   CALCUL  UNE ESTIMATION UN VECTEUR PARALLELE A DF/DQ
C           OU DF EST LE SEUIL DEVIATOIRE ET Q LE TENSEUR Q
C                     Q = S-X*I1
C   IN :
C       MATER : MATERIAU
C       SIG   : CONTRAINTES
C       X     : VARIABLES INTERNES CINEMATIQUES
C   OUT :
C       NOR   : ESTIMATION DE LA DITRECTION DE LA NORMALE
C               A LA SURFACE DEVIATOIRE DANS LE PLAN DEVIATOIRE
C               PERPENDICULAIRE A LA TRISECTRICE
C               LE VECTEUR NOR(1:NDT) N EST PAS NORME
C               SA NORME EST NOR(NDT+1)
C    DEVNUL   : VRAI SI DEVIATEUR DE Q NUL
C    TRAC     : VRAI SI I1  NUL
C
        IMPLICIT NONE


        REAL*8        MATER(14,2),SIG(6),X(6),NOR(7)
        LOGICAL       DEVNUL,TRAC
        REAL*8  ZERO,DEUX,SIX
        PARAMETER     ( ZERO   = 0.D0   )
        PARAMETER     ( DEUX   = 2.D0   )
        PARAMETER     ( SIX    = 6.D0   )

        REAL*8 G,PA,QINIT,Q(6),TQ(6),COEF,QII,COS3TQ,TRAV,TRAV2
        INTEGER I
        INTEGER       NDT, NDI
        COMMON /TDIM/   NDT, NDI




C-----------------------------------------------------------------------
C->     PROPRIETES CJS MATERIAU
C------------------------------
        G     = MATER(9,2)
        PA    = MATER(12,2)
        QINIT    = MATER(13,2)
C-----------------------------------------------------------------------
C->    Q QII ET COS3TQ
C-----------------------------------------------------------------------

        CALL CJSC3Q( SIG, X, PA,QINIT,Q,QII,COS3TQ,DEVNUL,TRAC)
C-----------------------------------------------------------------------
C->    TQ = DET(Q)*INV(Q)
C-----------------------------------------------------------------------
        CALL CJST(Q,TQ)
C

        COEF = SQRT(SIX)*G/QII
        TRAV2 = ZERO
        DO 10 I = 1 , NDT
         TRAV = (DEUX+G*COS3TQ)*Q(I)+COEF*TQ(I)
         TRAV2 = TRAV2+TRAV*TRAV
         NOR(I) = TRAV
   10   CONTINUE
        NOR(NDT+1) = SQRT(TRAV2)
        END
