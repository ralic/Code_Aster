        SUBROUTINE CJSDTD( MOD, Q, DTDDQ )
        IMPLICIT REAL*8 (A-H,O-Z)
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
C     ------------------------------------------------------------------
C     CALCUL DE LA DERIVEE DU TENSEUR TD PAR RAPPORT A Q
C     TD = DEVIATEUR( DET(Q) * INVERSE(Q) )
C     ------------------------------------------------------------------
C     IN   MOD      :  MODELISATION
C          Q        :  TENSEUR (6 COMPOSANTES)
C     OUT  DTDDQ    :  TENSEUR RESULTAT (6 COMPOSANTES)
C          DTDDQ(I,J) = D TD(I) / D Q(J)
C     ------------------------------------------------------------------

        INTEGER       NDT, NDI
        REAL*8        Q(6), DTDDQ(6,6)
        REAL*8        ZERO, DEUX, TROIS, QUATRE, RC2

        PARAMETER     ( ZERO   = 0.D0   )
        PARAMETER     ( DEUX   = 2.D0   )
        PARAMETER     ( TROIS  = 3.D0   )
        PARAMETER     ( QUATRE = 4.D0   )

        CHARACTER*8   MOD

        COMMON /TDIM/   NDT, NDI

C-----------------------------------------------------------------------


C------------------------------
C ATTENTION A LA PRISE EN COMPTE DE COEFFICIENTS 1/DEUX ET RC2 EN
C           FACTEUR DE CERTAINS TERMES
C

        RC2 = SQRT(2.D0)



C - MODELISATION 3D:

        IF( MOD(1:2) .EQ. '3D' ) THEN

        DTDDQ(1,1) = ( -       Q(2) -      Q(3) ) / TROIS
        DTDDQ(2,1) = ( -       Q(2) + DEUX*Q(3) ) / TROIS
        DTDDQ(3,1) = (   DEUX*Q(2)  -      Q(3) ) / TROIS
        DTDDQ(4,1) = ZERO
        DTDDQ(5,1) = ZERO
        DTDDQ(6,1) = - Q(6)


        DTDDQ(1,2) = ( -       Q(1) + DEUX*Q(3) ) / TROIS
        DTDDQ(2,2) = ( -       Q(1) -      Q(3) ) / TROIS
        DTDDQ(3,2) = (   DEUX*Q(1)  -      Q(3) ) / TROIS
        DTDDQ(4,2) = ZERO
        DTDDQ(5,2) = - Q(5)
        DTDDQ(6,2) = ZERO


        DTDDQ(1,3) = ( -       Q(1) + DEUX*Q(2) ) / TROIS
        DTDDQ(2,3) = (   DEUX*Q(1)  -      Q(2) ) / TROIS
        DTDDQ(3,3) = ( -       Q(1) -      Q(2) ) / TROIS
        DTDDQ(4,3) = - Q(4)
        DTDDQ(5,3) = ZERO
        DTDDQ(6,3) = ZERO


        DTDDQ(1,4) =   DEUX   * Q(4) / TROIS / DEUX
        DTDDQ(2,4) =   DEUX   * Q(4) / TROIS / DEUX
        DTDDQ(3,4) = - QUATRE * Q(4) / TROIS / DEUX
        DTDDQ(4,4) = - Q(3)
        DTDDQ(5,4) =   Q(6) / RC2
        DTDDQ(6,4) =   Q(5) / RC2


        DTDDQ(1,5) =   DEUX   * Q(5) / TROIS / DEUX
        DTDDQ(2,5) = - QUATRE * Q(5) / TROIS / DEUX
        DTDDQ(3,5) =   DEUX   * Q(5) / TROIS / DEUX
        DTDDQ(4,5) =   Q(6) / RC2
        DTDDQ(5,5) = - Q(2)
        DTDDQ(6,5) =   Q(5) / RC2


        DTDDQ(1,6) = - QUATRE * Q(6) / TROIS / DEUX
        DTDDQ(2,6) =   DEUX   * Q(6) / TROIS / DEUX
        DTDDQ(3,6) =   DEUX   * Q(6) / TROIS / DEUX
        DTDDQ(4,6) =   Q(5) / RC2
        DTDDQ(5,6) =   Q(4) / RC2
        DTDDQ(6,6) = - Q(1)



C - MODELISATION 2D : D_PLAN ET AXIS

        ELSE IF ( MOD(1:6) .EQ. 'D_PLAN' .OR.
     &            MOD(1:4) .EQ. 'AXIS'        ) THEN


        DTDDQ(1,1) = ( -       Q(2) -      Q(3) ) / TROIS
        DTDDQ(2,1) = ( -       Q(2) + DEUX*Q(3) ) / TROIS
        DTDDQ(3,1) = (   DEUX*Q(2)  -      Q(3) ) / TROIS
        DTDDQ(4,1) = ZERO


        DTDDQ(1,2) = ( -       Q(1) + DEUX*Q(3) ) / TROIS
        DTDDQ(2,2) = ( -       Q(1) -      Q(3) ) / TROIS
        DTDDQ(3,2) = (   DEUX*Q(1)  -      Q(3) ) / TROIS
        DTDDQ(4,2) = ZERO


        DTDDQ(1,3) = ( -       Q(1) + DEUX*Q(2) ) / TROIS
        DTDDQ(2,3) = (   DEUX*Q(1)  -      Q(2) ) / TROIS
        DTDDQ(3,3) = ( -       Q(1) -      Q(2) ) / TROIS
        DTDDQ(4,3) = - Q(4)


        DTDDQ(1,4) =   DEUX   * Q(4) / TROIS / DEUX
        DTDDQ(2,4) =   DEUX   * Q(4) / TROIS / DEUX
        DTDDQ(3,4) = - QUATRE * Q(4) / TROIS / DEUX
        DTDDQ(4,4) = - Q(3)





        ELSE IF ( MOD(1:6) .EQ. 'C_PLAN' .OR.
     &            MOD(1:2) .EQ. '1D' ) THEN
             CALL UTMESS('F','CJS','LES MODELISATIONS AUTORISEES'//
     &                       ' SONT 3D ET D_PLAN ET AXIS')
        ENDIF

        END
