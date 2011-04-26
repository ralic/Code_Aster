        SUBROUTINE CJSSMD ( MATER, SIG ,VIN, SEUILD )
        IMPLICIT NONE
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C       ----------------------------------------------------------------
C       CJS        :  SEUIL DU MECANISME DEVIATOIRE
C                     FD = QII HTQ + R I1
C       ----------------------------------------------------------------
C       IN  SIG    :  CONTRAINTE
C       IN  VIN    :  VARIABLES INTERNES = ( Q, R, X )
C       OUT SEUILD :  SEUIL  ELASTICITE DU MECANISME DEVIATOIRE
C       ----------------------------------------------------------------
        INTEGER       NDT, NDI
        REAL*8        MATER(14,2), SIG(6), VIN(*), SEUILD, RCOS3T, HTQ
        REAL*8        R, X(6), I1, DEV(6), Q(6), QII, GAMMA, PA, QINIT
        REAL*8        PREF, EPSSIG, TRACE, COS3T, HLODE
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
C ======================================================================
C --- DEFINITION DE PARAMETRE ------------------------------------------
C ======================================================================
        PARAMETER     ( EPSSIG = 1.D-8 )
C ======================================================================
        CALL JEMARQ ()
C ======================================================================
C --- VARIABLES INTERNES -----------------------------------------------
C ======================================================================
        R = VIN(2)
        CALL LCEQVN ( NDT , VIN(3), X )
C ======================================================================
C --- CARACTERISTIQUES MATERIAU ----------------------------------------
C ======================================================================
        GAMMA = MATER( 9,2)
        PA    = MATER(12,2)
        QINIT = MATER(13,2)
C ======================================================================
C --- PRESSION DE REFERENCE --------------------------------------------
C ======================================================================
         I1 = TRACE(NDI,SIG)
         IF( (I1+QINIT).EQ.0.0D0 ) THEN
            I1   = -QINIT + 1.D-12*PA
            PREF = ABS(PA)
         ELSE
            PREF = ABS(I1+QINIT)
         ENDIF
C ======================================================================
C --- CALCUL DU TENSEUR Q ----------------------------------------------
C ======================================================================
        CALL     LCDEVI(SIG,DEV)
        CALL     CJSQIJ(DEV,I1,X,Q)
        CALL     LCPRSC(Q,Q,QII)
        QII    = SQRT(QII)
C ======================================================================
C --- CALCUL DE HTQ ----------------------------------------------------
C ======================================================================
        RCOS3T = COS3T(Q, PREF, EPSSIG)
        HTQ    = HLODE(GAMMA,RCOS3T)
C ======================================================================
C --- CALCUL DU SEUIL DU MECANISME DEVIATOIRE --------------------------
C ======================================================================
        SEUILD = QII*HTQ + R*(I1+QINIT)
C ======================================================================
        CALL JEDEMA ()
C ======================================================================
        END
