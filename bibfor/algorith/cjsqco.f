         SUBROUTINE CJSQCO( GAMMA, SIG, X, PREF, EPSSIG, I1,
     >                      S, SII, SIIREL, COS3TS, HTS, DETS,
     >                      Q, QII, QIIREL, COS3TQ, HTQ, DETQ )
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
C     ------------------------------------------------------------------
C     CALCUL DE GRANDEURS UTILES
C     ------------------------------------------------------------------
C     IN
C          GAMMA    :  PARAMETRE MATERIAU
C          SIG      :  CONTRAINTES
C          X        :  VARIABLES ECROUI CINE
C          PREF     :  PRESS REF POUR NORMALISATION
C          EPSSIG   :  EPSILON POUR NULLITE DEVIATEUR
C          I1       :  TRACE DU TENSEUR DES CONTRAINTES
C     OUT
C          S        : DEV(SIG)
C          SII      : SQRT(S:S)
C          SIIREL   : SII/PREF
C          COS3TS   : LODE(SIG)
C          HTS      : FONCTION H(TETHA_S)
C          DETS     : DETERMINANT DE S
C
C          Q        : Q(SIG-X)
C          QII      : SQRT(Q:Q)
C          QIIREL   : QII/PREF
C          COS3TQ
C          HTQ      : FONCTION H(TETHA_Q)
C          DETQ     : DETERMINANT DE Q
C ======================================================================
        INTEGER       NDT, NDI
        REAL*8        GAMMA, PREF, EPSSIG
        REAL*8        SIG(6), X(6), S(6), Q(6)
        REAL*8        I1, SII, SIIREL, COS3TS, QII, QIIREL, COS3TQ
        REAL*8        HTS, DETS, HTQ, DETQ, COS3T, HLODE
C ======================================================================
        COMMON /TDIM/   NDT, NDI
C ======================================================================
C --- CALCUL DES ANGLES DE LODE POUR S ET Q ----------------------------
C ======================================================================
        CALL     LCDEVI(SIG,S)
        CALL     LCDETE(S,DETS)
        CALL     LCPRSC(S,S,SII)
        SII    = SQRT(SII)
        SIIREL = SII / PREF
        COS3TS = COS3T(S, PREF, EPSSIG)

        CALL     CJSQIJ(S, I1, X, Q)
        CALL     LCDETE(Q,DETQ)
        CALL     LCPRSC(Q,Q,QII)
        QII    = SQRT(QII)
        QIIREL = QII / PREF
        COS3TQ = COS3T(Q, PREF, EPSSIG)
C ======================================================================
C --- CALCUL DE HT POUR LES ANGLES DE LODE DE S ET Q -------------------
C ======================================================================
        HTS    = HLODE(GAMMA,COS3TS)
        HTQ    = HLODE(GAMMA,COS3TQ)
C ======================================================================
        END
