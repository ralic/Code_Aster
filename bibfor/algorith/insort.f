      SUBROUTINE INSORT ( EPSRX,SIGRX,STRNX,S1X,IFISU,IPLA,EQSTR,
     1  EPSEQ,JFISU,TANG,EPST,EPSC,EDC,EDT,RTM,DEFR,SIGMRX,IDIR )
        IMPLICIT REAL*8 (A-H,O-Z)
C       -----------------------------------------------------------
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
C       -----------------------------------------------------------
C       NADAI_B :  BETON FISSURE  LOI 1-D PLASTIQUE-ENDOMMAGEABLE
C
C ENTREES : EPSRX : DEFORMATION TOTALE A T
C           STRNX : INCREMENT DE DEFORMATION
C           SIGRX : CONTRAINTE  A T
C           SIGMRX: PREDICTEUR ELASTIQUE SIGRX+DSIG
C
C  E/S : IFISU,IPLA,EQSTR,EPSEQ,JFISU,TANG,EPST,EPSC,EDC,EDT,RTM,DEFR
C        CE SONT TOUTES DES VARIABLES INTERNES, VOIR LEUR
C        SIGNIFICATION DANS LA PROCEDURE  INSPIF
C
C SORTIES :  S1X :  CONTRAINTE UNIAXIALE A T+DT
C       -----------------------------------------------------------
      INTEGER   IFISU,IPLA,JFISU,IDIR
      REAL*8    EPSRX,SIGRX,STRNX,S1X,EQSTR,EPSEQ,TANG,EPST
      REAL*8    EPSC,EDC,EDT,RTM,DEFR,SIGMRX
      CHARACTER*8 PASDT,PASDC
      COMMON/CARMA/EX,RB,ALPHA,EMAX,PENT,ICU
C------------------------------------------------------------------
      STRNRX = EPSRX + STRNX
      A = ABS(STRNRX)
C=======================================================================
C      SI LE POINT A DEPASSE LA LIMITE EMAX EN COMPRESSION
C      ===> LE POINT NE REPREND PLUS DE CONTRAINTES ET LA CONTRAINTE
C           TOTALE EST NULLE QUELQUE SOIT L ETAT DE DEFORMATION
C
C
      IF( IPLA .EQ. 2 ) THEN
       EPSEQ = A
       S1X = 0.D0
       TANG = 0.D0
       GOTO 9999
      ENDIF
C=======================================================================
      IF( IFISU .EQ. 1 ) THEN
C=======================================================================
C              ***************************
C              *   POINT DEJA FISSURE    *
C              ***************************
C            TEST DE FERMETURE DE LA FISSURE
C
      CALL INSDT (EPSRX,STRNX,STRNRX,SIGMRX,TANG,S1X,
     1 EDT,EDC,EPST,DEFR,IFISU,IPLA,EQSTR,RTM,EPSC,IREFE,IDIR)
      IF(IFISU.EQ.0.OR.IREFE.EQ.1) GOTO 9999
C
C        TENSION-SOFTENING
C
       CALL INSFI1 ( S1X , STRNX , TANG , RTM , PENT )
       GOTO 9999
      ENDIF
C======================================================================
      IF( IFISU .EQ. 0 .AND. JFISU .EQ. 1 ) THEN

C       COMPORTEMENT DU BETON FISSURE (FISSURE FERMEE)
C
      CALL INSFFE (EPSRX,STRNX,STRNRX,SIGMRX,SIGRX,TANG,S1X,
     1     EDT,EDC,EPST,EPSC,DEFR,RTM,IFISU,JFISU,IPLA,EQSTR,IDIR)
       GOTO 9999
      ENDIF
C=======================================================================
C
C       BETON INTEGRE (NON FISSURE)
C
C-----------------------------------------------------------------------
      IF( ABS(STRNRX) .LT. EPSEQ ) THEN
C
C       ************ DOMAINE DES DECHARGES ************
C                    (DECHARGE ENDOMMAGEE)
C   ZONES DE COMPRESSION OU ZONES DE TRACTION APRES
C   DECHARGE EN COMPRESSION
C
      CALL INSDC (S1X,EDC,EPST,EDT,RTM,EPSC,DEFR,SIGRX,STRNX,STRNRX,
     1             EPSRX,IFISU,JFISU,SIGMRX,IPLA,TANG,IDIR)
      GOTO 9999
      ENDIF
C=======================================================================

      IF( ABS(STRNRX) .GE. EPSEQ .AND. SIGMRX .LT. 0.D0 ) THEN
C
C       ************ ECROUISSAGE EN COMPRESSION ************
C
      CALL INSCU ( A , SEQ , EPEQ , IPLA , TANG )
      EQSTR = SEQ
      EPSEQ = EPEQ
      S1X = -SEQ
      GOTO 9999
      ENDIF
C=======================================================================
      IF(ABS(STRNRX).GE.EPSEQ.AND.SIGMRX.GE.0.D0.AND.EPSRX.GE.0.D0)
     1  THEN
C
C       ***********  DOMAINE DES TRACTIONS  ***********
C       ********** POINT INITIALLEMENT TENDU **********
C
       IF( SIGMRX .GE. RTM ) THEN
C
C              *************************************
C              *    CE POINT VIENT DE FISSURER     *
C              *************************************
C
        CALL INSFI2 ( S1X , SIGRX , STRNX , RTM , PENT , TANG )
        IFISU = 1
        JFISU = 1
C
C       REMARQUE : A L ITERATION SUIVANTE CHEMIN ---> BETFIS
       ELSE
C              ****************************************
C              *  POINT ELASTIQUE INTEGRE (TRACTION)  *
C              ****************************************
C
       S1X = SIGMRX
       ENDIF
      GOTO 9999
      ENDIF
C=======================================================================
      IF( ABS(STRNRX).GE.EPSEQ.AND.SIGMRX.GE.0.D0.AND.EPSRX.LT.0.D0 )
     1 THEN
C
C       *********** DOMAINE DES TRACTIONS  ***********
C       ******** POINT INITIALLEMENT COMPRIME **********
C
      CALL INSDC (S1X,EDC,EPST,EDT,RTM,EPSC,DEFR,SIGRX,STRNX,STRNRX,
     1             EPSRX,IFISU,JFISU,SIGMRX,IPLA,TANG,IDIR)
      GOTO 9999
      ENDIF
C
 9999 CONTINUE
      END
