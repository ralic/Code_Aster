      SUBROUTINE SPLINE(X,Y,N,DY1,DYN,D2Y,IRET)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 26/05/98   AUTEUR H1BAXBG M.LAINET 
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
C-----------------------------------------------------------------------
C DESCRIPTION : INTERPOLATION SPLINE CUBIQUE
C -----------
C
C               ETANT DONNEE LA TABULATION DE LA FONCTION Y(I) = F(X(I))
C               EN N POINTS DE DISCRETISATION X(I) TELS QUE
C                               X(1) < X(2) < ... < X(N)
C               ETANT DONNEES LES VALEURS DE LA DERIVEE PREMIERE DY1 ET
C               DYN AUX PREMIER ET DERNIER POINTS X(1) ET X(N)
C               CETTE ROUTINE CALCULE LES VALEURS DE LA DERIVEE SECONDE
C               D2Y(I) DE LA FONCTION INTERPOLEE AUX N POINTS X(I)
C
C               SI DY1 ET/OU DYN DEPASSENT EN VALEUR ABSOLUE LE PLUS
C               GRAND NOMBRE ACCESSIBLE PAR LA MACHINE, LA SOLUTION
C               CALCULEE EST TELLE QUE LA DERIVEE SECONDE EST NULLE
C               AUX BORNES DE L'INTERVALLE
C
C IN     : X    : REAL*8 , VECTEUR DE DIMENSION N
C                 CONTIENT LES POINTS DE DISCRETISATION X(I)
C IN     : Y    : REAL*8 , VECTEUR DE DIMENSION N
C                 CONTIENT LES VALEURS DE LA FONCTION AUX POINTS X(I)
C IN     : N    : INTEGER , SCALAIRE
C                 NOMBRE DE POINTS DE DISCRETISATION
C IN     : DY1  : REAL*8 , SCALAIRE
C                 VALEUR DE LA DERIVEE PREMIERE DE LA FONCTION
C                 AU POINT X1
C IN     : DYN  : REAL*8 , SCALAIRE
C                 VALEUR DE LA DERIVEE PREMIERE DE LA FONCTION
C                 AU POINT XN
C OUT    : D2Y  : REAL*8 , VECTEUR DE DIMENSION N
C                 CONTIENT LES VALEURS DE LA DERIVEE SECONDE
C                 DE LA FONCTION INTERPOLEE AUX POINTS X(I)
C OUT    : IRET : INTEGER , SCALAIRE , CODE RETOUR
C                 IRET = 0  OK
C                 IRET = 1  VALEUR DE N INVALIDE
C
C-------------------   DECLARATION DES VARIABLES   ---------------------
C
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
      CHARACTER*32 JEXNOM, JEXNUM, JEXATR
C     ----- FIN   COMMUNS NORMALISES  JEVEUX  --------------------------
C
C ARGUMENTS
C ---------
      REAL*8     X(*), Y(*), DY1, DYN, D2Y(*)
      INTEGER    N, IRET
C
C VARIABLES LOCALES
C -----------------
      INTEGER    I, JW
      REAL*8     BIGNUM, P, QN, SIG, UN
C
      REAL*8     R8MIEM
C
C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------
C
      CALL JEMARQ()
      CALL WKVECT('&&SPLINE.WORK','V V R',N,JW)
C
      IRET = 0
      IF ( N.LT.3 ) THEN
         IRET = 1
         GO TO 9999
      ENDIF
C
      BIGNUM = 0.99D0 / R8MIEM()
C
      IF ( DBLE(ABS(DY1)).GT.BIGNUM ) THEN
         D2Y(1) = 0.0D0
         ZR(JW) = 0.0D0
      ELSE
         D2Y(1) = -0.5D0
         ZR(JW) = 3.0D0/(X(2)-X(1)) * ( (Y(2)-Y(1))/(X(2)-X(1)) - DY1 )
      ENDIF
C
      DO 10 I = 2, N-1
         SIG = (X(I)-X(I-1))/(X(I+1)-X(I-1))
         P = SIG * D2Y(I-1) + 2.0D0
         D2Y(I) = (SIG-1.0D0) / P
         ZR(JW+I-1) = ( 6.0D0 * ( (Y(I+1)-Y(I))/(X(I+1)-X(I))
     &                            - (Y(I)-Y(I-1))/(X(I)-X(I-1)) )
     &                        / (X(I+1)-X(I-1))
     &                        - SIG * ZR(JW+I-2) )
     &              / P
  10  CONTINUE
C
      IF ( DBLE(ABS(DYN)).GT.BIGNUM ) THEN
         QN = 0.0D0
         UN = 0.0D0
      ELSE
         QN = 0.5D0
         UN = 3.0D0/(X(N)-X(N-1))
     &      * ( DYN - (Y(N)-Y(N-1))/(X(N)-X(N-1)) ) 
      ENDIF
C
      D2Y(N) = (UN-QN*ZR(JW+N-2))/(QN*D2Y(N-1)+1.0D0)
      DO 20 I = N-1,1,-1
         D2Y(I) = D2Y(I) * D2Y(I+1) + ZR(JW+I-1)
  20  CONTINUE
C
9999  CONTINUE
      CALL JEDETC('V','&&SPLINE',1)
      CALL JEDEMA()
C
C --- FIN DE SPLINE.
      END
